use std::char::CharTryFromError;
use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter};
use std::fs;
use std::num::{ParseFloatError, ParseIntError};

macro_rules! skip_whitespace_match {
    ($self: ident, $($arms:tt)*) => {
        loop {
            match $self.current() {
                Some(b' ' | b'\n' | b'\r' | b'\t') => $self.position += 1,
                $($arms)*
            }
        }
    };
}

macro_rules! skip_whitespace_value_match {
    ($self: ident, $($arms:tt)*) => {
        loop {
            break match $self.current() {
                Some(b' ' | b'\n' | b'\r' | b'\t') => {
                    $self.position += 1;
                    continue;
                },
                Some(b'n') => $self.read_null(),
                Some(b't') => $self.read_true(),
                Some(b'f') => $self.read_false(),
                b @ Some(b'-' | b'0'..=b'9') => $self.read_number(b.cloned()),
                Some(b'"') => $self.read_string().map(Value::String),
                Some(b'[') => $self.read_array(),
                Some(b'{') => $self.read_object(),
                $($arms)*
            };
        }
    };
}

macro_rules! skip_digits_match {
    ($self: ident, $($arms:tt)*) => {
        loop {
            match $self.current() {
                Some(b'0'..=b'9') => $self.position += 1,
                $($arms)*
            }
        }
    };
}

macro_rules! skip_digit_expect_first {
    ($self: ident, $($arms:tt)*) => {
        match $self.current() {
            Some(b'0'..=b'9') => $self.position += 1,
            $($arms)*
        }
        while $self.current().is_some_and(u8::is_ascii_digit) {
            $self.position += 1;
        }
    }
}

pub fn verify_json() {
    for f in fs::read_dir("src/json_tests/test_parsing").unwrap().flatten() {
        let name = f.file_name().to_string_lossy().into_owned();
        let Ok(content) = fs::read_to_string(f.path()) else {
            println!("read error: {name}");
            continue;
        };
        let result = match Reader::deserialize(&content) {
            Ok(_) => String::from("ok"),
            Err(e) => format!("{e}"),
        };
        println!("{result} - {name}");
    }
}

#[derive(Debug)]
pub enum Value {
    Null,
    Boolean(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

#[derive(Debug)]
pub enum Number {
    Int(i64),
    Float(f64),
}

pub struct Reader<'a> {
    bytes: &'a [u8],
    position: usize,
    depth: usize,
}

impl<'a> Reader<'a> {
    pub fn deserialize(source: &'a str) -> Result<Value, Error> {
        Self { bytes: source.as_bytes(), position: 0, depth: 0 }.read()
    }

    fn read(mut self) -> Result<Value, Error> {
        let result = self.read_value()?;
        skip_whitespace_match!(
            self,
            b @ Some(_) => return Err(Error::expected_end(self.position, b.cloned())),
            None => return Ok(result),
        )
    }

    #[inline]
    fn read_keyword(&mut self, expected: &'static [u8], value: Value) -> Result<Value, Error> {
        match self.bytes.get(self.position + 1..self.position + expected.len()) {
            Some(actual) if actual == &expected[1..] => {
                self.position += expected.len();
                Ok(value)
            },
            actual => Err(Error::expected_keyword(self.position, actual, expected)),
        }
    }

    #[inline]
    fn read_null(&mut self) -> Result<Value, Error> { self.read_keyword(b"null", Value::Null) }

    #[inline]
    fn read_true(&mut self) -> Result<Value, Error> {
        self.read_keyword(b"true", Value::Boolean(true))
    }

    #[inline]
    fn read_false(&mut self) -> Result<Value, Error> {
        self.read_keyword(b"false", Value::Boolean(false))
    }

    fn read_number(&mut self, mut current: Option<u8>) -> Result<Value, Error> {
        let start = self.position;
        self.position += 1;
        let neg = current == Some(b'-');
        if neg {
            current = self.current().cloned();
            self.position += 1;
        };
        match current {
            Some(b'0') =>
                if !matches!(self.current(), Some(b'.' | b'e' | b'E')) {
                    return Ok(Value::Number(Number::Int(0)));
                },
            Some(b'1'..=b'9') => skip_digits_match!(
                self,
                Some(b'.' | b'e' | b'E') => break,
                _ =>
                    return unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.position]) }
                        .parse()
                        .map(|n| Value::Number(Number::Int(n)))
                        .map_err(|error| Error::ParseInt { position: start, error }),
            ),
            b => return Err(Error::expected_digit_sign(self.position, b, false, neg)),
        }
        current = self.current().cloned();
        if current == Some(b'.') {
            self.position += 1;
            skip_digit_expect_first!(
                self,
                b => return Err(Error::expected_digit_sign(self.position, b.cloned(), false, false)),
            );
            current = self.current().cloned()
        }
        if matches!(current, Some(b'e' | b'E')) {
            self.position += 1;
            let sign = matches!(self.current(), Some(b'+' | b'-'));
            if sign {
                self.position += 1;
            }
            skip_digit_expect_first!(
                self,
                b => return Err(Error::expected_digit_sign(self.position, b.cloned(), sign, sign))
            );
        }
        unsafe { std::str::from_utf8_unchecked(&self.bytes[start..self.position]) }
            .parse()
            .map(|n| Value::Number(Number::Float(n)))
            .map_err(|error| Error::ParseFloat { position: start, error })
    }

    fn read_string(&mut self) -> Result<String, Error> {
        self.position += 1;
        let mut result = Vec::new();
        loop {
            match self.current() {
                Some(b'"') => {
                    self.position += 1;
                    return Ok(unsafe { String::from_utf8_unchecked(result) });
                },
                Some(b'\\') => {
                    self.position += 1;
                    match self.current() {
                        Some(&b @ (b'"' | b'\\' | b'/')) => result.push(b),
                        Some(b'b') => result.push(b'\x08'),
                        Some(b'f') => result.push(b'\x0C'),
                        Some(b'n') => result.push(b'\n'),
                        Some(b'r') => result.push(b'\r'),
                        Some(b't') => result.push(b'\t'),
                        Some(b'u') => self.read_unicode(&mut result)?,
                        b => return Err(Error::expected_escape(self.position, b.cloned())),
                    }
                },
                Some(&b @ b' '..) => result.push(b),
                b => return Err(Error::expected_non_control_character(self.position, b.cloned())),
            }
            self.position += 1;
        }
    }

    fn read_unicode(&mut self, vec: &mut Vec<u8>) -> Result<(), Error> {
        self.position += 1;
        let high = self.read_hex()?;
        if !(0xd800..=0xdbff).contains(&high) {
            return match char::try_from(high) {
                Ok(_) => {
                    vec.extend_from_slice(&high.to_be_bytes()[2..]);
                    Ok(())
                },
                Err(error) => Err(Error::CharTryFrom { position: self.position, error }),
            };
        }
        self.position += 1;
        match self.bytes.get(self.position..self.position + 2) {
            Some([b'\\', b'u']) => self.position += 2,
            b => return Err(Error::expected_surrogate(self.position, b)),
        }
        let low = self.read_hex()?;
        if low < 0xdc00 {
            return Err(Error::LowSurrogate { position: self.position, value: low });
        }
        let charcode = 0x10000 + (high - 0xd800) * 0x400 + (low - 0xdc00);
        match char::try_from(charcode) {
            Ok(_) => {
                vec.extend_from_slice(&charcode.to_be_bytes());
                Ok(())
            },
            Err(error) => Err(Error::CharTryFrom { position: self.position, error }),
        }
    }

    #[inline]
    fn read_hex(&mut self) -> Result<u32, Error> {
        match self.bytes.get(self.position..self.position + 4) {
            Some(b) if b.iter().all(u8::is_ascii_hexdigit) => {
                self.position += 3;
                Ok(u32::from_str_radix(unsafe { std::str::from_utf8_unchecked(b) }, 16).unwrap())
            },
            b => Err(Error::expected_hex(self.position, b)),
        }
    }

    fn read_array(&mut self) -> Result<Value, Error> {
        self.depth += 1;
        if self.depth > 128 {
            return Err(Error::DepthError);
        }
        self.position += 1;
        let mut result = Vec::new();
        result.push(skip_whitespace_value_match!(
            self,
            Some(b']') => {
                self.position += 1;
                self.depth -= 1;
                return Ok(Value::Array(result));
            },
            b => Err(Error::expected_value_or_close(self.position, b.cloned())),
        )?);
        skip_whitespace_match!(
            self,
            Some(b']') => {
                self.position += 1;
                self.depth -= 1;
                return Ok(Value::Array(result));
            },
            Some(b',') => {
                self.position += 1;
                result.push(self.read_value()?);
            },
            b => return Err(Error::expected_comma_or_square(self.position, b.cloned())),
        );
    }

    fn read_object(&mut self) -> Result<Value, Error> {
        self.depth += 1;
        if self.depth > 128 {
            return Err(Error::DepthError);
        }
        self.position += 1;
        let mut result = HashMap::new();
        skip_whitespace_match!(
            self,
            Some(b'}') => {
                self.position += 1;
                self.depth -= 1;
                return Ok(Value::Object(result));
            },
            Some(b'"') => break,
            b => return Err(Error::expected_key_or_close(self.position, b.cloned())),
        );
        loop {
            let key = self.read_string()?;
            skip_whitespace_match!(
                self,
                Some(b':') => {
                    self.position += 1;
                    break;
                },
                b => return Err(Error::expected_colon(self.position, b.cloned())),
            );
            result.insert(key, self.read_value()?);
            skip_whitespace_match!(
                self,
                Some(b',') => {
                    self.position += 1;
                    break;
                },
                Some(b'}') => {
                    self.position += 1;
                    self.depth -= 1;
                    return Ok(Value::Object(result));
                },
                b => return Err(Error::expected_comma_or_curly(self.position, b.cloned())),
            );
            skip_whitespace_match!(
                self,
                Some(b'"') => break,
                b => return Err(Error::expected_key(self.position, b.cloned())),
            )
        }
    }

    fn read_value(&mut self) -> Result<Value, Error> {
        skip_whitespace_value_match!(
            self,
            b => Err(Error::expected_value(self.position, b.cloned())),
        )
    }

    #[inline(always)]
    fn current(&self) -> Option<&u8> { self.bytes.get(self.position) }
}

#[derive(Debug)]
pub enum Error {
    UnexpectedValue(UnexpectedValueError),
    UnexpectedValues(UnexpectedValuesError),
    DepthError,
    ParseInt { position: usize, error: ParseIntError },
    ParseFloat { position: usize, error: ParseFloatError },
    CharTryFrom { position: usize, error: CharTryFromError },
    LowSurrogate { position: usize, value: u32 },
}

#[derive(Debug)]
pub struct UnexpectedValueError {
    position: usize,
    value: Option<u8>,
    description: &'static str,
}

impl UnexpectedValueError {
    const DESCRIPTIONS: [&'static str; 13] = [
        "Expected a value ('n', 't', 'f', <digit>, '-', '\"', '[', '{')",
        "Expected a key or a closing brace ('\"', '}')", "Expected a colon (':')",
        "Expected a comma or a closing brace (',', '}')", "Expected a key ('\"')",
        "Expected a value or a closing bracket ('n', 't', 'f', <digit>, '-', '\"', '[', '{', ']')",
        "Expected a comma or a closing bracket (',', ']')",
        "Expected an escape sequence ('\"', '\\', '/', 'b', 'f', 'n', 'r', 't', 'u')",
        "Expected a non control character (from ' ')", "Expected a digit (from '0' to '9')",
        "Expected a digit or minus sign (from '0' to '9', '-')",
        "Expected a digit or a sign (from '0' to '9', '-', '+')", "Expected a end of file",
    ];

    fn new(position: usize, value: Option<u8>, description_index: usize) -> Self {
        Self { position, value, description: Self::DESCRIPTIONS[description_index] }
    }
}

impl Display for UnexpectedValueError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.value {
            Some(v) => write!(
                f,
                "Error at position: {}. {}, got: '{}'",
                self.position,
                self.description,
                char::from(v)
            ),
            None =>
                write!(f, "Error at position: {}. {}, got: 'None'", self.position, self.description),
        }
    }
}

#[derive(Debug)]
pub struct UnexpectedValuesError {
    position: usize,
    values: Option<Box<[u8]>>,
    description: &'static str,
}

impl UnexpectedValuesError {
    const DESCRIPTIONS: [&'static str; 5] = [
        "Expected a 4 digit hex string (\"xxxx\")", "Expected another surrogate (\"\\u\")",
        "Expected null", "Expected true", "Expected false",
    ];

    fn new(position: usize, values: Option<&[u8]>, description_index: usize) -> Self {
        Self {
            position,
            values: values.map(|s| s.to_vec().into_boxed_slice()),
            description: Self::DESCRIPTIONS[description_index],
        }
    }
}

impl Display for UnexpectedValuesError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.values {
            Some(v) => write!(
                f,
                "Error at position: {}. {}, got: '{}'",
                self.position,
                self.description,
                v.iter().map(|&b| char::from(b)).collect::<String>()
            ),
            None =>
                write!(f, "Error at position: {}. {}, got: None", self.position, self.description),
        }
    }
}

impl Error {
    fn expected_value(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 0))
    }

    fn expected_key_or_close(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 1))
    }

    fn expected_colon(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 2))
    }

    fn expected_comma_or_curly(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 3))
    }

    fn expected_key(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 4))
    }

    fn expected_value_or_close(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 5))
    }

    fn expected_comma_or_square(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 6))
    }

    fn expected_hex(position: usize, values: Option<&[u8]>) -> Self {
        Self::UnexpectedValues(UnexpectedValuesError::new(position, values, 0))
    }

    fn expected_surrogate(position: usize, values: Option<&[u8]>) -> Self {
        Self::UnexpectedValues(UnexpectedValuesError::new(position, values, 1))
    }

    fn expected_escape(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 7))
    }

    fn expected_non_control_character(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 8))
    }

    fn expected_digit_sign(position: usize, value: Option<u8>, plus: bool, minus: bool) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, match (plus, minus) {
            (false, false) => 9,
            (false, true) => 10,
            (true, true) => 11,
            _ => unreachable!(),
        }))
    }

    fn expected_keyword(position: usize, values: Option<&[u8]>, expected: &[u8]) -> Self {
        Self::UnexpectedValues(UnexpectedValuesError::new(position, values, match expected {
            b"null" => 2,
            b"true" => 3,
            b"false" => 4,
            _ => unreachable!(),
        }))
    }

    fn expected_end(position: usize, value: Option<u8>) -> Self {
        Self::UnexpectedValue(UnexpectedValueError::new(position, value, 12))
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedValue(e) => Display::fmt(e, f),
            Error::UnexpectedValues(e) => Display::fmt(e, f),
            Error::DepthError => write!(f, "Exceeded depth limit (128)"),
            Error::ParseInt { position, error } =>
                write!(f, "Error at position: {}: {}", position, error),
            Error::ParseFloat { position, error } =>
                write!(f, "Error at position: {}: {}", position, error),
            Error::CharTryFrom { position, error } =>
                write!(f, "Error at position: {}: {}", position, error),
            Error::LowSurrogate { position, value } =>
                write!(f, "Error at position: {}. 2nd surrogate was invalid: {:x}", position, value),
        }
    }
}
