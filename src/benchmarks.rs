extern crate test;

use std::fs;
use super::json::Reader;
use test::Bencher;
use rand::prelude::IteratorRandom;
use rand::Rng;
use serde_json::{Map, Number, Value};

#[bench]
fn test_serde(bencher: &mut Bencher) {
    let content = fs::read_to_string("test.json").unwrap();
    bencher.iter(|| {
        let value: Value = serde_json::de::from_str(&content).unwrap();
    });
}

#[bench]
fn test_my(bencher: &mut Bencher) {
    let content = fs::read_to_string("test.json").unwrap();
    bencher.iter(|| {
        let value = Reader::deserialize(&content).unwrap();
    });
}

pub fn create_file() {
    let result = Value::Object(random_object(0));
    let json = serde_json::ser::to_string_pretty(&result).unwrap();
    fs::write("test.json", json).unwrap();
}

fn random_value(depth: usize) -> Value {
    match rand::thread_rng().gen_range(0..=5) {
        0 => Value::Null,
        1 => Value::Bool(rand::random()),
        2 => Value::Number(random_number()),
        3 => Value::String(random_string()),
        4 => Value::Array(random_array(depth + 1)),
        5 => Value::Object(random_object(depth + 1)),
        _ => unreachable!(),
    }
}

fn random_number() -> Number {
    match rand::random() {
        false => Number::from_f64(rand::random()).unwrap(),
        true => Number::from(rand::random::<i64>()),
    }
}

fn random_string() -> String {
    let length = rand::thread_rng().gen_range(8..16);
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
    (0..length).map(|_| chars.chars().choose(&mut rand::thread_rng()).unwrap()).collect()
}

fn random_array(depth: usize) -> Vec<Value> {
    if depth > 5 {
        return Vec::new();
    }
    let length = rand::thread_rng().gen_range(8..32);
    (0..length).map(|_| random_value(depth)).collect()
}

fn random_object(depth: usize) -> Map<String, Value> {
    if depth > 5 {
        return Map::new();
    }
    let length = rand::thread_rng().gen_range(8..32);
    (0..length).map(|_| (random_string(), random_value(depth))).collect()

}