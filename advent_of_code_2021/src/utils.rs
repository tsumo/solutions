use std::fs;

pub fn parse_file_to_lines(path: &str) -> Vec<String> {
  fs::read_to_string(path)
    .expect("Cannot read file")
    .lines()
    .map(|s| s.to_string())
    .collect()
}

pub fn parse_file_to_numbers(path: &str) -> Vec<i32> {
  let input = fs::read_to_string(path).expect("Cannot read file");
  input.lines().map(|s| s.parse::<i32>().unwrap()).collect()
}
