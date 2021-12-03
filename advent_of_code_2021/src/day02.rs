use crate::utils;

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day02.input");

  let mut position: i32 = 0;
  let mut depth: i32 = 0;

  for line in lines {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let command = parts[0];
    let number = parts[1].parse::<i32>().unwrap();
    match command {
      "forward" => position += number,
      "down" => depth += number,
      "up" => depth -= number,
      _ => {}
    }
  }

  let res = position * depth;

  assert_eq!(res, 2039256);

  res
}

pub fn second() -> i32 {
  let lines = utils::parse_file_to_lines("src/day02.input");

  let mut position: i32 = 0;
  let mut depth: i32 = 0;
  let mut aim: i32 = 0;

  for line in lines {
    let parts: Vec<&str> = line.split_whitespace().collect();
    let command = parts[0];
    let number = parts[1].parse::<i32>().unwrap();
    match command {
      "forward" => {
        position += number;
        depth += aim * number
      }
      "down" => aim += number,
      "up" => aim -= number,
      _ => {}
    }
  }

  let res = position * depth;

  assert_eq!(res, 1856459736);

  res
}
