use crate::utils;

fn set_bit_at(a: &mut i32, i: u8) {
  *a |= 1 << i;
}

enum Count {
  ONES,
  ZEROS,
  EQUAL,
}

fn count_bits_at_index(numbers: &Vec<i32>, i: u8) -> Count {
  let mask = 1 << i;
  let mut ones = 0;
  for n in numbers {
    if (n & mask) != 0 {
      ones += 1;
    }
  }
  let half: f32 = numbers.len() as f32 / 2.0;
  if ones as f32 > half {
    Count::ONES
  } else if (ones as f32) < half {
    Count::ZEROS
  } else {
    Count::EQUAL
  }
}

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day03.input");

  let numbers = lines
    .iter()
    .map(|s| i32::from_str_radix(s, 2).unwrap())
    .collect();

  let mut gamma = 0;
  let mut epsilon = 0;

  for i in 0..12 {
    match count_bits_at_index(&numbers, i) {
      Count::EQUAL => set_bit_at(&mut gamma, i),
      Count::ONES => set_bit_at(&mut gamma, i),
      Count::ZEROS => set_bit_at(&mut epsilon, i),
    }
  }

  let res = gamma * epsilon;

  assert_eq!(res, 2724524);

  res
}

fn get_bit_at(a: i32, n: u8) -> i32 {
  a >> n & 1
}

pub fn second() -> i32 {
  let lines = utils::parse_file_to_lines("src/day03.input");

  let numbers: Vec<i32> = lines
    .iter()
    .map(|s| i32::from_str_radix(s, 2).unwrap())
    .collect();

  let mut oxygen = 0;
  let mut co2 = 0;

  let mut numbers_oxygen = numbers.clone();
  let mut numbers_co2 = numbers.clone();

  for i in (0..12).rev() {
    match count_bits_at_index(&numbers_oxygen, i) {
      Count::EQUAL => numbers_oxygen.retain(|n| get_bit_at(*n, i) == 1),
      Count::ONES => numbers_oxygen.retain(|n| get_bit_at(*n, i) == 1),
      Count::ZEROS => numbers_oxygen.retain(|n| get_bit_at(*n, i) == 0),
    }
    if numbers_oxygen.len() == 1 {
      oxygen = numbers_oxygen[0];
      break;
    }
  }

  for i in (0..12).rev() {
    match count_bits_at_index(&numbers_co2, i) {
      Count::EQUAL => numbers_co2.retain(|n| get_bit_at(*n, i) == 0),
      Count::ONES => numbers_co2.retain(|n| get_bit_at(*n, i) == 0),
      Count::ZEROS => numbers_co2.retain(|n| get_bit_at(*n, i) == 1),
    }
    if numbers_co2.len() == 1 {
      co2 = numbers_co2[0];
      break;
    }
  }

  let res = oxygen * co2;

  assert_eq!(res, 2775870);

  res
}
