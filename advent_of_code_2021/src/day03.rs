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
  let half = numbers.len() / 2;
  if ones > half {
    Count::ONES
  } else if ones < half {
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
