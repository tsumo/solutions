use crate::utils;

fn set_bit_at(a: &mut i32, i: u8) {
  *a |= 1 << i;
}

fn more_ones_at_index(numbers: &Vec<i32>, i: u8) -> bool {
  let mask = 1 << i;
  let mut ones = 0;
  for n in numbers {
    if (n & mask) != 0 {
      ones += 1;
    }
  }
  ones >= (numbers.len() / 2)
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
    if more_ones_at_index(&numbers, i) {
      set_bit_at(&mut gamma, i)
    } else {
      set_bit_at(&mut epsilon, i)
    }
  }

  let res = gamma * epsilon;

  assert_eq!(res, 2724524);

  res
}
