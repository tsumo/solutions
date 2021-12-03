use crate::utils;

pub fn first() -> i32 {
  let numbers = utils::parse_file_to_numbers("src/day01.input");

  let res = numbers.iter().fold((0, -1), |acc, c| {
    if acc.0 < *c {
      (*c, acc.1 + 1)
    } else {
      (*c, acc.1)
    }
  });

  assert_eq!(res.1, 1722);

  return res.1;
}

pub fn second() -> i32 {
  let numbers = utils::parse_file_to_numbers("src/day01.input");

  let mut res: i32 = -1;
  let mut prev_sum: i32 = 0;

  for i in 0..numbers.len() - 2 {
    let sum: i32 = numbers[i] + numbers[i + 1] + numbers[i + 2];
    if sum > prev_sum {
      res += 1
    }
    prev_sum = sum;
  }

  assert_eq!(res, 1748);

  res
}
