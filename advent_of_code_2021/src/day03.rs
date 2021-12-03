use crate::utils;

fn get_bit_at(a: i32, n: u8) -> i32 {
  if (a >> n & 1) == 0 {
    1
  } else {
    -1
  }
}

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day03.input");

  let numbers = lines.iter().map(|s| i32::from_str_radix(s, 2).unwrap());

  let r1 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 11));
  let r2 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 10));
  let r3 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 9));
  let r4 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 8));
  let r5 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 7));
  let r6 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 6));
  let r7 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 5));
  let r8 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 4));
  let r9 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 3));
  let r10 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 2));
  let r11 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 1));
  let r12 = numbers.clone().fold(0, |acc, c| acc - get_bit_at(c, 0));

  let g1 = if r1 > 0 { 1 } else { 0 };
  let g2 = if r2 > 0 { 1 } else { 0 };
  let g3 = if r3 > 0 { 1 } else { 0 };
  let g4 = if r4 > 0 { 1 } else { 0 };
  let g5 = if r5 > 0 { 1 } else { 0 };
  let g6 = if r6 > 0 { 1 } else { 0 };
  let g7 = if r7 > 0 { 1 } else { 0 };
  let g8 = if r8 > 0 { 1 } else { 0 };
  let g9 = if r9 > 0 { 1 } else { 0 };
  let g10 = if r10 > 0 { 1 } else { 0 };
  let g11 = if r11 > 0 { 1 } else { 0 };
  let g12 = if r12 > 0 { 1 } else { 0 };

  let gamma = format!(
    "{}{}{}{}{}{}{}{}{}{}{}{}",
    g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12
  );
  let gamma = i32::from_str_radix(&gamma, 2).unwrap();

  let e1 = if r1 > 0 { 0 } else { 1 };
  let e2 = if r2 > 0 { 0 } else { 1 };
  let e3 = if r3 > 0 { 0 } else { 1 };
  let e4 = if r4 > 0 { 0 } else { 1 };
  let e5 = if r5 > 0 { 0 } else { 1 };
  let e6 = if r6 > 0 { 0 } else { 1 };
  let e7 = if r7 > 0 { 0 } else { 1 };
  let e8 = if r8 > 0 { 0 } else { 1 };
  let e9 = if r9 > 0 { 0 } else { 1 };
  let e10 = if r10 > 0 { 0 } else { 1 };
  let e11 = if r11 > 0 { 0 } else { 1 };
  let e12 = if r12 > 0 { 0 } else { 1 };

  let epsilon = format!(
    "{}{}{}{}{}{}{}{}{}{}{}{}",
    e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12
  );
  let epsilon = i32::from_str_radix(&epsilon, 2).unwrap();

  let res = gamma * epsilon;

  assert_eq!(res, 2724524);

  res
}
