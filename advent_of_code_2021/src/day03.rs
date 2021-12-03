use crate::utils;

fn get_bit_at(a: i32, n: u8) -> i32 {
  if (a >> n & 1) == 0 {
    -1
  } else {
    1
  }
}

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day03.input");

  let numbers = lines.iter().map(|s| i32::from_str_radix(s, 2).unwrap());

  let mut r = (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);

  for number in numbers {
    r.0 += get_bit_at(number, 11);
    r.1 += get_bit_at(number, 10);
    r.2 += get_bit_at(number, 9);
    r.3 += get_bit_at(number, 8);
    r.4 += get_bit_at(number, 7);
    r.5 += get_bit_at(number, 6);
    r.6 += get_bit_at(number, 5);
    r.7 += get_bit_at(number, 4);
    r.8 += get_bit_at(number, 3);
    r.9 += get_bit_at(number, 2);
    r.10 += get_bit_at(number, 1);
    r.11 += get_bit_at(number, 0);
  }

  let g0 = if r.0 > 0 { 1 } else { 0 };
  let g1 = if r.1 > 0 { 1 } else { 0 };
  let g2 = if r.2 > 0 { 1 } else { 0 };
  let g3 = if r.3 > 0 { 1 } else { 0 };
  let g4 = if r.4 > 0 { 1 } else { 0 };
  let g5 = if r.5 > 0 { 1 } else { 0 };
  let g6 = if r.6 > 0 { 1 } else { 0 };
  let g7 = if r.7 > 0 { 1 } else { 0 };
  let g8 = if r.8 > 0 { 1 } else { 0 };
  let g9 = if r.9 > 0 { 1 } else { 0 };
  let g10 = if r.10 > 0 { 1 } else { 0 };
  let g11 = if r.11 > 0 { 1 } else { 0 };

  let gamma = format!(
    "{}{}{}{}{}{}{}{}{}{}{}{}",
    g0, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11
  );
  let gamma = i32::from_str_radix(&gamma, 2).unwrap();

  let e0 = if r.0 > 0 { 0 } else { 1 };
  let e1 = if r.1 > 0 { 0 } else { 1 };
  let e2 = if r.2 > 0 { 0 } else { 1 };
  let e3 = if r.3 > 0 { 0 } else { 1 };
  let e4 = if r.4 > 0 { 0 } else { 1 };
  let e5 = if r.5 > 0 { 0 } else { 1 };
  let e6 = if r.6 > 0 { 0 } else { 1 };
  let e7 = if r.7 > 0 { 0 } else { 1 };
  let e8 = if r.8 > 0 { 0 } else { 1 };
  let e9 = if r.9 > 0 { 0 } else { 1 };
  let e10 = if r.10 > 0 { 0 } else { 1 };
  let e11 = if r.11 > 0 { 0 } else { 1 };

  let epsilon = format!(
    "{}{}{}{}{}{}{}{}{}{}{}{}",
    e0, e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11
  );
  let epsilon = i32::from_str_radix(&epsilon, 2).unwrap();

  let res = gamma * epsilon;

  assert_eq!(res, 2724524);

  res
}
