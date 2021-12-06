use crate::utils;

fn parse_line(line: &str) -> Vec<i32> {
  line
    .split_whitespace()
    .map(|s| s.parse::<i32>().unwrap())
    .collect()
}

fn check_win(marks: &Vec<bool>, board_i: usize, size: usize) -> bool {
  let start = board_i * (size * size);
  let mut full_horizontal: bool;
  let mut full_vertical: bool;
  for i in 0..size {
    full_horizontal = true;
    full_vertical = true;
    for j in 0..size {
      if !marks[start + i * size + j] {
        full_horizontal = false;
      }
      if !marks[start + i + j * size] {
        full_vertical = false;
      }
    }
    if full_horizontal || full_vertical {
      return true;
    }
  }
  false
}

fn sum_unmarked(boards: &Vec<i32>, marks: &Vec<bool>, board_i: usize, size: usize) -> i32 {
  let start = board_i * (size * size);
  let end = start + (size * size);
  let mut sum = 0;
  for i in start..end {
    if !marks[i] {
      sum += boards[i]
    }
  }
  sum
}

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day04.input");
  let numbers: Vec<i32> = lines[0]
    .split(",")
    .map(|s| s.parse::<i32>().unwrap())
    .collect();

  let size = parse_line(&lines[2]).len();

  let boards_cnt = (lines.len() - 2) / (size + 1);

  let mut boards = Vec::<i32>::new();

  for i in 0..boards_cnt {
    for j in 0..size {
      boards.extend(parse_line(&lines[i * 6 + j + 2]))
    }
  }

  let mut marks = vec![false; boards.len()];

  for n in numbers {
    for (i, x) in boards.iter().enumerate() {
      if *x == n {
        marks[i] = true;
        for b in 0..boards_cnt {
          if check_win(&marks, b, size) {
            let res = sum_unmarked(&boards, &marks, b, size) * n;

            assert_eq!(res, 32844);

            return res;
          }
        }
      }
    }
  }

  panic!("No winning board")
}
