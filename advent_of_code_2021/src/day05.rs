use crate::utils;

type Coords = (usize, usize, usize, usize);

pub fn first() -> i32 {
  let lines = utils::parse_file_to_lines("src/day05.input");

  let mut coords: Vec<Coords> = vec![];

  let mut width: usize = 0;
  let mut heigth: usize = 0;

  for line in lines {
    let numbers: Vec<usize> = line
      .replace(" -> ", " ")
      .replace(",", " ")
      .split_whitespace()
      .map(|s| s.parse::<usize>().unwrap())
      .collect();
    if numbers.len() == 4 {
      width = width.max(numbers[0]).max(numbers[2]);
      heigth = heigth.max(numbers[1]).max(numbers[3]);
      coords.push((numbers[0], numbers[1], numbers[2], numbers[3]));
    }
  }

  width += 1;
  heigth += 1;

  let mut board: Vec<Vec<usize>> = vec![vec![0; width]; heigth];

  for coord in &coords {
    let x1 = coord.0.min(coord.2);
    let y1 = coord.1.min(coord.3);
    let x2 = coord.2.max(coord.0);
    let y2 = coord.3.max(coord.1);
    if x1 == x2 && y1 == y2 {
      continue;
    }
    if x1 == x2 {
      for i in y1..=y2 {
        board[x1][i] += 1;
      }
    }
    if y1 == y2 {
      for i in x1..=x2 {
        board[i][y1] += 1;
      }
    }
  }

  let mut res = 0;
  for l in board {
    for c in l {
      if c > 1 {
        res += 1;
      }
    }
  }

  assert_eq!(res, 4745);

  res
}
