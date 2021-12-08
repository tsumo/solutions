use crate::utils;

type Coords = (usize, usize, usize, usize);

type Board = Vec<Vec<usize>>;

fn parse_input() -> (Vec<Coords>, usize, usize) {
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

  (coords, width, heigth)
}

fn create_board(width: usize, heigth: usize) -> Board {
  vec![vec![0; width]; heigth]
}

fn draw_straight_line(board: &mut Board, x1: usize, y1: usize, x2: usize, y2: usize) {
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

fn sum_board(board: &Board) -> i32 {
  let mut res = 0;
  for l in board {
    for c in l {
      if c > &1 {
        res += 1;
      }
    }
  }
  res
}

pub fn first() -> i32 {
  let (coords, width, heigth) = parse_input();

  let mut board = create_board(width, heigth);

  for coord in &coords {
    let x1 = coord.0.min(coord.2);
    let y1 = coord.1.min(coord.3);
    let x2 = coord.2.max(coord.0);
    let y2 = coord.3.max(coord.1);
    if x1 == x2 && y1 == y2 {
      continue;
    }
    draw_straight_line(&mut board, x1, y1, x2, y2);
  }

  let res = sum_board(&board);

  assert_eq!(res, 4745);

  res
}
