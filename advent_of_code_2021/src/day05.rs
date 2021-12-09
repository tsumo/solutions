use crate::utils;

type Coords = (usize, usize, usize, usize);

type Board = Vec<Vec<u32>>;

fn parse_input() -> (Vec<Coords>, usize, usize) {
  let lines = utils::parse_file_to_lines("src/day05.input");

  let mut coords_vec: Vec<Coords> = vec![];

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
      coords_vec.push((numbers[0], numbers[1], numbers[2], numbers[3]));
    }
  }

  width += 1;
  heigth += 1;

  (coords_vec, width, heigth)
}

fn create_board(width: usize, heigth: usize) -> Board {
  vec![vec![0; width]; heigth]
}

fn try_draw_straight_line(board: &mut Board, coords: &Coords) {
  let x1 = coords.0.min(coords.2);
  let y1 = coords.1.min(coords.3);
  let x2 = coords.2.max(coords.0);
  let y2 = coords.3.max(coords.1);
  if x1 == x2 && y1 == y2 {
    return;
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

fn try_draw_diagonal_line(board: &mut Board, coords: &Coords) {
  let x1 = coords.0;
  let y1 = coords.1;
  let x2 = coords.2;
  let y2 = coords.3;
  if x1 == x2 || y1 == y2 {
    return;
  }
  let x_step: i32 = if x1 > x2 { -1 } else { 1 };
  let y_step: i32 = if y1 > y2 { -1 } else { 1 };
  let mut x = x1 as i32;
  let mut y = y1 as i32;

  while x != x2 as i32 {
    board[y as usize][x as usize] += 1;
    x += x_step;
    y += y_step;
  }
  board[y as usize][x as usize] += 1;
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
  let (coords_vec, width, heigth) = parse_input();

  let mut board = create_board(width, heigth);

  for coords in &coords_vec {
    try_draw_straight_line(&mut board, coords);
  }

  let res = sum_board(&board);

  assert_eq!(res, 4745);

  res
}

pub fn second() -> i32 {
  let (coords_vec, width, heigth) = parse_input();

  let mut board = create_board(width, heigth);

  for coords in &coords_vec {
    try_draw_straight_line(&mut board, coords);
    try_draw_diagonal_line(&mut board, coords);
  }

  let res = sum_board(&board);

  assert_eq!(res, 18442);

  res
}
