use crate::utils;

pub fn first() -> usize {
  let lines = utils::parse_file_to_lines("src/day06.input");

  let mut fish: Vec<u8> = lines[0]
    .split(",")
    .map(|s| s.parse::<u8>().unwrap())
    .collect();

  for _i in 0..80 {
    let len = fish.len();
    for j in 0..len {
      if fish[j] == 0 {
        fish[j] = 6;
        fish.push(8);
      } else {
        fish[j] -= 1;
      }
    }
  }

  let res = fish.len();

  res
}
