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

  assert_eq!(res, 379114);

  res
}

pub fn second() -> u128 {
  let lines = utils::parse_file_to_lines("src/day06.input");

  let mut fish: Vec<u128> = vec![0, 0, 0, 0, 0, 0, 0, 0, 0];

  let input = lines[0].split(",").map(|s| s.parse::<u64>().unwrap());

  for age in input {
    fish[age as usize] += 1;
  }

  for _i in 0..256 {
    let new = fish[0];
    for d in 0..6 {
      fish[d] = fish[d + 1];
    }
    fish[6] = new + fish[7];
    fish[7] = fish[8];
    fish[8] = new;
  }

  let res: u128 = fish.iter().fold(0, |acc, c| acc + *c as u128);

  assert_eq!(res, 1702631502303);

  res
}
