use::std::fs;

fn main() {
  let heights = fs::read_to_string("heights.txt")
    .expect("Something went wrong reading the file")
    .lines()
    .map(|line| line.parse::<i32>().unwrap())
    .collect::<Vec<i32>>();
  //println!("Read {} heights", heights.len());
  //println!("{}", heights[6237 + 26263]); // = 50155
  //println!("{}", heights[26263+28900*2] - heights[26263+28900]); // = 45992

  let mut pattern_start = 0;
  let mut pattern_length = 0;
  for i in 28900..heights.len() {
    let mut all_equal = true;
    if i % 1000 == 0 {
      println!("Checking height {}", i);
    }
    for j in 26263..i {
      all_equal = true;
      for k in 1..100 {
        if j + k*i >= heights.len() {
          continue
        }
        if heights[j+k*(i-1)] != heights[j + k*i] {
          all_equal = false;
          break;
        }
      }
      if all_equal {
        pattern_start = j;
        pattern_length = i;
        break;
      }
    }
    if all_equal {
      break;
    }
  }
  let mut num_iterations: usize = 1000000000000;
  let delta: usize = (heights[pattern_start + pattern_length] - heights[pattern_start]) as usize;
  let mut tower_height: usize = heights[pattern_start] as usize;
  num_iterations -= pattern_start;
  tower_height += (num_iterations / pattern_length) * delta;
  num_iterations %= pattern_length;
  tower_height += heights[pattern_start + num_iterations] as usize;
  println!("tower height: {}", tower_height);
}
// Found a pattern of length 28900 starting at 26263
// [26263 + i*28900]
// 1542941216996