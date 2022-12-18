use std::fs;
use std::cmp;
use std::io::stdin;
const WIDTH: usize = 7;
const HEIGHT: usize = 1<<18;

#[derive(Debug)]
#[derive(Clone)]
#[derive(PartialEq)]
enum Direction {
  Left,
  Right,
  Down,
}

#[derive(Debug)]
#[derive(Clone)]
enum RockType {
  HLine,
  Plus,
  L,
  VLine,
  Box,
}
fn get_rock_offsets(rock_type: RockType) -> Vec<(usize, usize)> {
  return match rock_type {
    RockType::HLine => vec![(0, 0), (1, 0), (2, 0), (3, 0)],
    RockType::Plus => vec![(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)],
    RockType::L => vec![(0, 0), (1, 0), (2, 0), (2, 1), (2, 2)],
    RockType::VLine => vec![(0, 0), (0, 1), (0, 2), (0, 3)],
    RockType::Box => vec![(0, 0), (1, 0), (0, 1), (1, 1)],
  };
}

struct RockTypes {
  rock_types: Vec<RockType>,
  count: usize,
  total: usize,
}

impl Iterator for RockTypes {
  type Item = RockType;
  fn next(&mut self) -> Option<Self::Item> {
    if self.count == self.total { return None; }
    let cur = self.count;
    self.count += 1;
    return Some(self.rock_types[cur % self.rock_types.len()].clone());
  }
}

struct Directions {
  directions: Vec<Direction>,
  count: usize,
}

impl Iterator for Directions {
  type Item = Direction;
  fn next(&mut self) -> Option<Self::Item> {
    let cur = self.count;
    self.count += 1;
    return Some(self.directions[cur % self.directions.len()].clone());
  }
}

fn get_jet_directions() -> Vec<Direction> {
  let fd = fs::read_to_string("input.txt")
    .expect("Something went wrong reading the file");
  return fd
    .trim()
    .chars()
    .map(|c| match c {
      '<' => Direction::Left,
      '>' => Direction::Right,
      _ => panic!("Unknown jet direction"),
    })
    .collect::<Vec<Direction>>();
}

#[derive(Debug)]
#[derive(Clone)]
#[derive(Copy)]
#[derive(PartialEq)]
enum GridItem {
  Empty,
  Full,
}

struct State {
  grid: [[GridItem; WIDTH]; HEIGHT],
  x: usize,
  y: usize,
  tower_height: usize,
}

fn show_state(state: &State, rock: RockType) {
  let offsets = get_rock_offsets(rock);
  for y in (0..(state.y + 4)).rev() {
    for x in 0..WIDTH {
      let mut printed = false;
      for (x_off, y_off) in &offsets {
        if x == state.x + x_off && y == state.y + y_off {
          print!("@");
          printed = true;
        }
      }
      if printed { continue; }
      match state.grid[y][x] {
        GridItem::Empty => print!("."),
        GridItem::Full => print!("#"),
      }
    }
    println!("");
  }
}

fn move_rock(state: &mut State, direction: Direction, rock_type: RockType) -> bool {
  let mut x = state.x;
  let mut y = state.y;
  if x == 0 && direction == Direction::Left { return false; }
  if y == 0 && direction == Direction::Down { return false; }
  match direction {
    Direction::Left => x -= 1, 
    Direction::Right => x += 1,
    Direction::Down => y -= 1,
  }
  let offsets = get_rock_offsets(rock_type);
  for (x_off, y_off) in offsets {
    if let Some(row) = state.grid.get(y+y_off) {
      if let Some(contents) = row.get(x+x_off) {
        if *contents == GridItem::Full {
          return false;
        }
      } else { return false; }
    } else { return false; }
  }
  state.x = x;
  state.y = y;
  return true;
}

fn main() {
  let mut directions = Directions {
    directions: get_jet_directions(),
    count: 0,
  };

  let rock_types = RockTypes {
    rock_types: vec![
      RockType::HLine,
      RockType::Plus,
      RockType::L,
      RockType::VLine,
      RockType::Box,
    ],
    count: 0,
    total: 1000000000000,
  };

  let mut state = State {
    grid: [[GridItem::Empty; WIDTH]; HEIGHT],
    x: 0,
    y: 0,
    tower_height: 0,
  };

  for rock in rock_types {
    println!("{}", state.tower_height);
    // stdin().read_line(&mut String::new()).unwrap();
    state.x = 2;
    state.y = state.tower_height + 3;
    loop {
      // show_state(&state, rock.clone());
      // stdin().read_line(&mut String::new()).unwrap();
      // print!("{}[2J", 27 as char);
      let direction = directions.next().unwrap();
      move_rock(&mut state, direction, rock.clone());
      // show_state(&state, rock.clone());
      // stdin().read_line(&mut String::new()).unwrap();
      // print!("{}[2J", 27 as char);
      if !move_rock(&mut state, Direction::Down, rock.clone()) {
        state.tower_height = cmp::max(state.tower_height, 1 + state.y + match rock {
          RockType::HLine => 0,
          RockType::Plus => 2,
          RockType::L => 2,
          RockType::VLine => 3,
          RockType::Box => 1,
        });
        let offsets = get_rock_offsets(rock.clone());
        for (x_off, y_off) in offsets {
          state.grid[state.y+y_off][state.x+x_off] = GridItem::Full;
        }
        break;
      }
    }
  }
  println!("Height of tower: {:?}", state.tower_height);
}
// 2771 too low