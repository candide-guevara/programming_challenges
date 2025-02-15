mod anagram;
mod config;
mod error;
mod game;
mod util;

fn main() {
  let conf = config::from_args();
  let words = anagram::from_dict(&conf).unwrap();
  let game = game::from_words(&conf, &words).unwrap();
  println!("game = {game}");
  println!("solution = {}", game::solution_for('b', "rijblda", &words).unwrap());
}

