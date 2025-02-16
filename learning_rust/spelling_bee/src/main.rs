use std::time;

mod anagram;
mod config;
mod error;
mod game;
mod util;

fn benchmark(conf: &config::Config, words: &anagram::Anagram) {
  let tries = conf.bench_tries;
  let now = time::Instant::now();
  for i in 0..tries {
    game::from_words(&conf, &words).unwrap();
  }
  println!("Bench (tries:{}) ran in {}secs", tries, now.elapsed().as_secs_f32());
}

fn demo(conf: &config::Config, words: &anagram::Anagram) {
  let game = game::from_words(&conf, &words).unwrap();
  println!("game = {game}");
  let solution = game::solution_for('b', "rijblda", &words).unwrap();
  println!("solution = {solution}");
}

fn main() {
  let conf = config::from_args();
  let words = anagram::from_dict(&conf).unwrap();
  match conf.routine {
    config::MainDo::DEMO => demo(&conf, &words),
    config::MainDo::BENCHMARK => benchmark(&conf, &words),
  }
}

