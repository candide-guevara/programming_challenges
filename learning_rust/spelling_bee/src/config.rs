use std::env;

// paman -Ss words
const kDefaultDict: &str = "/usr/share/dict/words";

pub enum MainDo {
  DEMO,
  BENCHMARK,
}

pub struct Config {
  pub input_path: String,
  pub min_len: usize,
  pub max_len: usize,
  pub mid_letter_min_rank: usize,
  pub game_word_limit: usize,
  pub min_pangram_count: usize,
  pub word_filter_rx: String,
  pub routine: MainDo,
  pub bench_tries: u32,
}

pub fn from_args() -> Config {
  let args: Vec<String> = env::args().skip(1).collect();
  let usage = r#"USAGE: spelling_bee --benchmark [DICT_PATH]
  "#;
  let mut input_path = String::from(kDefaultDict);
  let mut routine = MainDo::DEMO;
  for a in args {
    if a == "--benchmark" { routine = MainDo::BENCHMARK; }
    else { input_path = a; }
  }

  let conf = Config{
    input_path: input_path,
    min_len: 4,
    max_len: 12,
    mid_letter_min_rank: 15,
    game_word_limit: 40,
    min_pangram_count: 3,
    word_filter_rx: String::from(r#"^[a-z]+$"#),
    routine: routine,
    bench_tries: 100 * 1000,
  };
  return conf;
}

