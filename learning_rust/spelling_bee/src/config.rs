use std::env;

// paman -Ss words
const kDefaultDict: &str = "/usr/share/dict/words";

pub struct Config {
  pub input_path: String,
  pub min_len: usize,
  pub max_len: usize,
  pub mid_letter_min_rank: usize,
  pub game_word_limit: usize,
  pub min_pangram_count: usize,
  pub word_filter_rx: String,
}

pub fn from_args() -> Config {
  let args: Vec<String> = env::args().collect();
  let usage = r#"USAGE: spelling_bee [DICT_PATH]
  "#;
  let input_path =
    if let Some(s) = args.get(1) { s.clone() }
    else { String::from(kDefaultDict) };

  let conf = Config{
    input_path: input_path,
    min_len: 4,
    max_len: 12,
    mid_letter_min_rank: 15,
    game_word_limit: 40,
    min_pangram_count: 3,
    word_filter_rx: String::from(r#"^[a-z]+$"#),
  };
  return conf;
}

