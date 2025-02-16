mod test;
use super::config;
use super::anagram;
use super::util::*;
use super::error::*;

use rand;
use rand::seq::IndexedRandom;
use rand::seq::SliceRandom;
use std::fmt;
use std::iter;
use std::marker::PhantomData;

const kGameLen: usize = 7;
const kMinLen: usize = 4;

pub struct Game<'a> {
  main: char,
  key: String,
  solutions: Vec::<Vec<&'a str>>,
}
struct Generator {
  main: char,
  q: Vec<char>,
}
struct GeneratorIt<'a> {
  g: &'a Generator,
  p: PermutationIt<'a>,
  i: usize,
}
struct PermutationIt<'a> {
  main: char,
  p: PickIt<'a>,
  a: Vec<char>,
  w: String,
  i: usize,
}
struct PickIt<'a> {
  v: Vec<usize>,
  l: usize,
  f: bool,
  phantom: PhantomData<&'a bool>,
}

impl Generator {
  fn new(main: char) -> Generator {
    let rng = &mut rand::rng();
    let abc = alphabet();
    let mut q = abc.into_iter().filter(|&c| c != main)
                               .collect::<Vec<char>>();
    q.shuffle(rng);
    return Generator {
      main: main,
      q: q,
    };
  }

  fn reset(&mut self, main: char) {
    let rng = &mut rand::rng();
    for c in &mut self.q { if *c == main { *c = self.main; } }
    self.main = main;
    self.q.shuffle(rng);
  }

  fn take_from_q_sorted(&self, i: usize, out: &mut Vec<char>) {
    out.clear();
    out.push(self.main);
    for j in &self.q[i..kGameLen+i-1] { out.push(*j); }
    out.sort();
  }

  fn iter(&self) -> GeneratorIt {
    return GeneratorIt::new(self);
  }
}

impl<'a> GeneratorIt<'a> {
  fn new(g: &Generator) -> GeneratorIt {
    let mut v = Vec::<char>::with_capacity(kGameLen);
    g.take_from_q_sorted(0, &mut v);
    return GeneratorIt {
      g: g,
      p: PermutationIt::new(g.main, v),
      i: 0,
    };
  }
  #[inline(always)]
  fn is_exhausted(&self) -> bool {
    return (self.g.q.len()-self.i) < (kGameLen-1);
  }
  #[inline]
  fn advance(&mut self) -> bool {
    self.i += 1;
    if self.is_exhausted() { return false; }
    self.p.reset(self.g, self.i);
    return true;
  }
}
impl<'a> iter::Iterator for GeneratorIt<'a> {
  type Item = &'a str;
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      if let Some(s) = self.p.next() { return Some(s); }
      if !self.advance() { return None; }
    } // loop
  }
}

impl<'a> PermutationIt<'a> {
  fn new(main: char, a: Vec<char>) -> PermutationIt<'a> {
    return PermutationIt {
      main: main,
      p: PickIt::new(kGameLen),
      a: a,
      w: String::with_capacity(kGameLen*2),
      i: kGameLen,
    };
  }
  #[inline]
  fn reset(&mut self, g: &Generator, i: usize) {
    self.p.reset(kGameLen);
    g.take_from_q_sorted(i, &mut self.a);
    self.i = kGameLen;
  }
}
impl<'a> iter::Iterator for PermutationIt<'a> {
  type Item = &'a str;
  fn next(&mut self) -> Option<Self::Item> {
    loop {
      if self.i < kMinLen { return None; }
      if let Some(r) = self.p.next() {
        let mut has_main = false;
        self.w.clear();
        for &j in r {
          let &c = unsafe { self.a.get_unchecked(j) };
          has_main |= self.main == c;
          self.w.push(c);
        }
        if !has_main { continue; }
        return as_some_eternal(&self.w);
      }
      self.i -= 1;
      self.p.reset(self.i);
    } // loop
  }
}

impl<'a> PickIt<'a> {
  fn new(l: usize) -> PickIt<'a> {
    let mut it = PickIt{
      v:Vec::with_capacity(kGameLen),
      l:l,
      f:true,
      phantom: PhantomData,
    };
    for i in 0..l { it.v.push(i); }
    return it;
  }
  #[inline]
  fn reset(&mut self, l: usize) {
    self.l = l;
    self.f = true;
    self.v.clear();
    for i in 0..l { self.v.push(i); }
  }
}
impl<'a> iter::Iterator for PickIt<'a> {
  type Item = &'a [usize];
  fn next(&mut self) -> Option<Self::Item> {
    if self.f {
      self.f = false;
      return as_some_eternal_slice(&self.v);
    }
    for i in (1..=self.l) {
      let j = self.v[self.l - i];
      if j < kGameLen-i {
        for (m,n) in ((self.l - i)..self.l).enumerate() { self.v[n] = j + m + 1; }
        return as_some_eternal_slice(&self.v);
      }
    }
    return None;
  }
}

impl<'a> Game<'a> {
  fn new(main: char) -> Game<'a> {
    let mut game = Game {
      main: main,
      key: String::with_capacity(kGameLen*2),
      solutions: vec![Vec::new(); kGameLen+1],
    };
    return game;
  }
  #[inline(always)]
  fn get_sol(&mut self, k: usize) -> &mut Vec<&'a str> {
    return unsafe { self.solutions.get_unchecked_mut(k) };
  }
  fn valid_generation(&mut self) -> bool {
    let mut all_non_empty = true;
    for v in &mut self.solutions[kMinLen..=kGameLen] {
      v.sort();
      all_non_empty &= v.len() != 0;
    }
    return all_non_empty;
  }
  fn reset(&mut self, key: &str, v: &Vec<&'a str>) {
    debug_assert!(v.len() > 0);
    self.key.clear();
    self.key.push_str(key);
    for v in &mut self.solutions[kMinLen..=kGameLen] {
      v.clear();
    }
    self.get_sol(kGameLen).extend(v.iter());
  }
  #[inline(always)]
  fn add_sub_key(&mut self, key: &str, v: &Vec<&'a str>) {
    debug_assert!(v.len() > 0);
    self.get_sol(key.len()).extend(v.iter());
  }
}
impl<'a> fmt::Display for Game<'a> {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    writeln!(f, "main = {}", self.main);
    writeln!(f, "key = {}", self.key);
    for (k,v) in self.solutions.iter().enumerate() {
      if v.len() == 0 { continue; }
      writeln!(f, "  {} = {:?}", k, v);
    }
    return Ok(());
  }
}

pub fn solution_for<'a>(main: char, letters: &str, words: &'a anagram::Anagram)
    -> StatusOr<Game<'a>> {
  let mut sorted_letters = letters.chars()
                                  .collect::<Vec<char>>();
  sorted_letters.sort();
  assert_eq!(sorted_letters.len(), kGameLen);
  let mut it = PermutationIt::new(main, sorted_letters);
  let mut game = Game::new(main);

  while let Some(key) = it.next() {
    if key.len() == kGameLen {
      if let Some(v) = words.letters_words.get(key) {
        game.reset(key, v);
      }
    }
    else if let Some(v) = words.letters_words.get(key) {
      game.add_sub_key(key, v);
    }
  }
  if !game.valid_generation() {
    //return Error::new(Code::INTERNAL, "no solution found");
  }
  return Ok(game);
}

fn from_words_once<'a>(conf: &config::Config, words: &'a anagram::Anagram, generator: &Generator)
    -> StatusOr<Game<'a>> {
  let mut game = Game::new(generator.main);
  let mut it = generator.iter();
  let mut w_count = 0;
  let mut skip = false;

  while let Some(key) = it.next() {
    if key.len() == kGameLen {
      if game.valid_generation() { return Ok(game); }

      if let Some(v) = words.letters_words.get(key) {
        w_count = v.len();
        game.reset(key, v);
        if w_count >= conf.min_pangram_count { continue; }
      }
      skip = true;
    }
    else if let Some(v) = words.letters_words.get(key) {
      w_count += v.len();
      game.add_sub_key(key, v);
      skip = w_count > conf.game_word_limit;
    }

    if skip {
      skip = false;
      if it.advance() { continue; }
      else { break; }
    }
  }
  return Error::new(Code::INTERNAL, "could not find enough words");
}

pub fn from_words<'a>(conf: &config::Config, words: &'a anagram::Anagram)
    -> StatusOr<Game<'a>> {
  let rng = &mut rand::rng();
  let main_candidates = words.letters_by_freq()
                             .into_iter()
                             .take(conf.mid_letter_min_rank)
                             .collect::<Vec<char>>();
  let main = *main_candidates.choose(rng).unwrap();
  let mut generator = Generator::new(main);

  for tries in 1..10000 {
    if let Ok(game) = from_words_once(conf, words, &generator) {
      return Ok(game);
    }
    if tries % 1000 == 0 { println!("tries for {main} = {tries}..."); }
    let main = *main_candidates.choose(rng).unwrap();
    generator.reset(main);
  }
  return Error::new(Code::INTERNAL, "exhausted");
}

