use super::*;

#[test]
fn pick_it_1() {
  let mut it = PickIt::new(1);
  for (i,range) in it.enumerate() {
    assert_eq!(range.len(), 1);
    let j = range.get(0).unwrap();
    assert_eq!(*j, i);
  }
}

#[test]
fn pick_it_3() {
  let mut it = PickIt::new(3);
  let mut expect = Vec::<Vec<usize>>::new();
  for i in 0..kGameLen-2 {
    for j in i..kGameLen-2 {
      for k in j..kGameLen-2 { expect.push(vec![i,j+1,k+2]); }
    }
  }
  for (i,range) in it.enumerate() {
    let v = Vec::from(range);
    assert_eq!(v, expect[i]);
  }
}

#[test]
fn pick_it_max() {
  let mut it = PickIt::new(kGameLen);
  let v = Vec::from(it.next().unwrap());
  assert_eq!(v, Vec::from_iter(0..kGameLen));
}

fn perm_count() -> usize {
  let mut perm_count = 1;
  for i in 1..=kGameLen-kMinLen {
    let mut x = 1;
    let mut y = 1;
    for j in 1..=i { x *= (kGameLen-j); y *= j; }
    perm_count += x/y;
  }
  return perm_count;
}

#[test]
fn permutation_it() {
  let letters = "abcdefg";
  assert_eq!(letters.len(), kGameLen);
  let mut it = PermutationIt::new('a', Vec::from_iter(letters.chars()));
  assert_eq!(it.next().unwrap(), letters);

  for i in (1..kGameLen).rev() {
    let mut expect = String::from(letters);
    expect.remove(i);
    assert_eq!(it.next().unwrap(), expect);
  }
  assert_eq!(it.next().unwrap(), &letters[0..kGameLen-2]);

  let mut last: String = "".to_string();
  let mut count = 1 + kGameLen;
  while let Some(s) = it.next() {
    count += 1;
    last = String::from(s);
  }

  let mut expect = String::from("a");
  expect.push_str(&letters[kGameLen-kMinLen+1..kGameLen]);
  let expect_count = perm_count();

  assert_eq!(last, expect);
  assert_eq!(count, expect_count);
}

fn advance(it: &mut GeneratorIt, i: usize) -> Option<String> {
  let mut count = 0;
  while let Some(s) = it.next() {
    count += 1;
    if count >= i { return Some(String::from(s)); }
  }
  assert_eq!(i, count);
  return None;
}

#[test]
fn generator_it() {
  let letters = "gfazbhde";
  let perm_count = perm_count();
  assert_eq!(letters.len(), kGameLen+1);

  let g = Generator {
    main: 'c',
    q: Vec::from_iter(letters.chars()),
  };
  let mut it = GeneratorIt::new(&g);
  assert_eq!(it.next().unwrap(), "abcfghz");
  
  let cur_value = advance(&mut it, perm_count-2).unwrap();
  assert_eq!(it.next().unwrap(), "cghz");
  assert_eq!(it.next().unwrap(), "abcdfhz");

  let cur_value = advance(&mut it, perm_count-2).unwrap();
  assert_eq!(it.next().unwrap(), "cfhz");
  assert_eq!(it.next().unwrap(), "abcdehz");

  let cur_value = advance(&mut it, perm_count-2).unwrap();
  assert_eq!(it.next().unwrap(), "cehz");
  assert_eq!(it.next(), None);
  assert_eq!(it.next(), None);
}

#[test]
fn generator_advance() {
  let letters = "gfazbhde";
  assert_eq!(letters.len(), kGameLen+1);

  let g = Generator {
    main: 'c',
    q: Vec::from_iter(letters.chars()),
  };
  let mut it = GeneratorIt::new(&g);
  assert_eq!(it.next().unwrap(), "abcfghz");
  
  assert_eq!(it.advance(), true);
  assert_eq!(it.next().unwrap(), "abcdfhz");
  assert_eq!(it.next().unwrap(), "abcdfh");

  assert_eq!(it.advance(), true);
  assert_eq!(it.next().unwrap(), "abcdehz");
  assert_eq!(it.next().unwrap(), "abcdeh");

  assert_eq!(it.advance(), false);
  assert_eq!(it.advance(), false);
  // assert_eq!(it.next(), None); // BOOM!
}

