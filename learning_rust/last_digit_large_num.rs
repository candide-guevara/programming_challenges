const MAX_BASE:i32 = 10;

fn compute_possible(base: i32, head: i32) -> Vec<i32> {
  let candidates = (1..base).map(|x| head.pow(x as u32) % base)
                            .collect::<Vec<i32>>();
  for (i,c) in candidates.iter().enumerate() {
    if let Some(_) = candidates[0..i].iter().find(|&x| x == c) {
      return candidates[0..i].to_vec();
    }
  }
  assert!(false, "we should not reach this point");
  return vec![];
}

fn last_digit_helper(base: i32, power_tower: &[i32]) -> i32 {
  assert!(base <= MAX_BASE);
  assert!(power_tower[0] > 0);
  let head = power_tower[0] % base;
  if power_tower.len() == 1 { return head; }
  if head < 2 { return head; }

  let possible_digits = compute_possible(base, head);
  //println!("head = {}, base = {}, possible_digits = {:?}, remaining = {:?}",
  //         head, base, possible_digits, &power_tower[1..]);
  if possible_digits.len() == 1 { return possible_digits[0]; }
  if *possible_digits.last().unwrap() == 0 {
    assert!(possible_digits.len() == 2);
    return possible_digits[if power_tower[1] > 1 { 1 } else { 0 }];
  }
  let new_base = possible_digits.len() as i32;
  let power_last = last_digit_helper(new_base, &power_tower[1..]);
  let idx_shifted_down = ((power_last + new_base - 1) % new_base) as usize;
  return possible_digits[idx_shifted_down];
}

fn last_digit(power_tower: &Vec<i32>) -> i32 {
  assert!(power_tower.len() > 0);
  return last_digit_helper(10, power_tower);
}

fn main() {
  let last_digit_expect = move |power_tower, expect| {
    let digit = last_digit(&power_tower);
    assert_eq!(digit, expect);
    println!("power_tower = {:?}, last_digit = {}", power_tower, digit);
  };

  last_digit_expect(vec![2, 4, 5], 6);
  last_digit_expect(vec![3, 4, 2], 1);
  last_digit_expect(vec![3, 3], 7);
  last_digit_expect(vec![3, 3, 3], 7);
  last_digit_expect(vec![3, 3, 3, 3], 7);
  last_digit_expect(vec![2, 2, 2, 2], 6);
  last_digit_expect(vec![2, 2, 1], 4);
  last_digit_expect(vec![2, 2, 1, 1], 4);
  last_digit_expect(vec![5, 3, 7, 6], 5);
  last_digit_expect(vec![9, 2, 7, 6], 1);
  last_digit_expect(vec![7, 3, 2, 2], 7);
}

