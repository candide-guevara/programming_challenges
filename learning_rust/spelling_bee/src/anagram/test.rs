use super::*;

#[test]
fn remove_plurals_works() {
  let words = vec![
    "oar".to_string(),
    "something".to_string(),
    "oars".to_string(),
    "things".to_string(),
    "another".to_string(),
    "thing".to_string(),
    "stuff".to_string(),
  ];
  let expect = vec![
    "oar".to_string(),
    "something".to_string(),
    "another".to_string(),
    "thing".to_string(),
    "stuff".to_string(),
  ];
  let no_plural = remove_plurals(words.clone());
  assert_eq!(no_plural.unwrap(), expect);
}

