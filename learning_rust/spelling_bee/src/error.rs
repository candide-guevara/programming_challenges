use regex;
use std::convert::From;
use std::io;

#[derive(Debug)]
pub enum Code {
  OK,
  INVALID,
  INTERNAL,
  PRECONDITION,
}

#[derive(Debug)]
pub struct Error {
  code: Code,
  msg: String,
}

impl Error {
  pub fn new<T>(c: Code, msg: &str) -> Result<T, Error> {
    return Err(Error{
      code: c,
      msg: String::from(msg),
    });
  }
}

pub type Status = Result<(), Error>;
pub type StatusOr<T> = Result<T, Error>;

impl From<io::Error> for Error {
  fn from(e: io::Error) -> Self {
    return Error {
      code: Code::PRECONDITION,
      msg: format!("io:Error {}", e.kind()),
    };
  }
}

impl From<regex::Error> for Error {
  fn from(e: regex::Error) -> Self {
    return Error {
      code: Code::INVALID,
      msg: format!("regex:Error {}", e),
    };
  }
}

