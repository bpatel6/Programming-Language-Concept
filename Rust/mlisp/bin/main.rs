use std::env;
use std::fs;
use mlisp::interpreter::run_interpreter;
use mlisp::eval::EvalResult;


fn main() {
    let args: Vec<String> = env::args().collect();
    assert!(args.len() > 1, "Must pass an argument");
    let contents = fs::read_to_string(&args[1]).expect("Something went wrong reading the file");
    match run_interpreter(&*contents) {
        EvalResult::Err(err) => println!("{:?}", err),
        _ => {}
    }
}
