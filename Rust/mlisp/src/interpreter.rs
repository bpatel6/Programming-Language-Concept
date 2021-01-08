use crate::lex::{lex};
use crate::parse::{parse};
use crate::eval::{eval, Environment, EvalResult};

/// Lexes, parses, and evaluates the given program.
pub fn run_interpreter(program: &str) -> EvalResult {
    match lex(program) {
        Err(err) => EvalResult::Err(format!("Lex Error: {:?}",err)),
        Ok(tokens) => match parse(&tokens) {
            Err(err) => EvalResult::Err(format!("Parse Error: {:?}",err)),
            Ok(expr) => {
                let mut env = Environment::default();
                match eval(expr.clone(), &mut env) {
                    EvalResult::Err(err) => EvalResult::Err(format!("Eval Error: {:?}",err)),
                    _ => eval(expr.clone(), &mut env)
                }
            }
        }
    }
}
