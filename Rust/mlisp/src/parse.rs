use crate::lex::Token;
use crate::types::Expr;
use std::rc::Rc;
use crate::parse::ParseError::EOF;

#[derive(Debug)]
pub enum ParseError {
    BadParse(String),
    EOF,
}

#[derive(Debug)]
pub enum ParseResult {
    Success(usize, Rc<Expr>),
    Failure(ParseError),
}

pub fn parser(tokens: &[Token], index: usize) -> ParseResult {
    let size = tokens.len();
    let mut index = index;
    if let Some(mut t) = tokens.get(index) {
        match &*t {
            Token::LPar => {
                index += 1;
                let mut exprs = Vec::new();
                while *t != Token::RPar {
                    match parser(tokens, index) {
                        ParseResult::Success(idx, expr) => {
                            exprs.push(expr);
                            index = idx;
                        },
                        e => return e,
                    }
                    if index >= size {
                        ParseResult::Failure(EOF);
                    } else {
                        t = &tokens[index];
                    }

                }
                ParseResult::Success(index + 1, Expr::list(&exprs))
            },
            Token::RPar => {
                ParseResult::Failure(ParseError::BadParse("Unexpected ) encountered".to_string()))
            },
            Token::Literal(s) => {
                if let Ok(n) = s.parse::<f64>() {
                    ParseResult::Success(index + 1, Expr::fnum(n))
                }else {
                    ParseResult::Success(index + 1, Expr::symbol(&s))
                }
            },
        }} else {
            ParseResult::Failure(ParseError::EOF)
        }

}

pub fn parse(tokens: &[Token]) -> Result<Rc<Expr>, ParseError> {
    match parser(tokens, 0){
        ParseResult::Success(_, expr) => Ok(expr),
        ParseResult::Failure(err) => Err(err),
    }
}

#[cfg(test)]
mod test {
    use super::*;
}
