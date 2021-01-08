module Main where

import Autograder
import Data.Maybe
import Debug.Trace
import System.Environment
import Text.Read


{-
 - Note: these types are defined in Autograder.hs and should be used in
 - completing the undefined functions below.
 -
 - data Token = LPar | RPar | Literal String | Plus | Minus | Asterisk | Slash deriving (Eq, Show)
 - data Op = Add | Sub | Mul | Div deriving (Eq, Show)
 - data Expr = Binary Op Expr Expr | Val Double deriving (Eq, Show)
 - data RunError = MismatchedParentheses | LexFailure LexError | ParseFailure ParseError deriving (Eq, Show)
 - newtype LexError = UnknownToken String deriving (Eq, Show)
 - newtype ParseError = SyntaxError String deriving (Eq, Show)
 -}

-- TODO: put your name here
studentName = "Bhavik Patel"

instance HW Solution where
  name a = studentName
  parseSol a = Main.parse
  lexSol a = Main.lex
  validParensSol a = validParens
  evalSol a = eval
  runSol a = run

{-
 - run executes the input expression and returns the result as a string if the
 - given expression is valid. Otherwise, run returns a string error message.
 -}
run :: String -> String
run (x:xs) =if validParens (x:xs) then helper (words (helper2 "()" (x:xs))) [] 0 else "Invalid"
  where
    helper2 = filter . flip notElem
    helper [] _ value = show(value)
    helper (x:xs) operator value 
      | x == "+" && length xs /= 0 && isDouble (xs !! 0) = helper xs ("+") value
      | x == "-" && length xs /= 0 && isDouble (xs !! 0) = helper xs ("-") value
      | x == "*" && length xs /= 0 && isDouble (xs !! 0) = helper xs ("*") value
      | x == "/" && length xs /= 0 && isDouble (xs !! 0) = helper xs ("/") value
      | isDouble x && operator == "+" = helper xs operator (value + (read x :: Double))
      | isDouble x && operator == "-" = helper xs operator (value - (read x :: Double))
      | isDouble x && operator == "*" = helper xs operator (value * (read x :: Double))
      | isDouble x && operator == "/" = helper xs operator (value / (read x :: Double))
      | otherwise = "invalid"
{-
 - evaluates the given expression, which is assumed to be valid.
 -}
eval :: Expr -> Double
eval (Val n) = n
eval (Binary o x y) = case o of
  Add -> (eval x) + (eval y)
  Sub -> (eval x) - (eval y)
  Mul -> (eval x) * (eval y)
  Div -> (eval x) / (eval y)

-- Returns `True` if the string can parse as a double, `False` otherwise.
-- You may find this useful but are not strictly required to use it.
isDouble :: String -> Bool
isDouble x = isJust (readMaybe x :: Maybe Double) 

{-
 - Checks whether the input string contains balanced parentheses.
 -}
validParens :: String -> Bool
validParens (x:xs) = helper (x:xs) 0 == 0
  where 
    helper [] count = count
    helper (x:xs) count
      | count < 0 = count
      | x == '(' = helper xs (count + 1)
      | x == ')' = helper xs (count - 1)
      | otherwise = helper xs count

{-
 - Lexes the input string, returning either a LexError or a list of tokens.
 -}
lex :: String -> Either LexError [Token]
lex [] = Right ([])
lex (x:xs) 
  | validParens (x:xs) = helper2 (words ("( " ++ (helper3 "()" (x:xs)) ++ " )"))
  | otherwise = Left (UnknownToken "Fail")
    where 
      helper3 = filter . flip notElem
      helper2 [] = Right ([])
      helper2 (x:xs)
        | x == "+" = helper Plus
        | x == "-" = helper Minus
        | x == "*" = helper Asterisk
        | x == "/" = helper Slash
        | x == "(" = helper LPar
        | x == ")" = helper RPar
        | isDouble x == False = Left (UnknownToken "Fail")
        | isDouble x = helper (Literal x)
        | otherwise = Left (UnknownToken "Fail")
          where
            helper t =
              case helper2 xs of
                Right tokens -> Right (t : tokens)
                Left (UnknownToken "Fail") -> Left (UnknownToken "Fail")
        
{-
 - Parses the token list and returns either a ParseError or the parsed expression.
 -}
parse :: [Token] -> Either ParseError Expr
parse [] = Left (SyntaxError "Fail")
parse (x:xs) 
  | x == LPar && (last xs) == RPar = helper (tail (init (x:xs)))
  | otherwise = Left (SyntaxError "Fail")
  where 
    helper (x:y:xs) 
      | x == Plus = Right (Binary Add (helper2 y) (helper2 (xs !! 0))) 
      | x == Minus = Right (Binary Sub (helper2 y) (helper2 (xs !! 0)))
      | x == Asterisk = Right (Binary Mul (helper2 y) (helper2 (xs !! 0)))
      | x == Slash = Right (Binary Div (helper2 y) (helper2 (xs !! 0)))
      | otherwise = Left (SyntaxError "Fail")
      where
        helper2 (Literal n) = (Val (read n :: Double))

main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = Autograder.parse args
  autograde s exclusions
