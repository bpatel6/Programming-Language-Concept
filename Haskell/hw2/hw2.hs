module Main where

import Autograder
import Data.Map.Strict as Map

{- 
HW2 INSTRUCTIONS

1. Update the instance HW1 Solution name function to return your name
   (as it appears in ICON).
2. For each problem below, delete `undefined` and replace with your solution.

If you need a helper function (hint: you probably will for a few of these),
make sure it is limited to the context in which it is used. That is, do not
create helper functions that are globally visible.
-}

instance HW2 Solution where
  -- TODO: Add your name here
  name a = "Bhavik Patel"

  -- You should not need to modify these
  perfectsSol a x = perfects x
  fizzbuzzSol a n = fizzbuzz n
  areAnagramsSol a s1 s2 = areAnagrams s1 s2
  balancedParensSol a s = balancedParens s
  balancedBracketsSol a s = balancedBrackets s
  maybeHeadSol a xs = maybeHead xs
  maybeTailSol a xs = maybeTail xs
  eitherHeadSol a xs = eitherHead xs
  eitherTailSol a xs = eitherTail xs


{-
 (P1)
 Problem 5.6 - define the function perfects that returns all perfect numbers
 up to and including a given limit.
 
 A number is "perfect" if it is equal to the sum of its factors.
 
 Ex. 1) factors 6 = [1, 2, 3], sum [1, 2, 3] = 6 therefore 6 is perfect
 Ex. 2) factors 28 = [1, 2, 4, 7, 14], sum [1, 2, 4, 7, 14] = 28 therefore 28 is perfect
 Ex. 3) factors 8 = [1, 2, 4], sum [1, 2, 4] = 7 therefore 8 is _not_ perfect
 
 Example evaluation of this function (from the book)
 perfects 500 = [6, 28, 496]
-}
perfects :: Integral b => b -> [b]
perfects 0 = []
perfects n = [x | x <- [1..n], factors x == x] -- finds the perfect numbers
  where factors m = sum[x | x <- [1..m-1], m `mod` x == 0] -- compute the factor of n numbers

{-
 (P2)
 Fizzbuzz is a classic programming challenge were you must convert
 a sequence of integers to either the integer itself (as a string), the string
 "fizz" if the integer is divisible by 3, the string "buzz" if the
 string is divisible by 5, and "fizzbuzz" if the string is divisible
 by both 3 and 5. Implement fizzbuzz below, using an array
 comprehension on the sequence of integers 1 up to and including the
 given integer.
-}
fizzbuzz :: (Integral b, Show b) => b -> [String]
fizzbuzz n = [if (x `mod` 15 == 0) then "fizzbuzz"  -- if number is divisible by both 3 and 5 = fizzbuzz
              else if (x `mod` 5 == 0) then "buzz"  -- if number is divisible by 5 = buzz
              else if (x `mod` 3 == 0) then "fizz"  -- if number is divisible by 3 = fizz
              else show x | x <- [1..n]]            -- if not divisible by any just the n


{-
(P3)
Determine if two strings are anagrams. Hint: use Haskell's Data.Map.Strict data structure
-}
areAnagrams :: String -> String -> Bool
areAnagrams [] [] = True  -- both strings are empty
areAnagrams [] y = False  -- one string is empty
areAnagrams x [] = False  -- one string is empty
areAnagrams x y = False

-- I tried but it's too much without knowing anything about maps in haskell :(
{-areAnagrams x y = size(helper x y m) == 0
  where 
    m = Map.empty
    helper (x:xs) (y:ys) m = m
    helper (x:xs) (y:ys) m = Map.insert x 1 m  -}

  

  
  

{-
 (P4)
 Given a string of parentheses, determine if they are balanced.
     ()() -> balanced (True)
     ((()())) -> balanced (True)
     )( -> unbalanced (False)
     ())() -> unbalanced (False)
-}
balancedParens :: String -> Bool
balancedParens (x:xs) = helper (x:xs) count == 0
  where 
    count = 0
    helper [] count = count
    helper (x:xs) count = if count < 0 then count -- if close parens is found before open parens
                          else if x == '(' then helper xs (count + 1) -- open parens found increment 1
                          else if x == ')' then helper xs (count - 1) -- close parens found decrement 1
                          else count -- else just return count
                
    
    
                    

{-
 (P5)
 Similar to [[balancedParens]], but this time the input can contain parentheses,
 curly brakcets, and square brackets.
     ()[]{} -> balanced (True)
     ({}[]) -> balanced (True)
     ([)] -> unbalanced (False)
     ([[]) -> unbalanced (False)
-}
balancedBrackets :: String -> Bool
balancedBrackets (x:xs) = if (length (helper (x:xs) list) == 0) then True else False
  where
    list = []
    helper [] list = list
    helper (x:xs) list 
      | (x == '(') || (x == '{') || (x == '[') = helper xs (x : list)                            -- add open brackets to the empty list
      | length list == 0 = (x: list)
      | length list /= 0 = if x == ')' && (head list) == '(' then helper xs (tail list)          -- check if closing bracket is matched with the open bracket in list
                           else if x == ']' && (head list) == '[' then helper xs (tail list)
                           else if x == '}' && (head list) == '{' then helper xs (tail list)
                           else list                                                             -- if close bracket is not matched with the first bracket in list then return list
      | otherwise = list                                                                         -- otherwise return the list

{-
 (P6)
 Implement the function maybeHead so that it returns a Maybe containing
 either the head of the list if it exists, otherwise Nothing.
-}
maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:xs) = Just x

{-
 (P7)
 Implement the function maybeTail so that it returns a Maybe containing
 either the head of the list if it exists, otherwise Nothing.
-}
maybeTail :: [a] -> Maybe [a]
maybeTail [] = Nothing
maybeTail (x:xs) = Just xs

{-
 (P8)
 Implement eitherHead so that it returns an Either containing the
 head of the list if it exists, otherwise a string describing the error.
-}
eitherHead :: [a] -> Either String a
eitherHead [] = Left "String is empty or no head"
eitherHead (x:xs) = Right x

{-
 (P9)
 Implement eitherTail so that it returns an Either containing the
 tail of the list if it exists, otherwise a string describing the error.
-}
eitherTail :: [a] -> Either String [a]
eitherTail [] = Left "String is empty or no tail"
eitherTail (x:xs) = Right xs

main = do
  let s = Student "Bhavik Patel"
  autograde s
