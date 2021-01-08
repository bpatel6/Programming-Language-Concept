{-# LANGUAGE EmptyCase #-}
module Main where

import Autograder
import Data.Map.Strict as Map
import System.Environment
import Text.Read




{-
HW3 INSTRUCTIONS

1. Update the instance HW1 Solution name function to return your name
   (as it appears in ICON).
2. For each problem below, delete `undefined` and replace with your solution.

If you need a helper function (hint: you probably will for a few of these),
make sure it is limited to the context in which it is used. That is, do not
create helper functions that are globally visible.
-}

-- TODO: Put your name here
studentName = "Bhavik Patel"

instance HW Solution where
  name a = studentName

  -- You should not need to modify these
  everyOtherSol a = everyOther
  skipTwoSol a = skipTwo
  selectiveMapSol a = selectiveMap
  maprSol a = mapr
  filterrSol a = filterr
  isPrimeSol a = isPrime
  getPrimesSol a = getPrimes
  toCharFreqsSol a = toCharFreqs

{-
  P1
  Create a function that takes a list of elements and returns a list such that
  every other element has been removed (i.e., elements whose 0-based index is odd).

  Ex 1) [x1, x2, x3, x4, x5, x6] becomes [x1, x3, x5]
-}
everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x:[]) = [x]
everyOther (x:y:xs) = x:everyOther xs

{-
  P2
  Create a function that takes a list of elements and returns a list such that
  starting with the first element, the following two elements are skipped, the
  fourth is kept, the following two are skipped, etc.

  Ex 1) [x1, x2, x3] becomes [x1]
  Ex 2) [x1, x2, x3, x4, x5, x6, x7] becomes [x1, x4, x7]
-}
skipTwo :: [a] -> [a]
skipTwo [] = []
skipTwo (x:[]) = [x]
skipTwo (x:y:z:xs) = x:skipTwo xs

{-
  P3
  Problem 7.1 from Hutton. Implement a function that mimics the behavior of
  the list comprehension [f x | x <- xs, p x] using the functions map and filter.
-}
selectiveMap :: (a -> Bool) -> (a -> b) -> [a] -> [b]
selectiveMap p f [] = []
selectiveMap p f xs = Prelude.map f (Prelude.filter p xs)

{-
  P4
  Problem 7.3 from the book. Implement the function map using foldr.
-}
mapr :: (a -> b) -> [a] -> [b]
mapr f = Prelude.foldr (\x xs -> f x : xs) []

{-
  P5
  Problem 7.3 from the book. Implement the function filter using foldr.
-}
filterr :: (a -> Bool) -> [a] -> [a]
filterr p = Prelude.foldr (\x xs -> if p x then x:xs else xs) []

{-
  P6
  Create a function that determines if the given number is prime.
-}
isPrime :: Int -> Bool
isPrime x 
  | x <= 1 = False
  | x == 2 = True
  | length [i | i <- [2..x-1], x `mod` i == 0] > 0 = False
  | otherwise = True

{-
  P7
  Given a list of strings, each of which may or may not represent an integer,
  return a list of Ints containing only the strings representing prime integers.

  Hint: you may find the function readMaybe helpful.

  Ex 1) ["3", "4", "9", "11"] -> [3, 11]
  Ex 2 ["5", "a", "Hello", "7"] -> [5, 7]
-}
getPrimes :: [String] -> [Int]
getPrimes [] = []
getPrimes x = helper x list 
  where 
    list = []
    helper [] list = list
    helper (x:xs) list =
      case readMaybe x of
        Just x -> if isPrime x == True then helper xs (list ++ [x]) else helper xs list
        Nothing -> helper xs list
        
{-
  P8
  Return the character frequencies for the given string using either foldr or foldl.
-}
toCharFreqs :: String -> Map Char Int
toCharFreqs = Prelude.foldr(Map.alter helper) Map.empty
  where 
    helper (Just x) = Just (x + 1)
    helper _ = Just 1

--toCharFreqs (x:xs) = Map.filter(\x -> Prelude.foldl (\a b -> if b == x then (a+1) else a) 0 xs) nub xs
  
-- Prelude.map(\x -> (x, Prelude.foldl (\a b -> if b == x then (a+1) else a) 0 xs))

-- Note: you can now exclude test cases by running your program as follows:
--  ./hw3 -e 3 6
-- The above will exclude test cases 3 and 6
main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = parse args
  autograde s exclusions
