module Main where

import Autograder


{-
INSTRUCTIONS

1. Update the instance HW1 Solution name function to return your name
   (as it appears in ICON).
2. For each problem below, delete `undefined` and replace with your solution.

If you need a helper function (hint: you probably will for a few of these),
make sure it is limited to the context in which it is used. That is, do not
create helper functions that are globally visible.
-}

instance HW1 Solution where
  -- TODO: Add your name here
  name a = "Bhavik Patel"

  -- You should not need to modify these
  powSol a x y = pow x y
  factorialSol a n = factorial n
  maxSubarraySumSol a x = maxSubarraySum x
  reverseStringSol a s = reverseString s
  isPalindromeSol a x = isPalindrome x

{-
Problem 1

Define a function that raises its input to the specified power. For example:
  pow 2 2 = 4
  pow 1.5 2 = 2.25
Do this in O(lg n) time.
-}
pow :: Fractional a => a -> Integer -> a
-- if passed value 0 return 1
-- else call function recursively 
pow x y = if y == 0 then 1 else x * (pow x (y-1))

{-
Problem 2

Compute n!
-}
factorial :: Integer -> Integer
-- if passed value is 0 return 1
-- else call function recursively 
factorial n = if n == 0 then 1 else n * factorial (n-1)


{-
Problem 3

Given a list of integers, determine the subarray sum, where a subarray is
defined as a contiguous block of values within the given list. The subarray
may be the list itself.

Examples:
maxSubarraySum [1, 2, 3] = 6
maxSubArraySum [10, 3, -20, 8] = 13
-}
maxSubarraySum :: [Integer] -> Integer
maxSubarraySum (x:xs) = helper currentMax lastMax (x:xs)
-- helper function to store the value of currentMax and lastMax(to compare) and pass in the array
-- call recursively untill the list is empty
-- once empty return currentMax value
  where 
    currentMax = 0
    lastMax = 0
    helper currentMax lastMax [] = currentMax
    helper currentMax lastMax (x:xs) = helper currentMax' lastMax' xs
      where 
        lastMax' = if x > (lastMax + x) then x else (lastMax + x) 
        currentMax' = if currentMax > lastMax' then currentMax else lastMax'


{-
Problem 4

Given an input string, return the string reversed. Do not use any library
functions.
-}
reverseString :: String -> String
-- if length is 0 then return string
-- pass tail of the string recursively and keep adding head-the first character in concatination 
reverseString x = if length x == 0 then x else reverseString (tail x) ++ [head x]


{-
Problem 5

Determine if the input is a palindrome. You may only use other functions defined
within this homework.
-}
isPalindrome :: Show a => a -> Bool
isPalindrome x = show x == reverseString(show x)
 

main = do
  let s = Student "Bhavik Patel"
  autograde s
