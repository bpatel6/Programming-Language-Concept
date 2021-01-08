module Autograder where

import Text.Printf

data Solution = Student String deriving Show
data QuestionGrade = Pass String | Fail String String

instance Show QuestionGrade where
  show r = case r of
    Pass n -> "[PASS] " ++ n
    Fail n r -> "[FAIL] " ++ n ++ " (reason: " ++ r ++ ")"

points :: QuestionGrade -> Integer
points r = case r of
  Pass _ -> 1
  _ -> 0

class HW1 a where
  name :: a -> String
  powSol :: Fractional b => a -> b -> Integer -> b
  factorialSol :: a -> Integer -> Integer
  maxSubarraySumSol :: a -> [Integer] -> Integer
  reverseStringSol :: a -> String -> String
  isPalindromeSol :: a -> String -> Bool

data Score = Score Integer Integer

instance Show Score where
  show (Score correct total) = "Total "
    ++ (show correct)
    ++ "/"
    ++ (show total)
    ++ " ("
    ++ (printf "%.2f" $ (fromIntegral correct :: Double) / (fromIntegral total :: Double) * 100)
    ++ "%)"

--autograde :: HW1 a => a -> Score
autograde :: HW1 a => a -> IO ()
autograde s = do
  putStrLn $ "\nStudent: " ++ (name s)
  let manualResults = [ check "pow[1/3]" 1 $ powSol s 2 0
                      , check "pow[2/3]" 4 $ powSol s 2 2
                      --, check "pow[3/3]" 8 $ powSol s (1/2) (-3)
                      , check "factorial[1/3]" 1 $ factorialSol s 0
                      , check "factorial[2/3]" 1 $ factorialSol s 1
                      , check "factorial[3/3]" 120 $ factorialSol s 5
                      , check "maxSubarraySum[1/5]" 1 $ maxSubarraySumSol s [1]
                      , check "maxSubarraySum[2/5]" 15 $ maxSubarraySumSol s [1, 2, 3, 4, 5]
                      , check "maxSubarraySum[3/5]" 9 $ maxSubarraySumSol s [1, 2, (-10), 4, 5]
                      , check "maxSubarraySum[4/5]" 10 $ maxSubarraySumSol s [(-1), (-2), (-4), 10]
                      , check "maxSubarraySum[5/5]" 5 $ maxSubarraySumSol s [2, 3, -10, 3, -2]
                      , check "reverseString[1/3]" "ba" $ reverseStringSol s "ab"
                      , check "reverseString[2/3]" "cba" $ reverseStringSol s "abc"
                      , check "reverseString[3/3]" "" $ reverseStringSol s ""
                      , check "isPalindrome[1/3]" True $ isPalindromeSol s "abba"
                      , check "isPalindrome[2/3]" True $ isPalindromeSol s "aba"
                      , check "isPalindrome[3/3]" False $ isPalindromeSol s "abca"
                      ]
  let out = concat $ map (\x -> "    " ++ (show x) ++ "\n") manualResults
  putStrLn out
  let correct = sum $ map points manualResults
  let score = Score correct (toInteger $ length manualResults)
  putStrLn $ "  " ++ (show score)

check :: (Eq a, Show a) => String -> a -> a -> QuestionGrade
check name expected actual = if expected == actual then
                          Pass name
                        else Fail name ("Expected: "
                                   ++ (show expected)
                                   ++ " Got: "
                                   ++ (show actual))
