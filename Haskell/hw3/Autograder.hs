module Autograder where

import Data.List
import Data.Map.Strict as Map
import Text.Printf

data Solution = Student String deriving Show
data QuestionGrade = Pass String | Fail String String

instance Show QuestionGrade where
  show r = case r of
    Pass n -> "[PASS] " ++ n
    Fail n r -> "[FAIL] " ++ n ++ " (reason: " ++ r ++ ")"

qName :: QuestionGrade -> String
qName (Pass n) = n
qName (Fail n _) = n

points :: QuestionGrade -> Integer
points r = case r of
  Pass _ -> 1
  _ -> 0

class HW a where
  name :: a -> String
  everyOtherSol :: a -> [b] -> [b]
  skipTwoSol :: a -> [b] -> [b]
  selectiveMapSol :: a -> (b -> Bool) -> (b -> c) -> [b] -> [c]
  maprSol :: a -> (b -> c) -> [b] -> [c]
  filterrSol :: a -> (b -> Bool) -> [b] -> [b]
  isPrimeSol :: a -> Int -> Bool
  getPrimesSol :: a -> [String] -> [Int]
  toCharFreqsSol :: a -> String -> Map Char Int

data Score = Score Integer Integer

instance Show Score where
  show (Score correct total) = "Total "
    ++ (show correct)
    ++ "/"
    ++ (show total)
    ++ " ("
    ++ (printf "%.2f" $ (fromIntegral correct :: Double) / (fromIntegral total :: Double) * 100)
    ++ "%)"

convertEither :: String -> Either String a -> Either String a
convertEither s (Left _) = Left s
convertEither _ x = x

parse :: [String] -> [Int]
parse [] = []
parse (x:xs) = if x == "-e" then loop xs [] else []
  where loop [] acc = acc
        loop (x:xs) acc = loop xs (acc ++ [read x])

--autograde :: HW2 a => a -> Score
autograde :: HW a => a -> [Int] -> IO ()
autograde s exclusions = do
  putStrLn $ "\nStudent: " ++ (name s)
  let manualResults =
        [ check "everyOther[1/3]" [1] $ everyOtherSol s [1, 2]
        , check "everyOther[2/3]" [1, 3] $ everyOtherSol s [1, 2, 3]
        , check "skipTwo[1/4]" [1] $ skipTwoSol s [1, 2, 3]
        , check "skipTwo[4/4]" [1, 4, 7] $ skipTwoSol s [x | x <- [1..7]]
        , check "selectiveMap[2/3]" [1, 4, 9, 16] $ selectiveMapSol s (\x -> True) (\x -> x*x) [1, 2, 3, 4]
        , check "mapr[2/2]" [1, 4, 9] $ maprSol s (\x -> x*x) [1, 2, 3]
        , check "filterr[2/3]" [1, 3] $ filterrSol s (\x -> x `mod` 2 == 1) [1, 2, 3]
        , check "isPrime[1/4]" True $ isPrimeSol s 5
        , check "isPrime[2/4]" True $ isPrimeSol s 373
        , check "getPrimes[1/4]" [3, 11] $ getPrimesSol s ["3", "4", "9", "11"]
        , check "getPrimes[2/4]" [5, 7] $ getPrimesSol s ["5", "a", "Hello", "7"]
        , check "toCharFreqs[1/3]"
          (fromList [('a', 1), ('b', 1), ('c', 1)] :: Map Char Int)
          $ toCharFreqsSol s "abc"
        ]

  let testCases = Prelude.filter (\(i, _) -> not $ elem i exclusions) $ zip [0..] manualResults
  let excludedCases = Prelude.map (\(_, x) -> (-1, Fail (qName x) "EXCLUDED")) $ Prelude.filter (\(i, _) -> elem i exclusions) $ zip [0..] manualResults

  let sortedResults = sortBy (\(i, _) (j, _) -> compare i j) testCases ++ excludedCases
  let out = concat $ Prelude.map (\(i, x) -> "    " ++ (show x) ++ "\n") sortedResults
  putStrLn out
  let correct = sum $ Prelude.map points manualResults
  let score = Score correct (toInteger $ length manualResults + length exclusions)
  putStrLn $ "  " ++ (show score)

check :: (Eq a, Show a) => String -> a -> a -> QuestionGrade
check name expected actual = if expected == actual
                             then Pass name
                             else Fail name ("Expected: "
                                        ++ (show expected)
                                        ++ " Got: "
                                        ++ (show actual))
