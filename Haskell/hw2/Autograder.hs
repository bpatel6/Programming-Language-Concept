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

class HW2 a where
  name :: a -> String
  perfectsSol :: Integral b => a -> b -> [b]
  fizzbuzzSol :: (Integral b, Show b) => a -> b -> [String]
  areAnagramsSol :: a -> String -> String -> Bool
  balancedParensSol :: a -> String -> Bool
  balancedBracketsSol :: a -> String -> Bool
  maybeHeadSol :: a -> [b] -> Maybe b
  maybeTailSol :: a -> [b] -> Maybe [b]
  eitherHeadSol :: a -> [b] -> Either String b
  eitherTailSol :: a -> [b] -> Either String [b]

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

--autograde :: HW2 a => a -> Score
autograde :: HW2 a => a -> IO ()
autograde s = do
  putStrLn $ "\nStudent: " ++ (name s)
  let manualResults = [ check "fizzbuzz[1/3]" ["1", "2"] $ fizzbuzzSol s 2
                      , check "fizzbuzz[2/3]" ["1", "2", "fizz", "4", "buzz"] $ fizzbuzzSol s 5
                      , check "perfects[2/4]" [6] $ perfectsSol s 10
                      , check "areAnagrams[2/2]" False $ areAnagramsSol s "hello" "helo"
                      , check "balancedParens[1/4]" True $ balancedParensSol s "()"
                      , check "balanceParens[4/4]" False $ balancedParensSol s ")("
                      , check "balancedBrackets[1/5]" True $ balancedBracketsSol s "()[]{}"
                      , check "balancedBrackets[3/5]" False $ balancedBracketsSol s "([[])"
                      , check "maybeHead[1/2]" (Nothing :: Maybe Int) $ maybeHeadSol s []
                      , check "maybeHead[2/2]" (Just 5 :: Maybe Int) $ maybeHeadSol s [5]
                      , check "maybeTail[1/2]" (Nothing :: Maybe [Int]) $ maybeTailSol s []
                      , check "maybeTail[2/2]" (Just [] :: Maybe [Int]) $ maybeTailSol s [5]
                      , check "eitherHead[1/2]" (Left "empty" :: Either String Int) $ convertEither "empty" $ eitherHeadSol s []
                      , check "eitherHead[2/2]" (Right 5 :: Either String Int) $ eitherHeadSol s [5]
                      , check "eitherTail[1/2]" (Left "empty" :: Either String [Int]) $ convertEither "empty" $ eitherTailSol s []
                      , check "eitherTail[2/2]" (Right [] :: Either String [Int]) $ eitherTailSol s [5]
                      ]
  let out = concat $ map (\x -> "    " ++ (show x) ++ "\n") manualResults
  putStrLn out
  let correct = sum $ map points manualResults
  let score = Score correct (toInteger $ length manualResults)
  putStrLn $ "  " ++ (show score)

check :: (Eq a, Show a) => String -> a -> a -> QuestionGrade
check name expected actual = if expected == actual
                             then Pass name
                             else Fail name ("Expected: "
                                        ++ (show expected)
                                        ++ " Got: "
                                        ++ (show actual))
