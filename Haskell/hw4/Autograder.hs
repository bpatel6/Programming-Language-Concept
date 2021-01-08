module Autograder where

import Text.Printf
import Data.Char
import Data.Map.Strict as Map
import Data.List

data TreeNode k v = Node (TreeNode k v) k v (TreeNode k v) | Nil deriving (Eq, Show)

newtype Solution = Student String deriving Show
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
  treeInsertSol :: (Eq k, Ord k) => a -> k -> v -> TreeNode k v -> TreeNode k v
  treeLookupSol :: (Eq k, Ord k) => a -> k -> TreeNode k v -> Maybe v
  treeFromListSol :: (Eq k, Ord k) => a -> [(k, v)] -> TreeNode k v
  treePreorderFoldSol :: (Eq k, Ord k, Show k, Show v) => a -> (k -> v -> b -> b) -> b -> TreeNode k v -> b
  treeInorderFoldSol :: (Eq k, Ord k, Show k, Show v) => a -> (k -> v -> b -> b) -> b -> TreeNode k v -> b
  treePostorderFoldSol :: (Eq k, Ord k, Show k, Show v) => a -> (k -> v -> b -> b) -> b -> TreeNode k v -> b
  treeMapSol :: a -> (v -> v') -> TreeNode k v -> TreeNode k v'
  hasPathSumSol :: a -> TreeNode Int Int -> Int -> Bool

data Score = Score Integer Integer

instance Show Score where
  show (Score correct total) = "Total "
    ++ show correct
    ++ "/"
    ++ show total
    ++ " ("
    ++ printf "%.2f" ((fromIntegral correct :: Double) / (fromIntegral total :: Double) * 100)
    ++ "%)"

parse :: [String] -> [Int]
parse [] = []
parse (x:xs) = if x == "-e" then loop xs [] else []
  where loop [] acc = acc
        loop (x:xs) acc = loop xs (acc ++ [read x])

autograde :: HW a => a -> [Int] -> IO ()
autograde s exclusions = do
  putStrLn $ "\nExclusions: " ++ show exclusions
  putStrLn $ "Student: " ++ name s
  let st1 = Node (Node Nil 1 "one" Nil) 2 "two" (Node Nil 3 "three" Nil)
  let st2 = Node (Node Nil 5 "five" Nil) 6 "six" (Node Nil 7 "seven" Nil)
  let foldF k v acc = acc ++ "[" ++ show k ++ ":" ++ show v ++ "]"
  let manualResults = [check "treeInsert[2/3]"
                          (Node Nil 1 "one" (Node Nil 2 "two" Nil))
                          $ treeInsertSol s 2 "two" (Node Nil 1 "one" Nil)
                      , check "treeLookup[3/4]" (Just "one") $ treeLookupSol s 1 (Node Nil 1 "one" Nil)
                      , check "treeFromList[3/3]" (Node Nil 1 "one" Nil) $ treeFromListSol s [(1, "one")]
                      , check "treePreorderFold[2/3]" "[2:\"two\"][1:\"one\"][3:\"three\"]"
                          $ treePreorderFoldSol s foldF "" st1
                      , check "treePostorderFold[2/3]" "[1:\"one\"][3:\"three\"][2:\"two\"]"
                          $ treePostorderFoldSol s foldF "" st1
                      , check "treeInorderFold[2/3]" "[1:\"one\"][2:\"two\"][3:\"three\"]"
                          $ treeInorderFoldSol s foldF "" st1
                      , check "treeMap[2/2]" (Node (Node Nil 1 "ONE" Nil) 2 "TWO" (Node Nil 3 "THREE" Nil))
                          $ treeMapSol s (Prelude.map toUpper) st1
                      , check "hasPathSum[1/5]" False $ hasPathSumSol s (Node Nil 5 10 Nil) 3
                      , check "hasPathSum[1/5]" True $ hasPathSumSol s (Node (Node Nil 5 3 Nil) 6 7 (Node (Node Nil 8 9 Nil) 7 8 Nil)) 24
                      ]

  let testCases = Prelude.filter (\(i, _) -> i `notElem` exclusions) $ zip [0..] manualResults
  let excludedCases =
        Prelude.map (\(_, x) -> (-1, Fail (qName x) "EXCLUDED"))
        $ Prelude.filter (\(i, _) -> i `elem` exclusions)
        $ zip [0..] manualResults

  let sortedResults = sortBy (\(i, _) (j, _) -> compare i j) testCases ++ excludedCases
  let out = Prelude.concatMap (\(i, x) -> "    " ++ show x ++ "\n") sortedResults
  putStrLn out
  let correct = sum $ Prelude.map points manualResults
  let score = Score correct (toInteger $ length manualResults + length exclusions)
  putStrLn $ "  " ++ show score



check :: (Eq a, Show a) => String -> a -> a -> QuestionGrade
check name expected actual = if expected == actual
                             then Pass name
                             else Fail name ("Expected: "
                                        ++ show expected
                                        ++ " Got: "
                                        ++ show actual)
