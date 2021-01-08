module Main where

import Autograder
import Data.Map.Strict as Map
import System.Environment
import Text.Read

-- NOTE: imported from Autograder (line 6):
-- data TreeNode k v =  Node (TreeNode k v) k v (TreeNode k v) | Nil deriving (Eq, Show)

{-
HW4 INSTRUCTIONS

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
  -- You should not need to modify these
  name a = studentName
  treeInsertSol a = treeInsert
  treeLookupSol a = treeLookup
  treeFromListSol a = treeFromList
  treePreorderFoldSol a = treePreorderFold
  treeInorderFoldSol a = treeInorderFold
  treePostorderFoldSol a = treePostorderFold
  treeMapSol a = treeMap
  hasPathSumSol a = hasPathSum


{-
 - P1
 - Given a key-value pair and a TreeNode, return a new TreeNode (the root of the tree)
 - such that the tree now contains the given key-value pair after inserting them in the
 - correct position.
 -}
treeInsert :: (Ord k, Eq k) =>  k -> v -> TreeNode k v -> TreeNode k v
treeInsert k v Nil = Node Nil k v Nil
treeInsert k v (Node left a b right) 
  | k == a = Node left k v right 
  | k < a = Node (treeInsert k v left) a b right 
  | k > a = Node left a b (treeInsert k v right)

{-
 - P2
 - Lookup the value for the given key, returning it as a Maybe type.
 -}

treeLookup :: (Eq k, Ord k) => k -> TreeNode k v -> Maybe v
treeLookup k (Node left a b right)
  | k == a = Just b
  | k < a = treeLookup k left 
  | k > a = treeLookup k right 
  | otherwise = Nothing

{-
 - P3
 - Given a list of key-value pairs, return a TreeNode (the root of the tree)
 - for the binary search tree built form the key-value pairs.
 -}
treeFromList :: (Eq k, Ord k) => [(k, v)] -> TreeNode k v
treeFromList [] = Nil
treeFromList (x:xs) = helper (Node Nil (fst x) (snd x) Nil) xs
  where
    helper tree [] = tree
    helper tree (x:xs) = helper (treeInsert (fst x) (snd x) tree) xs
 

{-
 - P4
 - Create a function that performs a folding operation over a TreeNode using an
 - preorder traversal.
 -}
treePreorderFold :: (Eq k, Ord k) => (k -> v -> a -> a) -> a -> TreeNode k v -> a
treePreorderFold _ s Nil = s
treePreorderFold f s (Node left k v right) = treePreorderFold f (treePreorderFold f (f k v s) left) right

{-
 - P5
 - Create a function that performs a folding operation over a TreeNode using
 - an inorder traversal.
 -}
treeInorderFold :: (Eq k, Ord k) => (k -> v -> a -> a) -> a -> TreeNode k v -> a
treeInorderFold _ s Nil = s
treeInorderFold f s (Node left k v right) = treeInorderFold f (f k v (treeInorderFold f s left)) right



{-
 - P6
 - Create a function that performs a folding operation over a TreeNode using
 - a postorder traversal.
 -}
treePostorderFold :: (Eq k, Ord k) => (k -> v -> a -> a) -> a -> TreeNode k v -> a
treePostorderFold _ s Nil = s
treePostorderFold f s (Node left k v right) = f k v (treePostorderFold f (treePostorderFold f s left) right)

{-
 - P7
 - Implement a map function that maps the TreeNode's value using the given function.
 -}
treeMap :: (v -> v') -> TreeNode k v -> TreeNode k v'
treeMap _ Nil = Nil
treeMap f (Node left k v right) = Node (treeMap f left) k (f v) (treeMap f right)

{-
 - P8
 - Given a tree whose nodes contain int keys and int values and a target in, determine
 - if there exists a root to leaf path whose values sum to the given target value.
 -
 - You may assume if you are given an empty tree (i.e., Nil) has a sum of 0.
 -}
hasPathSum :: TreeNode Int Int -> Int -> Bool
hasPathSum Nil n = if n /= 0 then False else True
hasPathSum (Node left k v right) n = hasPathSum left (n - v) || hasPathSum right (n - v) 


main = do
  let s = Student studentName
  args <- getArgs
  let exclusions = parse args
  autograde s exclusions
