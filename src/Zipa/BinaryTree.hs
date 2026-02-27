module Zipa.BinaryTree
  ( BinaryTree (..),
    empty,
    from,
    fromList,
    insert,
    merge,
    first,
  )
where

data BinaryTree a
  = Empty
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Show)

empty :: BinaryTree a
empty = Empty

insert :: (Ord a) => a -> BinaryTree a -> BinaryTree a
insert value (Node left leaf right) =
  if leaf < value
    then Node left leaf (insert value right)
    else Node (insert value left) leaf right
insert value Empty = Node Empty value Empty

merge :: a -> BinaryTree a -> BinaryTree a -> BinaryTree a
merge parent a = Node a parent

from :: (Ord a) => a -> BinaryTree a
from = flip insert empty

fromList :: (Ord a) => [a] -> BinaryTree a
fromList = foldl (flip insert) empty

first :: BinaryTree a -> Maybe a
first (Node _ leaf _) = Just leaf
first Empty = Nothing
