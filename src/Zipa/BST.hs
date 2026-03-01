module Zipa.BST
  ( BST (..),
    extract,
    empty,
    first,
    from,
    fromList,
    insert,
    merge,
    size,
    fromBy,
    fromListBy,
    insertBy,
  )
where

data BST a
  = Empty
  | Node (BST a) a (BST a)
  deriving (Show)

empty :: BST a
empty = Empty

insert :: (Ord a) => a -> BST a -> BST a
insert value (Node left leaf right)
  | leaf < value = Node left leaf (insert value right)
  | otherwise = Node (insert value left) leaf right
insert value Empty = Node Empty value Empty

merge :: a -> BST a -> BST a -> BST a
merge parent a = Node a parent

from :: (Ord a) => a -> BST a
from = flip insert empty

fromList :: (Ord a) => [a] -> BST a
fromList = foldl (flip insert) empty

first :: BST a -> Maybe a
first (Node Empty value _) = Just value
first (Node left _ _) = first left
first Empty = Nothing

size :: BST a -> Int
size Empty = 0
size (Node left _ right) = 1 + size left + size right

extract :: BST a -> Maybe (a, BST a)
extract Empty = Nothing
extract (Node Empty value right) = Just (value, right)
extract (Node left value right) = do
  (value', left') <- extract left
  Just (value', Node left' value right)

type Cmp a = (a -> a -> Ordering)

insertBy :: Cmp a -> a -> BST a -> BST a
insertBy cmp value (Node left leaf right) =
  case value `cmp` leaf of
    LT -> Node (insertBy cmp value left) leaf right
    _ -> Node left leaf (insertBy cmp value right)
insertBy _ value Empty = Node Empty value Empty

fromBy :: Cmp a -> a -> BST a
fromBy cmp = flip (insertBy cmp) empty

fromListBy :: Cmp a -> [a] -> BST a
fromListBy cmp = foldl (flip (insertBy cmp)) empty
