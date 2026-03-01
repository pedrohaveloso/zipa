module Zipa.MinHeap
  ( MinHeap,
    empty,
    insert,
    peek,
    poll,
    size,
    from,
    fromList,
  )
where

import Data.Function (on)
import Zipa.BST (BST)
import qualified Zipa.BST as BST

type MinHeap p v = BST (p, v)

empty :: MinHeap p v
empty = BST.empty

size :: MinHeap p v -> Int
size = BST.size

insert :: (Ord p) => (p, v) -> MinHeap p v -> MinHeap p v
insert = BST.insertBy (compare `on` fst)

peek :: MinHeap p v -> Maybe v
peek mh = case BST.first mh of
  Just (_, v) -> Just v
  _ -> Nothing

poll :: MinHeap p v -> Maybe ((p, v), MinHeap p v)
poll mh = case BST.extract mh of
  Just (v, rest) -> Just (v, rest)
  _ -> Nothing

from :: (Ord p) => (p, v) -> MinHeap p v
from = BST.fromBy (compare `on` fst)

fromList :: (Ord p) => [(p, v)] -> MinHeap p v
fromList = BST.fromListBy (compare `on` fst)
