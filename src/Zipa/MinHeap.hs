{-# LANGUAGE InstanceSigs #-}

module Zipa.MinHeap where

newtype Value a = Value (Int, a)

instance Ord (Value a) where
  compare :: Value a -> Value a -> Ordering
  compare (Value (a, _)) (Value (b, _)) = compare a b

newtype MinHeap a = MinHeap (BinaryTree (Value a))

insert :: MinHeap a -> Int -> a -> MinHeap a
insert pq p value = undefined
