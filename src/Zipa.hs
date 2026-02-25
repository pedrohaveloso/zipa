{-# LANGUAGE InstanceSigs #-}

module Zipa
  ( compress,
    decompress,
  )
where

import Data.Function ((&))
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate, sortBy)

data Tree
  = Node String Tree Tree
  | Leaf Char
  deriving (Show)

charFreqs :: String -> [(Char, Int)]
charFreqs s = go s HM.empty & HM.toList & sortFreqs
  where
    go :: String -> HM.HashMap Char Int -> HM.HashMap Char Int
    go [] hm = hm
    go (c : cs) hm =
      go cs $
        if HM.member c hm
          then HM.update (\i -> Just $ i + 1) c hm
          else HM.insert c 1 hm

    sortFreqs :: [(Char, Int)] -> [(Char, Int)]
    sortFreqs = sortBy (\(_, a) (_, b) -> compare a b)

makeTree :: String -> Maybe Tree
makeTree text = go chars
  where
    chars :: String
    chars = text & charFreqs & map fst

    go :: String -> Maybe Tree
    go [] = Nothing
    go [c] = Just $ Leaf c
    go [a, b] = Just $ Node [a, b] (Leaf a) (Leaf b)
    go (a : b : cs) = do
      node <- go [a, b]
      rest <- go cs
      Just $ Node (a : b : cs) rest node

type Binary = [Bool]

binaryToString :: Binary -> String
binaryToString = concatMap (\bin -> if bin then "1" else "0")

newtype Dict = Dict (HM.HashMap Char Binary)

instance Show Dict where
  show :: Dict -> String
  show (Dict dict) =
    intercalate ";;" $ map (\(c, b) -> c : "::" <> binaryToString b) (HM.toList dict)

makeDict :: Tree -> Dict
makeDict tree = Dict $ HM.fromList $ go tree []
  where
    go :: Tree -> Binary -> [(Char, Binary)]
    go (Leaf value) acc = [(value, acc)]
    go (Node _ a b) acc = go a (False : acc) ++ go b (True : acc)

type Header = String

type Compressed = String

compress :: String -> (Header, Compressed)
compress text = case makeTree text of
  Nothing -> ([], [])
  Just tree -> do
    let (Dict dict) = makeDict tree
        compressed =
          concatMap
            (maybe "" binaryToString . (`HM.lookup` dict))
            text

    (show (Dict dict), compressed)

decompress :: Compressed -> Maybe String
decompress compressed = undefined
