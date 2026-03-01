module Zipa
  ( -- todo,
    compress,
    decompress,
  )
where

import Data.Bits (Bits (shiftL, (.|.)))
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as Char8
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Zipa.BST (BST)
import qualified Zipa.BST as BST
import Zipa.MinHeap (MinHeap)
import qualified Zipa.MinHeap as MinHeap

type ByteFreqs = [(Word8, Int)]

byteFreqs :: ByteString -> ByteFreqs
byteFreqs s = go (ByteString.unpack s) HashMap.empty & HashMap.toList & sortFreqs
  where
    go :: [Word8] -> HashMap Word8 Int -> HashMap Word8 Int
    go [] hm = hm
    go (b : bs) hm =
      go bs $
        if HashMap.member b hm
          then HashMap.update (\i -> Just $ i + 1) b hm
          else HashMap.insert b 1 hm

    sortFreqs :: ByteFreqs -> ByteFreqs
    sortFreqs = sortBy (\(_, a) (_, b) -> compare a b)

type Binary = [Bool]

binaryToString :: Binary -> String
binaryToString = concatMap (\bin -> if bin then "1" else "0")

bitsToWord8 :: Binary -> Word8
bitsToWord8 bits = foldl (\acc b -> (acc `shiftL` 1) .|. if b then 1 else 0) 0 (take 8 bits)

bitsToByteString :: Binary -> ByteString
bitsToByteString [] = ByteString.empty
bitsToByteString bits =
  let (byte, rest) = splitAt 8 bits
   in ByteString.cons (bitsToWord8 byte) (bitsToByteString rest)

showDict :: Dict -> ByteString
showDict (Dict dict) =
  ByteString.intercalate (Char8.pack ";;") $ map (\(c, b) -> (ByteString.pack [c]) <> (Char8.pack "::") <> (bitsToByteString b)) (HashMap.toList dict)

type Queue = MinHeap Int (BST [Word8])

makeQueue :: ByteString -> Queue
makeQueue text =
  text
    & byteFreqs
    & map (\(w, i) -> (i, BST.from [w]))
    & MinHeap.fromList

type Tree = BST [Word8]

makeTree :: Queue -> Maybe Tree
makeTree queue
  | MinHeap.size queue > 1 = do
      ((fp, fv), queue1) <- MinHeap.poll queue
      ((sp, sv), queue2) <- MinHeap.poll queue1

      fv' <- BST.first fv
      sv' <- BST.first sv

      let newV = fv' ++ sv'
      let newP = fp + sp

      makeTree $ MinHeap.insert (newP, BST.merge newV fv sv) queue2
  | otherwise = MinHeap.peek queue

newtype Dict = Dict (HashMap Word8 Binary)

makeDict :: Tree -> Maybe Dict
makeDict tree = case go tree [] of
  Nothing -> Nothing
  Just dict -> Just $ Dict $ HashMap.fromList dict
  where
    go :: Tree -> Binary -> Maybe [(Word8, Binary)]
    go BST.Empty _ = Nothing
    -- todo head
    go (BST.Node BST.Empty value BST.Empty) acc = pure [(head value, acc)]
    go (BST.Node left _ right) acc = do
      l <- go left (False : acc)
      r <- go right (True : acc)
      pure $ l ++ r

compress :: ByteString -> ByteString
compress text = fromMaybe text compress'
  where
    compress' :: Maybe ByteString
    compress' = do
      let queue = makeQueue text

      tree <- makeTree queue
      (Dict dict) <- makeDict tree

      let allBits :: Binary
          allBits =
            ByteString.foldr
              (\b acc -> maybe [] id (HashMap.lookup b dict) ++ acc)
              []
              text
          compressed = bitsToByteString allBits

      pure $ showDict (Dict dict) <> Char8.pack "##" <> compressed

-- Nothing -> ByteString.empty
-- Just tree -> do
--   let (Dict dict) = makeDict tree
--       compressed =
--         ByteString.concatMap
--           (maybe (Char8.pack "") bitsToByteString . (`HashMap.lookup` dict))
--           text

--   showDict (Dict dict) <> Char8.pack "##" <> compressed

decompress :: ByteString -> ByteString
decompress = id
