module Zipa
  ( -- todo,
    -- compress,
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

makeQueue :: ByteString -> MinHeap Int (BST Word8)
makeQueue text =
  text
    & byteFreqs
    & map (\(w, i) -> (i, BST.from w))
    & MinHeap.fromList

-- todo :: IO ()
-- todo =
-- print $ makeTree (Char8.pack "aaaabbbcce")

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

newtype Dict = Dict (HashMap Word8 Binary)

-- showDict :: Dict -> ByteString
-- showDict (Dict dict) =
--   ByteString.intercalate (Char8.pack ";;") $ map (\(c, b) -> (ByteString.pack [c]) <> (Char8.pack "::") <> (bitsToByteString b)) (HashMap.toList dict)

-- makeDict :: Tree -> Dict
-- makeDict tree = Dict $ HashMap.fromList $ go tree []
--   where
--     go :: Tree -> Binary -> [(Word8, Binary)]
--     go (Leaf value) acc = [(value, acc)]
--     go (Node _ a b) acc = go a (False : acc) ++ go b (True : acc)

compress :: ByteString -> ByteString
compress text = do
  let queue = makeQueue text

  case makeQueue text of
    Nothing -> ByteString.empty
    Just tree ->
      let (Dict dict) = makeDict tree
          allBits :: Binary
          allBits =
            ByteString.foldr
              (\b acc -> maybe [] id (HashMap.lookup b dict) ++ acc)
              []
              text
          compressed = bitsToByteString allBits
       in showDict (Dict dict) <> Char8.pack "##" <> compressed

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
