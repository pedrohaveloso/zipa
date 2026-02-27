module Main (main) where

-- import Zipa (compress, todo)

import qualified Data.ByteString as ByteString
import Data.Function ((&))
import System.Environment (getArgs)
import qualified Zipa.BinaryTree as BinaryTree

invalidCmd :: IO ()
invalidCmd = print "Invalid command. 'zipa compress <file>' or 'zipa decompress <file>'."

main :: IO ()
main = do
  -- x <- todo

  print $ BinaryTree.fromList [10, 20, 2, 4, 99]
  print $
    BinaryTree.insert 10 BinaryTree.empty
      & BinaryTree.insert 20
      & BinaryTree.insert 2
      & BinaryTree.insert 4
      & BinaryTree.insert 99

-- args <- getArgs

-- case args of
--   [cmd, file] -> case cmd of
--     "compress" -> do
--       content <- ByteString.readFile file
--       let compressed = compress content
--       ByteString.writeFile (file <> ".zipa") compressed
--     "decompress" -> undefined
--     _ -> invalidCmd
--   _ -> invalidCmd
