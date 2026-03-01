module Main (main) where

import qualified Data.ByteString as ByteString
import Data.Function ((&))
import System.Environment (getArgs)
import Zipa (compress)

invalidCmd :: IO ()
invalidCmd = print "Invalid command. 'zipa compress <file>' or 'zipa decompress <file>'."

main :: IO ()
main = do
  args <- getArgs

  case args of
    [cmd, file] -> case cmd of
      "compress" -> do
        content <- ByteString.readFile file
        let compressed = compress content
        ByteString.writeFile (file <> ".zipa") compressed
      "decompress" -> undefined
      _ -> invalidCmd
    _ -> invalidCmd
