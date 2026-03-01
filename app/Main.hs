module Main (main) where

-- import Zipa (compress, todo)

import qualified Data.ByteString as ByteString
import Data.Function ((&))
import System.Environment (getArgs)

invalidCmd :: IO ()
invalidCmd = print "Invalid command. 'zipa compress <file>' or 'zipa decompress <file>'."

main :: IO ()
main = undefined