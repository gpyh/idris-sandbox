module Main

import Ex5_3_5
import Data.Vect

main : IO ()
main = do
  (n ** vec) <- readVectFile "/home/gpyh/socket/test.txt"
  putStrLn (concat vec)
