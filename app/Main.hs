module Main where

import qualified Day1


main :: IO ()
main = do
    result <- Day1.run
    putStrLn result
