module Main where

import qualified Day1
import qualified Data.ByteString as B


main :: IO ()
main = do
    contents <- B.readFile "inputs/Day1.input"
    putStrLn $ "First star: " ++ (Day1.task1 contents)
    putStrLn $ "Second star: " ++ (Day1.task2 contents)