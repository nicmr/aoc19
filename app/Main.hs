module Main where

import qualified Day1


import qualified Data.ByteString as B
import Data.List.Safe as Safe
import Data.Function ((&))


import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO () 
main = do
    args <- getArgs
    let mDay = (Safe.!!) args 0
    let mIndex = mDay & fmap read & fmap ((-) 1)
    let mTask = mIndex >>= ((Safe.!!) taskList)
    s <- case mTask of
        Nothing ->
            pure $ "Please pass an integer representing the day (1-" ++ (show (length taskList)) ++ ")" 
        Just task ->
            task
    putStrLn s
    putStrLn "done."

day1 :: IO String
day1 = do
    contents <- B.readFile "inputs/Day1.input"
    putStrLn $ "First star: " ++ (Day1.task1 contents)
    putStrLn $ "Second star: " ++ (Day1.task2 contents)
    return "Success"

taskList :: [IO String]
taskList = 
    [ day1
    ]


