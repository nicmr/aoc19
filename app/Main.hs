module Main where

import qualified Day1
import qualified Day2
import qualified Day3


import qualified Data.ByteString as B
import qualified Data.List.Safe as Safe
import Data.Function ((&))


import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO () 
main = do
    args <- getArgs
    let mDay = (Safe.!!) args 0
    -- convert to index integer
    let mIndex = mDay & fmap read & fmap (subtract 1)
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

day2 :: IO String
day2 = do
    contents <- B.readFile "inputs/Day2.input"
    putStrLn $ "First star: " ++ (Day2.task1 contents)
    putStrLn $ "Second star: " ++ (Day2.task2 contents)
    return "Success"

day3 :: IO String
day3 = do
    contents <- B.readFile "inputs/Day3.input"
    putStrLn $ "First star: " ++ (Day3.task1 contents)
    putStrLn $ "Second star: " ++ (Day3.task2 contents)
    return "Success"

taskList :: [IO String]
taskList = 
    [ day1
    , day2
    , day3
    ]


