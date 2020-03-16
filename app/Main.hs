module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day6


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


star1 = "First star: "
star2 = "Second star: "

day1 :: IO String
day1 = do
    contents <- B.readFile "inputs/Day1.input"
    putStrLn $ star1 ++ (Day1.task1 contents)
    putStrLn $ star2 ++ (Day1.task2 contents)
    return "Success"

day2 :: IO String
day2 = do
    contents <- B.readFile "inputs/Day2.input"
    putStrLn $ star1 ++ (Day2.task1 contents)
    putStrLn $ star2 ++ (Day2.task2 contents)
    return "Success"

day3 :: IO String
day3 = do
    contents <- B.readFile "inputs/Day3.input"
    putStrLn $ star1 ++ (Day3.task1 contents)
    putStrLn $ star2 ++ (Day3.task2 contents)
    return "Success"

day4 :: IO String
day4 = do
    putStrLn $ star1 ++ (Day4.task1)
    putStrLn $ star2 ++ (Day4.task2)
    return "Success"

day6 :: IO String
day6 = do
    testInput <- readFile "inputs/test/Day6.test"
    testInput2 <- readFile "inputs/test/Day6.task2.test"
    input <- readFile "inputs/Day6.input"
    putStrLn $ "Task 1 test values: " ++ (Day6.task1 testInput)
    putStrLn $ star1 ++ (Day6.task1 input)
    -- putStrLn $ "Task 2 test values: " ++ (Day6.task2 testInput2)
    putStrLn $ star2 ++ (Day6.task2 input)
    return "Success"


taskList :: [IO String]
taskList = 
    [ day1
    , day2
    , day3
    , day4
    , day6
    ]
