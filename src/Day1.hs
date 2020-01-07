module Day1
    ( massToFuel
    , recursiveMTF
    , task1
    , task2
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Text.Read

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)


import Data.Function ((&))

task1 :: B.ByteString -> String
task1 raw_data =
    let
        masses = parseContents raw_data
        total = totalFuel masses
    in
        show total

task2 :: B.ByteString -> String
task2 raw_data =
    let
        masses = parseContents raw_data
        total = totalFuelRecursive masses
    in
        show total

linebreak = fromIntegral (Char.ord '\n')

parseContents :: B.ByteString -> [Int]
parseContents contents =
    B.split linebreak contents
    & map Char8.unpack
    & map (Text.Read.readMaybe :: [Char] -> Maybe Int)
    & map (fromMaybe 0)

totalFuel :: [Int] -> Int
totalFuel masses =
    masses
    & map massToFuel
    & foldr (+) 0


massToFuel :: Int -> Int
massToFuel mass =
    maximum [(quot mass 3) - 2, 0]

-- ==============================
-- recursive functions for Task 2
-- ==============================

totalFuelRecursive :: [Int] -> Int
totalFuelRecursive masses =
    masses
    & map (\a -> recursiveMTF a 0)
    & foldr (+) 0

-- let MTF = MassToFuel
-- should be eligible for TCO
recursiveMTF :: Int -> Int -> Int
recursiveMTF mass acc =
    if mass > 0 then
        recursiveMTF extraFuel (acc + extraFuel)
    else
        acc 
    where
        extraFuel = massToFuel mass