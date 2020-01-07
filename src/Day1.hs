module Day1
    ( massToFuel
    , run
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Text.Read

import qualified Data.Char as Char
import Data.Maybe (fromMaybe)


import Data.Function ((&))

run :: IO String
run = do
    contents <- B.readFile "inputs/Day1.input"
    let masses = parseContents contents
    let total = totalFuel masses
    return (show total)

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
    (quot mass 3) - 2