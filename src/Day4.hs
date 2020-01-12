module Day4 where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

import Data.Array (Array, (!)) 
import qualified Data.Array as Array
import Data.Ix (Ix)

import Data.Function ((&))

input = "152085-670283"

task1 :: String
task1 = 
    show $ length possibleKeys
    -- show possibleKeys

task2 :: String
task2 =
    show $ length possibleKeys2

possibleKeys :: [Int]
possibleKeys =
    [x | x <- [152085..670283]
    , let digs = digits x
    , ascending (digits x)
    , hasDuplicate IntSet.empty digs
    ]

possibleKeys2 :: [Int]
possibleKeys2 =
    [x | x <- [152085..670283]
    , let digs = digits x
    , ascending (digits x)
    , duplicatesButNotTriples (Array.array (0, 9) [(y,0) | y <- [0..9]]) digs
    ]

digits :: Integral x => x -> [x]
digits 0 = []
digits x = digits (x `div` 10) ++ [x `mod` 10]

-- checks if the list contains a duplicate
hasDuplicate :: IntSet -> [Int] -> Bool
hasDuplicate _ [] = False
hasDuplicate set (x:xs) =
    if IntSet.member x set then
        True
    else
        hasDuplicate (IntSet.insert x set) xs    

-- duplicatesButNotTriples :: (Ix a, Ix b) => Array (a,b) Int -> [Int] -> Bool
duplicatesButNotTriples :: Array Int Int -> [Int] -> Bool
duplicatesButNotTriples array [] =
    Array.elems array
    & filter (\value -> value == 2)
    & length
    & (\a -> a>0)

duplicatesButNotTriples array (x:xs) =
    duplicatesButNotTriples (Array.accum (+) array [(x,1)]) xs


-- checks if all members of the list are ascending
ascending :: (Ord a) => [a] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)