module Day4 where

import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet

input = "152085-670283"

task1 :: String
task1 = 
    show $ length possibleKeys
    -- show possibleKeys

task2 :: String
task2 =
    "Not yet implemented"

possibleKeys :: [Int]
possibleKeys =
    [x | x <- [152085..670283], let digs = digits x, ascending (digits x), hasDuplicate IntSet.empty digs]

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

-- checks if all members of the list are ascending
ascending :: (Ord a) => [a] -> Bool
ascending [] = True
ascending [x] = True
ascending (x:y:xs) = x <= y && ascending (y:xs)