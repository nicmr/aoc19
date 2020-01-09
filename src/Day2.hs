module Day2
    ( task1
    , task2
    , instruction
    )
    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import qualified Text.Read

import Data.Function ((&))


task1 :: B.ByteString -> String
task1 contents =
    "not yet implemented"

-- task1' :: B.ByteString -> Maybe String
-- task1' contents = do
--     let parsed = parseContents content
    

groupsOf4 :: [a] -> Maybe [(a, a, a, a)]
groupsOf4 list =
    groupsOf4' list []
    
groupsOf4' :: [a] -> [(a,a,a,a)] -> Maybe [(a,a,a,a)]
groupsOf4' [] acc = Just (reverse acc)
-- this would be so nice, not sure how to write correctly
-- groupsOf4' [a,b,c,d:xs] acc = groupsOf4' xs ((a,b,c,d) : acc)
-- groupsOf4' _ acc = Nothing
groupsOf4' list acc =
    if length list > 4 then
        let ([a,b,c,d], back) = splitAt 4 list
        in groupsOf4' back ((a,b,c,d) : acc)
    else 
        Nothing



    

parseContents :: B.ByteString -> [Int]
parseContents contents =
    B.split comma contents
    & map Char8.unpack
    & map (Text.Read.readMaybe :: [Char] -> Maybe Int)
    & map (fromMaybe 0)

comma = fromIntegral (Char.ord ',')



task2 :: B.ByteString -> String
task2 _ = "not yet implemented"

data Op = Add |  Mult | Finish

fromInt :: Int -> Maybe Op
fromInt 1 = Just Add
fromInt 2 = Just Mult
fromInt 99 = Just Finish
fromInt _  = Nothing

data Instruction = Instruction Op Int Int Int

maybeSmaller :: (Ord a) => a -> a -> Maybe a
maybeSmaller x y =
    if x < y then
        Just x
    else
        Nothing

instruction :: Int -> Int -> Int -> Int -> Int -> Maybe Instruction
instruction op from0 from1 to maxAddr = do
    let validAddress = (\a -> maybeSmaller a maxAddr)
    op' <- fromInt op
    from0' <- validAddress from0
    from1' <- validAddress from1
    to' <- validAddress to
    return (Instruction op' from0' from1' to')