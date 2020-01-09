module Day2

    where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char
import Data.Maybe (fromMaybe)
import qualified Text.Read

import Data.Function ((&))


task1 :: B.ByteString -> String
task1 contents =
    case task1' contents of
        Just s -> s
        Nothing -> "failed to run"


task2 :: B.ByteString -> String
task2 _ = "not yet implemented"

task1' :: B.ByteString -> Maybe String
task1' contents = do
    let integers = parseContents contents
    let modifiedIntegers = replaceNth 2 2 $ replaceNth 1 12 integers
    a <- runProgram 0 modifiedIntegers
    return (show a)

extractTuple4 :: Int -> [a] -> Maybe (a,a,a,a)
extractTuple4 index list=
    case splitAt index list of
        (_, (a:(b:(c:(d:xs))))) -> Just (a,b,c,d)
        _ -> Nothing

parseContents :: B.ByteString -> [Int]
parseContents contents =
    B.split comma contents
    & map Char8.unpack
    & map (Text.Read.readMaybe :: [Char] -> Maybe Int)
    -- instead of using a default value, consider returning Maybe [Int] and
    --   returning Nothing if one element of the list is Nothings
    & map (fromMaybe 0)

comma = fromIntegral (Char.ord ',')


data Op = Add |  Mult | Finish
    deriving Show

fromInt :: Int -> Maybe Op
fromInt 1 = Just Add
fromInt 2 = Just Mult
fromInt 99 = Just Finish
fromInt _  = Nothing

data Instruction = Instruction Op Int Int Int
    deriving Show

maybeSmaller :: (Ord a) => a -> a -> Maybe a
maybeSmaller x y =
    if x < y then
        Just x
    else
        Nothing

buildInstruction :: Int -> (Int, Int, Int, Int) -> Maybe Instruction
buildInstruction maxAddr (op, from0, from1, to) = do
    let validAddress = (\a -> maybeSmaller a maxAddr)
    op' <- fromInt op
    from0' <- validAddress from0
    from1' <- validAddress from1
    to' <- validAddress to
    return (Instruction op' from0' from1' to')

runProgram ::  Int -> [Int] ->  Maybe [Int]
runProgram _ [] = Just []
-- ip = instruction pointer
runProgram ip program = do
    validIP <- maybeSmaller ip (length program)
    tuple <- extractTuple4 validIP program
    instruction <- buildInstruction (length program * 4) tuple
    let (finished, modified) =  execute program instruction
    finalProgram <- case finished of
        True -> pure modified
        False -> runProgram (ip + 4) modified
    return finalProgram

data ProgramState = ProgramState Int [Int]
    deriving Show

step :: ProgramState -> Maybe ProgramState
step (ProgramState ip program) = do
    validIP <- maybeSmaller ip (length program)
    tuple <- extractTuple4 validIP program
    instruction <- buildInstruction (length program * 4) tuple
    let (finished, modified) =  execute program instruction
    return (ProgramState (ip + 4) modified)
    
execute :: [Int] -> Instruction -> (Bool, [Int])
execute p (Instruction op from0 from1 to) =
    case op of
        Add -> 
            (False, replaceNth to ((p !! from0) + (p !! from1)) p)
        Mult ->
            (False, replaceNth to ((p !! from0) * (p !! from1)) p)
        Finish -> 
            (True, p)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs