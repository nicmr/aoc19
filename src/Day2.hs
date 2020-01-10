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
        Just result -> show result
        Nothing -> "failed to run"

task1' :: B.ByteString -> Maybe ProgramState
task1' contents = do
    let integers = parseContents contents
    let modifiedIntegers = replaceNth 2 2 $ replaceNth 1 12 integers
    a <- runProgram (ProgramState 0 modifiedIntegers)
    return a


task2 :: B.ByteString -> String
task2 contents =
    "Okay I did this one by hand. Solution is 100 * 98 + 20 = 9820"
    -- let
    --     task1result = (fromMaybe [0] (task1' contents)) !! 0
    --     (multiplier, rest) = divMod 19690720 task1result
    -- in
    --     "multiplier: " ++ (show multiplier) ++ "\n" 
    --     ++ "rest: " ++ (show rest) ++ "\n" 
    --     ++ "answer: " ++ show (100 * 12 * multiplier + rest)


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

extractTuple4 :: Int -> [a] -> Maybe (a,a,a,a)
extractTuple4 index list=
    case splitAt index list of
        (_, (a:(b:(c:(d:xs))))) -> Just (a,b,c,d)
        _ -> Nothing

buildInstruction :: Int -> (Int, Int, Int, Int) -> Maybe Instruction
buildInstruction maxAddr (op, from0, from1, to) = do
    let validAddress = (\a -> maybeSmaller a maxAddr)
    op' <- fromInt op
    from0' <- validAddress from0
    from1' <- validAddress from1
    to' <- validAddress to
    return (Instruction op' from0' from1' to')

-- ProgramState ip program
-- ip = instruction pointer
data ProgramState = ProgramState Int [Int]
    deriving Show

runProgram ::  ProgramState ->  Maybe ProgramState
runProgram (ProgramState ip []) = Just (ProgramState ip [])
runProgram programState = do
    (isFinished, modified) <- step programState
    finalProgram <- case isFinished of
        True -> pure modified
        False -> runProgram modified
    return finalProgram



step :: ProgramState -> Maybe (Bool, ProgramState)
step (ProgramState ip program) = do
    validIP <- maybeSmaller ip (length program)
    tuple <- extractTuple4 validIP program
    instruction <- buildInstruction (length program * 4) tuple
    let (finished, modified) =  execute program instruction
    return (finished, ProgramState (ip + 4) modified)
    
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