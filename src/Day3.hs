{-# LANGUAGE StrictData #-}

module Day3 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char

import Data.List (foldl', intersect, minimumBy)
import Data.Function ((&))
import Data.Void (Void)

import Text.Megaparsec.Char (newline)
import Text.Megaparsec


task1 :: B.ByteString -> String
task1 contents = 
    let as_string = (toString contents)
    in
        case task1' as_string of
            Left error -> "Error: " ++ error
            Right ok -> show $ ok

task1' :: String -> Either String Int
task1' contents =
    let
        eitherErrorSteps = parse parseContents "" contents
    in
        case eitherErrorSteps of
            Left error ->
                Left $ "Failed to parse input:\n" ++ show error
            Right steps ->
                let traced = map tail $ map (foldl' trace [(0,0)]) steps
                in 
                    case traced of
                        [] ->
                            Left $ "Tracing results in empty list. Result of parser: " ++ (show steps)
                        (x:xs) -> 
                            Right $ uncurry (\a b -> manhattanDistance (a,b)) $ minimumBy manhattanOrdering $ intersect (traced !! 0) (traced !! 1)

task2 :: B.ByteString -> String
task2 _ = 
    "not yet implemented"

toString :: B.ByteString -> String
toString bytes = do
    Char8.unpack bytes

type Parser = Parsec Void String

-- prefixed with Go to avoid name clash with Prelude.Either
data Direction = GoUp | GoDown | GoLeft | GoRight
    deriving (Show, Eq)

data Segment = Segment Direction Int
    deriving (Show, Eq)

parseUp :: Parser Direction
parseUp = do
    single 'U'
    return GoUp

parseDown :: Parser Direction
parseDown = do
    single 'D'
    return GoDown

parseLeft :: Parser Direction
parseLeft = do
    single 'L'
    return GoLeft


parseRight :: Parser Direction
parseRight = do
    single 'R'
    return GoRight

parseDigit :: Parser Char
parseDigit = do
    a <- satisfy Char.isDigit
    return a

parseSegment :: Parser Segment
parseSegment = do
    direction <- parseUp <|> parseDown <|> parseLeft <|> parseRight
    numStr <- many parseDigit
    let numInt = read numStr
    return (Segment direction numInt)

parseSequence :: Parser [Segment]
parseSequence = do
    segments <- sepBy parseSegment (single ',')
    return segments

parseContents :: Parser [[Segment]]
parseContents = do
    first <- parseSequence
    newline
    second <- parseSequence
    eof
    return [first, second]

trace :: [(Int, Int)] -> Segment -> [(Int, Int)]
-- we can't trace without start values
trace [] _  = []
trace acc (Segment direction distance)=
    let
        (headX, headY) = last acc
        nextSteps = case direction of
            GoUp -> [(headX,y) | y <- [(headY+1)..(headY + distance)]]
            GoDown -> [(headX,y) | y <- [(headY-1),(headY-2)..(headY - distance)]]
            GoRight -> [(x,headY) | x <- [(headX+1)..(headX + distance)]]
            GoLeft -> [(x,headY) | x <- [(headX-1),(headX-2)..(headX - distance)]]
    in
        acc ++ nextSteps

manhattanOrdering :: (Int, Int) -> (Int, Int) -> Ordering
manhattanOrdering a b =
    compare (manhattanDistance a) (manhattanDistance b)

manhattanDistance :: (Int, Int) -> Int
manhattanDistance (x,y) =
    (abs x) + (abs y)