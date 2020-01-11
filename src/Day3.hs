module Day3 where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as Char8
import qualified Data.Char as Char

import Data.Function ((&))
import Text.Megaparsec.Char (newline)

import Data.Void (Void)


import Text.Megaparsec

task1 :: B.ByteString -> String
task1 contents = 
    show $ parse parseContents "" (toString contents)

task2 :: B.ByteString -> String
task2 _ = 
    "not yet implemented"


toString :: B.ByteString -> String
toString bytes = do
    Char8.unpack bytes


type Parser = Parsec Void String

-- prefixed with Go to avoid name clash with Prelude.Either
data Direction = GoUp | GoDown | GoLeft | GoRight
    deriving Show

data Segment = Segment Direction Int
    deriving Show


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


test' =
    parse parseContents "" "U23,D14\nU17"