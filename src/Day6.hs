module Day6 where

import Data.Function ((&))

--parsing imports
import Text.Megaparsec.Char (alphaNumChar, char)
import Text.Megaparsec

-- general datastructures
import Data.Void (Void)
import Data.Either (isRight)
import Data.List (foldl')
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Data.Either as Either


import Control.Monad (join)
import Data.Bifunctor (bimap, first)


type Parser = Parsec Void String

task1 :: String -> String
task1 input =
    parse parseOrbits "" input
    & (\orbits -> fmap ( foldl' (\hashmap (center, orbit) -> Map.alter (appendOrCreate orbit) center hashmap) Map.empty) orbits  )
    & (\orbitMap -> fmap (countOrbits "COM" 0) orbitMap)
    & show

-- Checks if a foldable of Eithers consists only of Rights
allRight :: (Foldable t) => t (Either a b) -> Bool
allRight eithers =
    foldr (\x acc -> (isRight x) && acc ) True eithers

-- Parses many orbits
parseOrbits :: Parser [(String, String)]
parseOrbits = do
    orbits <- parseOrbit `sepBy` char '\n'
    pure $ orbits

-- Parses a single orbit
parseOrbit :: Parser (String, String)
parseOrbit = do
    left <- some alphaNumChar 
    char ')'
    right <- some alphaNumChar
    pure $ (left, right)

-- Appends to the list at key or creates a new entry if not present
appendOrCreate :: a -> Maybe [a] -> Maybe [a]
appendOrCreate newValue toBeAltered =
    case toBeAltered of
        Nothing -> Just [newValue]
        Just x -> Just (newValue:x)

-- counts the orbits in the map of orbits
countOrbits :: String -> Int -> Map.Map String [String] -> Int
countOrbits key depth orbitMap =
    let orbiters = Map.lookup key orbitMap
    in
        case orbiters of
            Nothing -> depth
            Just os ->
                case os of
                    [] -> depth -- in practice already handled by Nothing case
                    [child] -> depth + (countOrbits child (depth+1) orbitMap)
                    children -> foldl' (\sum child -> sum + countOrbits child (depth+1) orbitMap) depth children



-- =========================================

-- task2 :: String -> String
-- task2 input =
--     let orbits = parse parseOrbits "" input
--         orbitmap = fmap ( foldl' (\hashmap (center, orbit) -> Map.alter (appendOrCreate orbit) center hashmap) Map.empty) orbits
--         pathsToYouAndSan = combineEithers2 (fmap (path "COM" "SAN" []) orbitmap) (fmap (path "COM" "YOU" []) orbitmap)
--         -- firstSharedMember = fmap (\pathToSan pathToYou -> findFirstMember pathToSan (Set.fromList pathToYou)) pathsToYouAndSan  --makes me feel like this should really be done with monad / applicative in do notation...
--     in
--         show pathsToYouAndSan
--             -- Left err -> show err
--             -- Right shared -> show shared

task2 :: String -> String
-- task2 input =
--     let orbits = parse parseOrbits "" input
--         orbitmap = fmap ( foldl' (\hashmap (center, orbit) -> Map.alter (appendOrCreate orbit) center hashmap) Map.empty) orbits
--     in
--         case fmap (path "COM" "SAN" []) orbitmap of
--             Left e -> "failed to find path to santa: " ++ (show e)
--             Right p2San ->
--                 case fmap (path "COM" "YOU" []) orbitmap of
--                     Left e -> "failed to find path to you " ++ (show e)
--                     Right p2You ->
--                         case firstSharedMemeberInd p2San p2You of
--                             Nothing -> "failed to find shared member"
--                             Just (indexSan, indexYou) ->
--                                 show indexSan ++ " -- " show indexYou
task2 input =
    case task2' input of
        Left err -> err
        Right ok -> ok


task2' :: String -> Either String String
task2' input = do
    orbits <- first (show) $ parse parseOrbits "" input 
    -- create hashmap from orbits
    let orbitmap = foldl' (\hashmap (center, orbit) -> Map.alter (appendOrCreate orbit) center hashmap) Map.empty orbits
    -- calculate paths to root
    pathToSanta <- toEither "failure at Path to Santa" $ path "COM" "SAN" [] orbitmap
    pathToYou <- toEither "failure at Path to You" $ path "COM" "YOU" [] orbitmap
    -- find index of split node
    (indexSan, indexYou) <- toEither "unable to find shared member" $ firstSharedMemberIndices pathToSanta pathToYou
    pure $ foldl' (++) "" [show pathToSanta, "and ", show pathToYou, "\nTransfers required:", show (indexSan + indexYou)] 


toEither :: a -> Maybe b -> Either a b
toEither left x  =
    case x of
        Nothing -> Left left
        Just y -> Right y

-- A path through the orbit map. Can only go down through the hierarchy.
path :: String -> String -> [String] -> Map.Map String [String] -> Maybe [String]
path from to acc orbitMap =
    if from == to then
        Just acc
    else
        let maybeOrbiters = Map.lookup from orbitMap
            orbiterPaths =
                case maybeOrbiters of
                    Nothing -> [] -- impossible path
                    Just orbiters -> map (\orbiter -> path orbiter to (from:acc) orbitMap) orbiters
        in
            foldl' (\acc maybePath ->
                case acc of
                    Just path -> Just path
                    Nothing ->
                        case maybePath of
                            Just path' -> Just path'
                            Nothing -> Nothing
                ) Nothing orbiterPaths

-- extracts index of first shared member in both lists 
-- O(n*m)
firstSharedMemberIndices :: [String] -> [String] -> Maybe (Int, Int)
firstSharedMemberIndices a b =
    let (enumA, enumB) = join bimap (zip [0..]) (a, b)
    in
        foldl' (\accA (indexA, elemA) ->
            case accA of
                Just x -> Just x
                Nothing ->
                    case (foldl' (\accB (indexB, elemB) ->
                        case accB of
                            Just y -> Just y
                            Nothing -> 
                                if elemA == elemB then
                                    Just (indexA, indexB)
                                else
                                    Nothing
                    ) Nothing enumB) of
                        Just (indexA, indexB) -> Just (indexA, indexB)
                        Nothing -> Nothing 
        ) Nothing enumA

-- Combines a list of Either into a Single Either around a list of the contained values.
-- Will always return Left if at least one Either is Left <=> Will return Right if all Values are Right 
combineEithers :: [Either a b] -> Either [a] [b]
combineEithers eithers =
    case Either.lefts eithers of
        [] -> Right $ Either.rights eithers
        [lefts] -> Left [lefts]

-- Basically the same as combineEithers but for a 2-tuple instead of lists
-- Will always return Left if at least one Either is Left, and will only return the first value of Left encountered (because I'm lazy and this is just a coding challenge, not prod code)
combineEithers2 :: (Either a b,  Either a b) -> Either a (b,b)
combineEithers2 (e, f) =
    case e of
        Right val1 ->
            case f of
                Right val2 ->
                    Right (val1, val2)
                Left err->
                    Left err
        Left err ->
            Left err