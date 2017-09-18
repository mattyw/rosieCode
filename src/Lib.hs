module Lib
    ( someFunc
    , rot
    , decode
    , boundedSucc
    , boundedPred
    , decodeAll
    , decodeFile
    , rotAll
    , readCode
    , readDict
    ) where

import Data.Set ( Set, member, fromDistinctAscList)

someFunc :: IO ()
someFunc = putStrLn "foo"

boundedSucc :: Char -> Char
boundedSucc 'z' = 'a'
boundedSucc 'Z' = 'A'
boundedSucc c = succ c

boundedPred :: Char -> Char
boundedPred 'a' = 'z'
boundedPred 'A' = 'Z'
boundedPred c = pred c

rot :: Int -> (Char -> Char) -> String -> String
rot 0 _ s = s
rot n f s = rot (n-1) f (map f s)

rotAll :: Int -> (Char -> Char) -> String -> [String]
rotAll 0  _ s = [s]
rotAll n f s = rot n f s : rotAll (n-1) f s

readDict = readLines "/usr/share/dict/british-english"

readCode = readLines "rcode"

decode :: Int -> (Char -> Char) -> [String] -> [String]
decode n f words = map (rot n f) words 

decodeAll :: [String] -> (Char -> Char) -> [[String]]
decodeAll words f = map (rotAll 25 f) words

decodeFile f = do
    fileContent <- readCode
    return (decodeAll fileContent f)

readLines file = do
    words <- readFile file
    return $ lines words
