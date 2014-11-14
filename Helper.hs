
module Helper where

import Data.Char (ord, chr, isAsciiUpper)
import Data.Maybe (fromJust)
import Data.List (elemIndex, nub, delete)

-- helper functions -----------------------------------------------------------
alphs :: String
alphs = ['A'..'Z']

rem26 :: Int -> Int
rem26 x = rem (x+52) 26

charToInt :: Char -> Int
charToInt c
    | isAsciiUpper c = ord c - 65
    | otherwise = error "Not a capital alphabet in charToInt."

intToChar :: Int -> Char
intToChar x
    | x >= 0 && x <= 25 = chr $ x + 65
    | otherwise = error "Argument is not between 0 and 25 in intToChar."

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = fromJust $ elemIndex x xs

cycleChar :: Char -> Char
cycleChar c
    | c == 'Z' = 'A'
    | isAsciiUpper c = succ c
    | otherwise = error "Argument is not between 'A' and 'Z' in cycleChar."

cycleList :: Eq a => [a] -> a -> a
cycleList [x] _ = x
cycleList xs a
    | a == last xs = head xs
    | otherwise = xs !! (1 + getIndex a xs)

-- perform k-permutation
kperm :: Eq a => Int -> [a] -> [[a]]
kperm 0 _ = [[]]
kperm k xs = [x:ys | x <- xs, ys <- kperm (k-1) (delete x xs)]

-- setting generation functions -----------------------------------------------
-- this function gets the nth setting for using a next* function
nthElemSetting :: (a -> a) -> Int -> a -> a
nthElemSetting _ 0 start = start
nthElemSetting nextfn n start =
    nthElemSetting nextfn (n-1) (nextfn start)

-- same as cycleList but for string
-- 2 possibilities
nextReflector :: [String] -> String -> String
nextReflector = cycleList

-- get the next location for the ring or the rotor location setting
-- 26*26*26 17576 possibilities
nextRingLoc :: String -> String
nextRingLoc [] = error "empty string in nextRingLoc"
nextRingLoc [a] = [cycleChar a]
nextRingLoc (x:xs)
    | all (== 'Z') xs = cycleChar x : nextRingLoc xs
    | otherwise = x : nextRingLoc xs

-- returns next permutation element for k = 3
-- 5!/3! 60 possibilities
nextRotorType :: Eq a => [a] -> [a] -> [a]
nextRotorType allSettings =
    cycleList (kperm 3 allSettings) -- currently hardcoded to be 3

-- this is used for optional parameter, same as flip fromMaybe
-- (//) :: Maybe a -> a -> a
-- Just x // _ = x
-- Nothing // y = y

-- verification functions -----------------------------------------------------
verifyInput :: String -> Bool
verifyInput = all isAsciiUpper

verifyConfStr :: String -> Bool
verifyConfStr xs =
    verifyInput xs
    && length xs == 26
    && length (nub xs) == 26


