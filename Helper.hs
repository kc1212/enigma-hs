
module Helper where

import Data.Char (ord, chr, isUpper)
import Data.Maybe (fromJust)
import Data.List (elemIndex, nub, delete)

type State = [Char] -- state of the enigma machine
data Direction = Fwd | Bwd deriving (Show, Eq)
data RotorSignalDir = SignalIn | SignalOut deriving (Show, Eq)
data Conf = Conf
  { get_pb :: String
  , get_refl :: String
  , get_type :: [(String, Char)]
  , get_ring :: [Char]}
  deriving (Show)


-- helper functions -----------------------------------------------------------
alphs :: [Char]
alphs = ['A'..'Z']

rem26 :: Int -> Int
rem26 x = rem (x+52) 26

charToInt :: Char -> Int
charToInt c
  | c >= 'A' && c <= 'Z' = (ord c) - 65
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
  | c >= 'A' && c <= 'Z' = succ c
  | otherwise = error "Argument is not between 'A' and 'Z' in cycleChar."

cycleList :: Eq a => [a] -> a -> a
cycleList [x] _ = x
cycleList xs a
  | a == last xs = head xs
  | otherwise = xs !! (1 + getIndex a xs)


-- setting generation functions -----------------------------------------------
nextSetting :: (Conf,State) -> (Conf,State)
nextSetting (c,s) = (c,s)

-- same as cycleList but for string
nextReflector :: [String] -> String -> String
nextReflector rs r = cycleList rs r

nextRingLoc :: String -> String
nextRingLoc [a] = [cycleChar a]
nextRingLoc (x:xs)
  | head xs == 'Z' = cycleChar x : nextRingLoc xs
  | otherwise = x : nextRingLoc xs

-- returns next permutation element for k = 3
next3Perm:: Eq a => [a] -> [a] -> [a]
next3Perm all setting =
  cycleList (kperm 3 all) setting -- currently hardcoded to be 3

-- perform k-permutation
kperm :: Eq a => Int -> [a] -> [[a]]
kperm 0 _ = [[]]
kperm k xs = [x:ys | x <- xs, ys <- kperm (k-1) (delete x xs)]


-- verification functions -----------------------------------------------------
verifyInput :: String -> Bool
verifyInput = all isUpper

verifyConfStr :: String -> Bool
verifyConfStr xs =
  verifyInput xs
  && length xs == 26
  && length (nub xs) == 26


