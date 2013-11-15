
module Enigma where

import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import Data.List (elemIndex)

type State = [Char] -- state of the enigma machine
data Direction = Fwd | Bwd deriving (Show, Eq)
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
  | otherwise = error "Not a capital alphabet."

intToChar :: Int -> Char
intToChar x
  | x >= 0 && x <= 25 = chr $ x + 65
  | otherwise = error "Argument is not between 0 and 25"

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = fromJust $ elemIndex x xs

cycleChar :: Char -> Char
cycleChar c
  | c == 'Z' = 'A'
  | c >= 'A' && c <= 'Z' = succ c
  | otherwise = error "Argument is not between 'A' and 'Z'"


-- enigma machine functions ---------------------------------------------------
-- this is for re-routing the signal to a different character
plugboard :: Direction -> String -> Char -> Char
plugboard Bwd pb_conf c = alphs !! (getIndex c pb_conf)
plugboard Fwd pb_conf c = pb_conf !! (charToInt c)

-- rotates a single rotor depending on the state and its configuration
rotate_rotor :: Int -> Conf -> State -> State
rotate_rotor 2 _ state =
  (init state) ++ [cycleChar $ last state] -- move the last element
rotate_rotor x conf state
  | cycleChar pawl_loc == right_loc = part1 ++ (cycleChar current_loc):part2
  | otherwise = state
  where
    (part1,_:part2) = splitAt x state
    pawl_loc = snd (get_type conf !! (x+1))
    right_loc = state !! (x+1)
    current_loc = state !! x

-- like the plugboard, also for re-routing the signal
reflector :: String -> Char -> Char
reflector ref_conf c = plugboard Bwd ref_conf c

-- encodes a single character using a single rotor,
-- depends on the signal direction, rotor type, ring setting,
-- rotor location and of course the input character respectively
rotor :: Direction -> (String,Char) -> Char -> Char -> Char -> Char
rotor dir (rtype,_) ring loc c = -- where rtype is rotor type
  intToChar (rem26 $ (charToInt code) - iloc + iring)
  where
    ic = charToInt c
    iloc = charToInt loc
    iring = charToInt ring
    code = if (dir == Fwd) -- for intermediate calculation
        then rtype !! rem26 (ic + iloc - iring)
        else alphs !! getIndex ((intToChar.rem26) $ ic + iloc - iring) rtype

-- this function runs the enigma machine for a single character
-- this does not change the machine state
enigma_char :: Conf -> State -> Char -> Char
enigma_char conf state c =
  ( plugboard Bwd (get_pb conf)
     . rotor Bwd ((get_type conf) !! 2) ((get_ring conf) !! 2) (state !! 2)
     . rotor Bwd ((get_type conf) !! 1) ((get_ring conf) !! 1) (state !! 1)
     . rotor Bwd ((get_type conf) !! 0) ((get_ring conf) !! 0) (state !! 0)
     . reflector (get_refl conf)
     . rotor Fwd ((get_type conf) !! 0) ((get_ring conf) !! 0) (state !! 0)
     . rotor Fwd ((get_type conf) !! 1) ((get_ring conf) !! 1) (state !! 1)
     . rotor Fwd ((get_type conf) !! 2) ((get_ring conf) !! 2) (state !! 2)
     . plugboard Fwd (get_pb conf)
  ) c

-- runs the enigma machine for a string
enigma :: Conf -> State -> String -> String
enigma _ _ [] = []
enigma conf state (x:rest) =
  let new_state =
        ((rotate_rotor 0 conf)
        .(rotate_rotor 1 conf)
        .(rotate_rotor 2 conf)) state
  in (enigma_char conf new_state x) : (enigma conf new_state rest)


-- verify_conf :: -- need to verify the config type

-- some default configurations
plugs = ['A'..'Z'] -- plug locations
ref_b    = "YRUHQSLDPXNGOKMIEBFZCWVJAT" -- M3 B reflector
ref_c    = "FVPJIAOYEDRZXWGCTKUQSBNMHL" -- M3 C reflector
rtypeI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'Q') -- rotor type I
rtypeII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'E') -- rotor type II
rtypeIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'V') -- rotor type III
rtypeIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 'J') -- rotor type IV
rtypeV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", 'Z') -- rotor type V

