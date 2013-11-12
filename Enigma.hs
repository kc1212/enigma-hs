
module Enigma where


import Data.Char (ord, chr)
import Data.Maybe (fromJust)
import Data.List (elemIndex)


type PB_Conf = String
type Ref_Conf = String
type Rot_Ring = Char
type Rot_Loc = Char
type Rot_Type = (String, Char)
type State = [Rot_Loc]

data Direction = Fwd | Bwd deriving (Show, Eq)

data Conf = Conf
  { pb :: PB_Conf
  , refl :: Ref_Conf
  , rtype :: [Rot_Type]
  , ring :: [Rot_Ring]}
  deriving (Show)


-- helper functions
alphs = ['A'..'Z']

rem26 :: Int -> Int
rem26 x = rem (x+52) 26

charToInt :: Char -> Int
charToInt c
  | ((ord c) >= 65) && ((ord c) <= 90) = (ord c) - 65
  | otherwise = error "Not a capital alphabet."

intToChar :: Int -> Char
intToChar x
  | (x >= 0) && (x <= 25) = chr $ x + 65
  | otherwise = error "Argument is not between 65 and 90"

getIndex :: Eq a => a -> [a] -> Int
getIndex x xs = fromJust $ elemIndex x xs

nextChar :: Char -> Char
nextChar c
  | c == 'Z' = 'A'
  | ((ord c) >= 65) && ((ord c) <= 90) = succ c
  | otherwise = error "Argument is not between 'A' and 'Z'"


-- enigma functions
plugboard :: Direction -> PB_Conf -> Char -> Char
plugboard Bwd conf c = alphs !! (getIndex c conf)
plugboard Fwd conf c = conf !! (charToInt c)


rotate_rotor :: Int -> Conf -> State -> State
rotate_rotor 2 _ state =
  (init state) ++ [nextChar $ last state] -- move the last element
rotate_rotor x conf state
  | nextChar pawl_loc == right_loc = part1 ++ (nextChar current_loc):part2
  | otherwise = state
  where
    (part1,_:part2) = splitAt x state
    pawl_loc = snd (rtype conf !! (x+1))
    right_loc = state !! (x+1)
    current_loc = state !! x


reflector :: Ref_Conf -> Char -> Char
reflector conf c = plugboard Bwd conf c


rotor :: Direction -> Rot_Type -> Rot_Ring -> Rot_Loc -> Char -> Char
rotor dir (rt,_) ring loc c -- where rt is rotor type
  | dir == Fwd =
    intToChar (rem26 $ (charToInt fwd_f) - iloc + iring)
  | dir == Bwd =
    intToChar (rem26 $ (charToInt bwd_f) - iloc + iring)
  | otherwise = error "Unknown direction"
  where
    ic = charToInt c
    iloc = charToInt loc
    iring = charToInt ring
    fwd_f = rt!! rem26 (ic + iloc - iring)
    bwd_f = alphs !! getIndex (intToChar (rem26 (ic + iloc - iring))) rt


enigma_char :: Conf -> State -> Char -> Char
enigma_char conf state c =
  ( plugboard Bwd (pb conf)
     . rotor Bwd ((rtype conf) !! 2) ((ring conf) !! 2) (state !! 2)
     . rotor Bwd ((rtype conf) !! 1) ((ring conf) !! 1) (state !! 1)
     . rotor Bwd ((rtype conf) !! 0) ((ring conf) !! 0) (state !! 0)
     . reflector (refl conf)
     . rotor Fwd ((rtype conf) !! 0) ((ring conf) !! 0) (state !! 0)
     . rotor Fwd ((rtype conf) !! 1) ((ring conf) !! 1) (state !! 1)
     . rotor Fwd ((rtype conf) !! 2) ((ring conf) !! 2) (state !! 2)
     . plugboard Fwd (pb conf)
  ) c


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

