
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
  { get_pb :: PB_Conf
  , get_refl :: Ref_Conf
  , get_type :: [Rot_Type]
  , get_ring :: [Rot_Ring]}
  deriving (Show)


-- helper functions
alphs :: [Char]
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
    pawl_loc = snd (get_type conf !! (x+1))
    right_loc = state !! (x+1)
    current_loc = state !! x


reflector :: Ref_Conf -> Char -> Char
reflector conf c = plugboard Bwd conf c


rotor :: Direction -> Rot_Type -> Rot_Ring -> Rot_Loc -> Char -> Char
rotor dir (tpe,_) ring loc c = -- where tpe is rotor type
  intToChar (rem26 $ (charToInt code) - iloc + iring)
  where
    ic = charToInt c
    iloc = charToInt loc
    iring = charToInt ring
    code = if (dir == Fwd) -- for intermediate calculation
        then tpe !! rem26 (ic + iloc - iring)
        else alphs !! getIndex ((intToChar.rem26) $ ic + iloc - iring) tpe



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

