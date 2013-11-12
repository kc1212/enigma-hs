-- Enigma Process
--  1. Convert input letter to number - validate!
--  2. Rotate wheels
--  3. Pass through plugboard
--  4. Pass through right-hand wheel
--  5. Pass through middle wheel
--  6. Pass through left-hand wheel
--  7. Pass through reflector
--  8. Pass through left-hand wheel
--  9. Pass through middle wheel
-- 10. Pass through right-hand wheel
-- 11. Pass through plugboard
-- 12. Convert to output letter

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
rotate_rotor 0 _ state =
  (nextChar (state !! 0)) : tail state
rotate_rotor x conf state 
  | (snd (rtype conf !! x)) == current_char = part1 ++ (nextChar current_char):part2
  | otherwise = state
  where
    (part1,_:part2) = splitAt x state
    current_char = state !! x



reflector :: Ref_Conf -> Char -> Char
reflector conf c = plugboard Bwd conf c


rotor :: Direction -> Rot_Type -> Rot_Ring -> Rot_Loc -> Char -> Char
rotor dir (rtype,_) ring loc c
  | dir == Fwd =
    intToChar (rem26 $ (charToInt fwd_f) - iloc + iring)
  | dir == Bwd =
    intToChar (rem26 $ (charToInt bwd_f) - iloc + iring)
  | otherwise = error "Unknown direction"
  where
    ic = charToInt c
    iloc = charToInt loc
    iring = charToInt ring
    fwd_f = rtype !! rem26 (ic + iloc - iring)
    bwd_f = alphs !! getIndex (intToChar (rem26 (ic + iloc - iring))) rtype


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
  let
    new_state =
      ((rotate_rotor 2 conf)
      .(rotate_rotor 1 conf)
      .(rotate_rotor 0 conf)) state
    result = enigma_char conf new_state x
  in result : enigma conf new_state rest


-- verify_conf :: -- need to verify the config type
--
-- my main program...
plugs    = "QWERTYUIOPASDFGHJKLZXCVBNM" -- plug locations
ref_b    = "YRUHQSLDPXNGOKMIEBFZCWVJAT" -- M3 B reflector
ref_c    = "FVPJIAOYEDRZXWGCTKUQSBNMHL" -- M3 C reflector
rtypeI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'Q') -- rotor type I
rtypeII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'E') -- rotor type II
rtypeIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'V') -- rotor type III
rtypeIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 'J') -- rotor type IV
rtypeV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", 'Z') -- rotor type V
myconf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['Z','A','U']
mystate = ['R','X','C']

run_char = enigma_char myconf mystate
run = enigma myconf mystate



-- test functions
testconf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['A','A','A']
teststate = ['A','A','A']
rotate_test1 = rotate_rotor 1 myconf ['R','E','Z']
rotate_test0 = rotate_rotor 0 myconf ['R','E','Z']
rotate_test3 =
  ((rotate_rotor 2 testconf)
  .(rotate_rotor 1 testconf)
  .(rotate_rotor 0 testconf)) teststate
rotor_test1 = rotor Fwd ((rtype myconf) !! 0) ((ring testconf) !! 0) (teststate !! 0) 'B'


