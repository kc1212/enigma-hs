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

data Direction = Fwd | Bwd deriving (Show, Eq)

data State = State
  { loc1:: Rot_Loc
  , loc2:: Rot_Loc
  , loc3:: Rot_Loc}
  deriving (Show)

data Conf = Conf
  { pb :: PB_Conf
  , refl :: Ref_Conf
  , rtype_1 :: Rot_Type
  , rtype_2 :: Rot_Type
  , rtype_3 :: Rot_Type
  , ring1 :: Rot_Ring
  , ring2 :: Rot_Ring
  , ring3 :: Rot_Ring }
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


rotate_rotor :: Conf -> State -> State
rotate_rotor conf state = state
--   | snd (rtype_1 conf) == loc1 state = 
--     (State 


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
  let new_state = rotate_rotor conf state
  in ( plugboard Bwd (pb conf)
     . rotor Bwd (rtype_1 conf) (ring1 conf) (loc1 state)
     . rotor Bwd (rtype_2 conf) (ring2 conf) (loc2 state)
     . rotor Bwd (rtype_3 conf) (ring3 conf) (loc3 state)
     . reflector (refl conf)
     . rotor Fwd (rtype_3 conf) (ring3 conf) (loc3 state)
     . rotor Fwd (rtype_2 conf) (ring2 conf) (loc2 state)
     . rotor Fwd (rtype_1 conf) (ring1 conf) (loc1 state)
     . plugboard Fwd (pb conf)
     ) c


enigma :: Conf -> State -> String -> String
enigma _ _ [] = []
enigma setting state (x:rest) =
  let new_state = rotate_rotor setting state
      result = enigma_char setting new_state x
  in result : enigma setting new_state rest


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


run_char =
  enigma_char
  (Conf plugs ref_b rtypeI rtypeII rtypeIII 'Z' 'A' 'U')
  (State 'R' 'X' 'C')
run =
  enigma
  (Conf plugs ref_b rtypeI rtypeII rtypeIII 'Z' 'A' 'U')
  (State 'R' 'X' 'C')




