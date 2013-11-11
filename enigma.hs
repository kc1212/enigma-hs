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
type Rot_Conf = Char
type Rot_Loc = Char
type Rot_Type = String

data Direction = Fwd | Bwd deriving (Show, Eq)

data State = State
  { rotor1 :: Rot_Loc
  , rotor2 :: Rot_Loc
  , rotor3 :: Rot_Loc}
  deriving (Show)

data Conf = Conf
  { pb :: PB_Conf
  , refl :: Ref_Conf
  , rtype_1 :: Rot_Type
  , rtype_2 :: Rot_Type
  , rtype_3 :: Rot_Type
  , ring1 :: Rot_Conf
  , ring2 :: Rot_Conf
  , ring3 :: Rot_Conf }
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


-- enigma functions
plugboard :: Direction -> PB_Conf -> Char -> Char
plugboard Bwd conf c = alphs !! (getIndex c conf)
plugboard Fwd conf c = conf !! (charToInt c)


rotate_rotor :: Conf -> State -> State
rotate_rotor conf state = State 'a' 'b' 'c' -- dummy


reflector :: Ref_Conf -> Char -> Char
reflector conf c = plugboard Bwd conf c


rotor :: Direction -> Rot_Type -> Rot_Conf -> Rot_Loc -> Char -> Char
rotor dir rtype ring loc c
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
     . plugboard Fwd (pb conf)
     . reflector (refl conf)
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
rtypeI   = "EKMFLGDQVZNTOWYHXUSPAIBRCJ" -- rotor type I
rtypeII  = "AJDKSIRUXBLHWTMCQGZNPYFVOE" -- rotor type II
rtypeIII = "BDFHJLCPRTXVZNYEIWGAKMUSQO" -- rotor type III
rtypeIV  = "ESOVPZJAYQUIRHXLNFTGKDCMWB" -- rotor type IV
rtypeV   = "VZBRGITYUPSDNHLXAWMJQOFECK" -- rotor type V


run_char =
  enigma_char
  (Conf plugs ref_b rtypeI rtypeII rtypeIII 'a' 'b' 'c')
  (State 'd' 'e' 'f')
run =
  enigma
  (Conf plugs ref_b rtypeI rtypeII rtypeIII 'a' 'b' 'c') 
  (State 'd' 'e' 'f')




