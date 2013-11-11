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


type PB_Config = String
type Ref_Config = String
type Rot_Config = Char
type Rot_Loc = Char

data Direction = Fwd | Bwd deriving (Show, Eq)

data State = State
  { rotor1 :: Rot_Loc
  , rotor2 :: Rot_Loc
  , rotor3 :: Rot_Loc}
  deriving (Show)

data Conf = Conf
  { pb :: PB_Config
  , refl :: Ref_Config
  , pin1 :: Rot_Config
  , pin2 :: Rot_Config
  , pin3 :: Rot_Config }
  deriving (Show)


charToInt :: Char -> Int
charToInt c
  | ((ord c) >= 65) && ((ord c) <= 90) = (ord c) - 65
  | otherwise = error "Not a capital alphabet."


intToChar :: Int -> Char
intToChar x
  | (x >= 0) && (x <= 25) = chr $ x + 65
  | otherwise = error "Argument is not between 65 and 90"


plugboard :: Direction -> PB_Config -> Char -> Char
plugboard Bwd conf c = ['A'..'Z'] !! (fromJust $ elemIndex c conf)
plugboard Fwd conf c = conf !! (charToInt c)


rotate_rotor :: Conf -> State -> State
rotate_rotor conf state = State 'a' 'b' 'c' -- dummy


reflector :: Ref_Config -> Char -> Char
reflector conf c = plugboard Bwd conf c


-- rotor :: Rot_Config -> Rot_Loc -> Char -> Char
-- rotor conf loc c = 


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
plugs = "QWERTYUIOPASDFGHJKLZXCVBNM"
ref_b = "YRUHQSLDPXNGOKMIEBFZCWVJAT" -- M3 B
ref_c = "FVPJIAOYEDRZXWGCTKUQSBNMHL" -- M3 C

run_char =
  enigma_char
  (Conf plugs ref_b 'a' 'b' 'c')
  (State 'd' 'e' 'f')
run =
  enigma
  (Conf plugs ref_b 'a' 'b' 'c') 
  (State 'd' 'e' 'f')


