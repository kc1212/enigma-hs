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

type PB_Config = (String,String)

data Direction = Fwd | Bwd deriving (Show, Eq)

data State = State
  { rotor1 :: Char, rotor2 :: Char, rotor3 :: Char }
  deriving (Show)

data Conf = Conf
  { pb :: PB_Config, pin1 :: Char, pin2 :: Char, pin3 :: Char }
  deriving (Show)


plugboard :: Direction -> PB_Config -> Char -> Char
plugboard Bwd (xs1,xs2) c =
  plugboard Fwd (xs2,xs1) c
plugboard Fwd (s1:xs1,s2:xs2) c
  | c == s1 = s2
  | otherwise = plugboard Fwd (xs1,xs2) c


rotate_rotor :: Conf -> State -> State
rotate_rotor conf state = State 'a' 'b' 'c' -- dummy


enigma_char :: Conf -> State -> Char -> Char
enigma_char conf state c =
  let new_state = rotate_rotor conf state
  in (plugboard Bwd (pb conf) . (plugboard Fwd (pb conf))) c


enigma :: Conf -> State -> String -> String
enigma _ _ [] = []
enigma setting state (x:rest) =
  let new_state = rotate_rotor setting state
      result = enigma_char setting new_state x
  in result : enigma setting new_state rest


-- verify_conf :: -- need to verify the config type
--
-- my main program...
run_char = enigma_char (Conf ("abcdefg","efgabc") 'a' 'b' 'c') (State 'd' 'e' 'f')
run = enigma (Conf ("abcdefg","efgabc") 'a' 'b' 'c') (State 'd' 'e' 'f')

