
module Enigma where

import Helper

-- enigma machine functions ---------------------------------------------------
-- this is for re-routing the signal to a different character
plugboard :: Direction -> String -> Char -> Char
plugboard Bwd pb_conf c = alphs !! (getIndex c pb_conf)
plugboard Fwd pb_conf c = pb_conf !! (charToInt c)

-- rotates a single rotor depending on the state and its configuration
rotateRotor :: Int -> Conf -> State -> State
rotateRotor 2 _ state =
  (init state) ++ [cycleChar $ last state] -- move the last element
rotateRotor x conf state
  | cycleChar pawlLoc == rightLoc = part1 ++ (cycleChar currentLoc):part2
  | otherwise = state
  where
    (part1,_:part2) = splitAt x state
    pawlLoc = snd (get_type conf !! (x+1))
    rightLoc = state !! (x+1)
    currentLoc = state !! x

-- like the plugboard, also for re-routing the signal
reflector :: String -> Char -> Char
reflector refConf c = plugboard Bwd refConf c

-- encodes a single character using a single rotor,
-- depends on the signal direction, rotor type, ring setting,
-- rotor location and of course the input character respectively
rotor :: Direction -> (String,Char) -> Char -> Char -> Char -> Char
rotor dir rtype ring loc c = -- where rtype is rotor type
  ((rotorOffset SignalOut ring loc)
  .(rotorWiring dir rtype)
  .(rotorOffset SignalIn ring loc)) c

-- helper function for rotor: performs the offset
rotorOffset :: RotorSignalDir -> Char -> Char -> Char -> Char
rotorOffset dir ring loc c
  | dir == SignalIn = intToChar $ rem26 $ ic + iloc - iring
  | dir == SignalOut = intToChar $ rem26 $ ic - iloc + iring
  | otherwise = error "Critical error in rotorOffset."
  where
    ic = charToInt c
    iloc = charToInt loc
    iring = charToInt ring

-- helper function for rotor: uses the wiring table to reroute the signal
rotorWiring :: Direction -> (String,Char) -> Char -> Char
rotorWiring Fwd (wiring,_) c =
  wiring !! charToInt c
rotorWiring Bwd (wiring,_) c =
  alphs !! getIndex c wiring

-- this function runs the enigma machine for a single character
-- this does not change the machine state
enigmaChar :: Conf -> State -> Char -> Char
enigmaChar conf state c =
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
        ((rotateRotor 0 conf)
        .(rotateRotor 1 conf)
        .(rotateRotor 2 conf)) state
  in (enigmaChar conf new_state x) : (enigma conf new_state rest)


-- some default configurations
plugs = ['A'..'Z'] -- plug locations, only wires in real enigma
refB    = "YRUHQSLDPXNGOKMIEBFZCWVJAT" -- M3 B reflector
refC    = "FVPJIAOYEDRZXWGCTKUQSBNMHL" -- M3 C reflector
rtypeI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'Q') -- rotor type I
rtypeII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'E') -- rotor type II
rtypeIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'V') -- rotor type III
rtypeIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 'J') -- rotor type IV
rtypeV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", 'Z') -- rotor type V


