
module Enigma where

import Helper

type State = [Char] -- state of the enigma machine
data Direction = Fwd | Bwd deriving (Show, Eq)
data RotorSignalDir = SignalIn | SignalOut deriving (Show, Eq)
data Conf = Conf
    { getPlugboard :: String
    , getRefl :: String
    , getType :: [(String, Char)]
    , getRing :: [Char]}
    deriving (Show)

-- enigma machine functions ---------------------------------------------------
-- this is for re-routing the signal to a different character
plugboard :: Direction -> String -> Char -> Char
plugboard Bwd pbConf c = alphs !! getIndex c pbConf
plugboard Fwd pbConf c = pbConf !! charToInt c

-- rotates a single rotor depending on the state and its configuration
rotateRotor :: Int -> Conf -> State -> State
rotateRotor 2 _ state =
    (init state) ++ [cycleChar $ last state] -- move the last element
rotateRotor x conf state
    | cycleChar pawlLoc == rightLoc = part1 ++ (cycleChar currentLoc):part2
    | otherwise = state
    where
        (part1,_:part2) = splitAt x state
        pawlLoc = snd (getType conf !! (x+1))
        rightLoc = state !! (x+1)
        currentLoc = state !! x

-- like the plugboard, also for re-routing the signal
reflector :: String -> Char -> Char
reflector = plugboard Bwd

-- encodes a single character using a single rotor,
-- depends on the signal direction, rotor type, ring setting,
-- rotor location and of course the input character respectively
rotor :: Direction -> (String,Char) -> Char -> Char -> Char -> Char
rotor dir rtype ring loc = -- where rtype is rotor type
    ((rotorOffset SignalOut ring loc)
    .(rotorWiring dir rtype)
    .(rotorOffset SignalIn ring loc))

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
enigmaChar conf state =
    plugboard Bwd (getPlugboard conf)
    . rotor Bwd ((getType conf) !! 2) ((getRing conf) !! 2) (state !! 2)
    . rotor Bwd ((getType conf) !! 1) ((getRing conf) !! 1) (state !! 1)
    . rotor Bwd ((getType conf) !! 0) ((getRing conf) !! 0) (state !! 0)
    . reflector (getRefl conf)
    . rotor Fwd ((getType conf) !! 0) ((getRing conf) !! 0) (state !! 0)
    . rotor Fwd ((getType conf) !! 1) ((getRing conf) !! 1) (state !! 1)
    . rotor Fwd ((getType conf) !! 2) ((getRing conf) !! 2) (state !! 2)
    . plugboard Fwd (getPlugboard conf)

-- runs the enigma machine for a string
enigma :: Conf -> State -> String -> String
enigma _ _ [] = []
enigma conf state (x:rest) =
    let newState =
            ((rotateRotor 0 conf)
            . (rotateRotor 1 conf)
            . (rotateRotor 2 conf)) state
    in (enigmaChar conf newState x) : (enigma conf newState rest)

-- at the moment we're not including plugboard settings
intToSetting :: (Conf,State) -> [(String, Char)] -> [String] -> Integer -> (Conf,State)
intToSetting (conf,state) allRTypes allReflectors n
    | n < 0 = error "4th input (n) cannot be negative"
    | otherwise =
    (Conf newPlugs newRefl newRType newRing, newState)
    where
        newPlugs = getPlugboard conf -- not considered at the moment
        newRefl = nthElemSetting (nextReflector allReflectors) (m `quot` 18534946560) (getRefl conf)
        newRType = nthElemSetting (nextRotorType allRTypes) (m `quot` 308915776) (getType conf)
        newRing = nthElemSetting nextRingLoc (m `quot` 17576) (getRing conf)
        newState = nthElemSetting nextRingLoc (m `rem` 17576) state
        m = n `rem` 37069893120 -- (26^3 * 26^3 * 60 * 2) plugs not included

intToSettingDefault :: Integer -> (Conf, State)
intToSettingDefault =
    intToSetting (Conf plugs refB [rtypeI,rtypeII,rtypeIII] "AAA", "AAA")
        [rtypeI, rtypeII, rtypeIII, rtypeIV, rtypeV] [refB, refC]

-- some default configurations
plugs :: String
refB  :: String
refC  :: String
rtypeI   :: (String, Char)
rtypeII  :: (String, Char)
rtypeIII :: (String, Char)
rtypeIV  :: (String, Char)
rtypeV   :: (String, Char)

plugs    = ['A'..'Z'] -- plug locations, only wires in real enigma
refB     = "YRUHQSLDPXNGOKMIEBFZCWVJAT" -- M3 B reflector
refC     = "FVPJIAOYEDRZXWGCTKUQSBNMHL" -- M3 C reflector
rtypeI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 'Q') -- rotor type I
rtypeII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 'E') -- rotor type II
rtypeIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 'V') -- rotor type III
rtypeIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 'J') -- rotor type IV
rtypeV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", 'Z') -- rotor type V


