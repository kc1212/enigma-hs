
import Crypto.Enigma
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (Arbitrary(..), Property, Result(..),
                        quickCheckResult,
                        choose, elements,
                        listOf1, shuffle,
                        vectorOf, (===))

-- general tests --------------------------------------------------------------
aaaConf :: Conf
aaaConf = Conf plugs refB [rtypeI,rtypeII,rtypeIII] "AAA"

aaaState :: String
aaaState = "AAA"

-- encoding those letter will cause two rotors to rotate
aaaTest3A :: Bool
aaaTest3A = enigma aaaConf "AAT" "AAA" == "BMU"

aaaTest48A :: Bool
aaaTest48A =
    enigma aaaConf aaaState (replicate 48 'A')
    == "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTH"

aaaTest48ADecode :: Bool
aaaTest48ADecode =
    enigma aaaConf aaaState "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTH"
    == replicate 48 'A'


-- rotor test -----------------------------------------------------------------
rotorTest :: Bool
rotorTest =
    ((rotateRotor 0 aaaConf) . (rotateRotor 1 aaaConf) . (rotateRotor 2 aaaConf)) "AAU"
    == "AAV"


-- detailed tests: encode A at rotor pos AAV using default settings -----------
rotor2FwdTest :: Bool
rotor2FwdTest =
    (rotor Fwd ((getType aaaConf) !! 2) ((getRing aaaConf) !! 2) 'V' 'A')
    == 'R'

-- carried forward from rotor2FwdTest
rotor1FwdTest :: Bool
rotor1FwdTest =
    (rotor Fwd ((getType aaaConf) !! 1) ((getRing aaaConf) !! 1) 'A' 'R')
    == 'G'

-- carried forward from rotor1FwdTest
rotor0FwdTest :: Bool
rotor0FwdTest =
    (rotor Fwd ((getType aaaConf) !! 0) ((getRing aaaConf) !! 0) 'A' 'G')
    == 'D'

-- reflector
reflectorTest :: Bool
reflectorTest = reflector refB 'D' == 'H'

-- backwards now..
rotor0BwdTest :: Bool
rotor0BwdTest =
    (rotor Bwd ((getType aaaConf) !! 0) ((getRing aaaConf) !! 0) 'A' 'H')
    == 'P'

rotor1BwdTest :: Bool
rotor1BwdTest =
    (rotor Bwd ((getType aaaConf) !! 1) ((getRing aaaConf) !! 1) 'A' 'P')
    == 'U'

rotor2BwdTest :: Bool
rotor2BwdTest =
    (rotor Bwd ((getType aaaConf) !! 2) ((getRing aaaConf) !! 2) 'V' 'U')
    == 'M'

manualTests :: Bool
manualTests = and
    [ aaaTest3A
    , aaaTest48A
    , aaaTest48ADecode
    , rotorTest
    , rotor2FwdTest
    , rotor1FwdTest
    , rotor0FwdTest
    , reflectorTest
    , rotor0BwdTest
    , rotor1BwdTest
    , rotor2BwdTest
    ]


newtype TestConf = TestConf Conf deriving (Show)

instance Arbitrary TestConf where
    arbitrary = do
        let rotors = take 3 <$> shuffle [ rtypeI
                                        , rtypeII
                                        , rtypeIII
                                        , rtypeIV
                                        , rtypeV ]

        conf <- Conf <$> return plugs
                     <*> elements [refB, refC]
                     <*> rotors
                     <*> vectorOf 3 (elements plugs)

        return . TestConf $ conf

-- | Used to satisfy the precondition that messages must consist of
-- capital Chars only.
newtype TestMsg = TestMsg String deriving (Show)
instance Arbitrary TestMsg where
    arbitrary = TestMsg <$> listOf1 (choose ('A','Z'))

-- | A message encoded two times is the original message.
prop_reversible :: TestConf -> TestMsg -> Property
prop_reversible (TestConf conf) (TestMsg msg) =
    let state = getRing conf
    in (msg === enigma conf state (enigma conf state msg))

main :: IO ()
main = do
    result <- quickCheckResult prop_reversible
    if manualTests && isSuccess result then exitSuccess else exitFailure

isSuccess :: Result -> Bool
isSuccess Success {} = True
isSuccess _ = False
