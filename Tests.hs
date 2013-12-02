
import Helper
import Enigma

-- general tests --------------------------------------------------------------
aaaConf = Conf plugs refB [rtypeI,rtypeII,rtypeIII] ['A','A','A']
aaaState = ['A','A','A']

aaa_test_3A = -- encoding those letter will cause two rotors to rotate
  (enigma aaaConf ['A','A','T'] "AAA")
  == "BMU"

aaa_test_48A =
  (enigma aaaConf aaaState (take 48 $ repeat 'A'))
  == "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTH"

aaa_test_48A_decode =
  (enigma aaaConf aaaState "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTH")
  ==
  (take 48 $ repeat 'A')


-- rotor test -----------------------------------------------------------------
rotor_test = (
  ((rotateRotor 0 aaaConf)
  .(rotateRotor 1 aaaConf)
  .(rotateRotor 2 aaaConf)) ['A','A','U']) 
  == ['A','A','V']


-- detailed tests: encode A at rotor pos AAV using default settings -----------
rotor2_fwd_test =
  (rotor Fwd ((getType aaaConf) !! 2) ((getRing aaaConf) !! 2) 'V' 'A')
  ==
  'R'

-- carried forward from rotor2_fwd_test
rotor1_fwd_test =
  (rotor Fwd ((getType aaaConf) !! 1) ((getRing aaaConf) !! 1) 'A' 'R')
  ==
  'G'

-- carried forward from rotor1_fwd_test
rotor0_fwd_test =
  (rotor Fwd ((getType aaaConf) !! 0) ((getRing aaaConf) !! 0) 'A' 'G')
  ==
  'D'

-- reflector
reflector_test =
  (reflector refB 'D')
  ==
  'H'

-- backwards now..
rotor0_bwd_test =
  (rotor Bwd ((getType aaaConf) !! 0) ((getRing aaaConf) !! 0) 'A' 'H')
  ==
  'P'

rotor1_bwd_test =
  (rotor Bwd ((getType aaaConf) !! 1) ((getRing aaaConf) !! 1) 'A' 'P')
  ==
  'U'

rotor2_bwd_test =
  (rotor Bwd ((getType aaaConf) !! 2) ((getRing aaaConf) !! 2) 'V' 'U')
  ==
  'M'



