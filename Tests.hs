
import Enigma

-- general tests
-------------------------------------------------------------------------------
aaa_conf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['A','A','A']
aaa_state = ['A','A','A']

aaa_test_1A =
  (enigma aaa_conf ['A','A','T'] "AAA")
  == "BMU"

aaa_test_48A =
  (enigma aaa_conf aaa_state (take 48 $ repeat 'A'))
  == "BDZGOWCXLTKSBTMCDLPBMUQOFXYHCXTGYJFLINHNXSHIUNTH"


-- rotor test
-------------------------------------------------------------------------------
rotor_test = (
  ((rotate_rotor 0 aaa_conf)
  .(rotate_rotor 1 aaa_conf)
  .(rotate_rotor 2 aaa_conf)) ['A','A','U']) 
  == ['A','A','V']


-- detailed tests: encode A at rotor pos AAV using default settings
-------------------------------------------------------------------------------
rotor2_fwd_test =
  (rotor Fwd ((get_type aaa_conf) !! 2) ((get_ring aaa_conf) !! 2) 'V' 'A')
  ==
  'R'

-- carried forward from rotor2_fwd_test
rotor1_fwd_test =
  (rotor Fwd ((get_type aaa_conf) !! 1) ((get_ring aaa_conf) !! 1) 'A' 'R')
  ==
  'G'

-- carried forward from rotor1_fwd_test
rotor0_fwd_test =
  (rotor Fwd ((get_type aaa_conf) !! 0) ((get_ring aaa_conf) !! 0) 'A' 'G')
  ==
  'D'

-- reflector
reflector_test =
  (reflector ref_b 'D')
  ==
  'H'

-- backwards now..
rotor0_bwd_test =
  (rotor Bwd ((get_type aaa_conf) !! 0) ((get_ring aaa_conf) !! 0) 'A' 'H')
  ==
  'P'

rotor1_bwd_test =
  (rotor Bwd ((get_type aaa_conf) !! 1) ((get_ring aaa_conf) !! 1) 'A' 'P')
  ==
  'U'

rotor2_bwd_test =
  (rotor Bwd ((get_type aaa_conf) !! 2) ((get_ring aaa_conf) !! 2) 'V' 'U')
  ==
  'M'




