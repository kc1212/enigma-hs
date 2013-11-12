
import Enigma

-- test functions
aaa_conf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['A','A','A']
aaa_state = ['A','A','A']

test_conf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['Z','A','U']
test_state = ['R','E','Z']

rotate_test1 = rotate_rotor 1 test_conf test_state
rotate_test0 = rotate_rotor 0 test_conf test_state 

rotate_test3 =
  ((rotate_rotor 2 aaa_conf)
  .(rotate_rotor 1 aaa_conf)
  .(rotate_rotor 0 aaa_conf)) aaa_state

rotor_test1 = rotor Fwd ((rtype test_conf) !! 0) ((ring test_conf) !! 0) (test_state !! 0) 'B'

aaa_test1 = enigma aaa_conf aaa_state "AAAAAAAAAAAAAAAAAAAAA"
aaa_test2 = enigma aaa_conf aaa_state "LCIBFOKQR"


