
import Enigma

myconf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['A','A','A']
mystate = ['A','A','A']

run = enigma myconf mystate

main = do
  msg <- getLine
  putStrLn (run msg)
