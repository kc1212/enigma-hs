
import Enigma

myconf :: Conf
myconf = Conf plugs ref_b [rtypeI,rtypeII,rtypeIII] ['A','A','A']
mystate :: State
mystate = ['A','A','A']

run :: String -> String
run = enigma myconf mystate

main :: IO ()
main = do
  msg <- getLine
  putStrLn (run msg)
