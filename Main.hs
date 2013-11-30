
import Helper
import Enigma

myConf :: Conf
myConf = Conf plugs refB [rtypeI,rtypeII,rtypeIII] ['A','A','A']

myState :: State
myState = ['A','A','A']

main :: IO ()
main = do
  msg <- getLine
  putStrLn (enigma myConf myState msg)


