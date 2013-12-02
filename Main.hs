
import Enigma

confs = intToSettingDefault 0

main :: IO ()
main = do
  msg <- getLine
  putStrLn (enigma (fst confs) (snd confs) msg)


