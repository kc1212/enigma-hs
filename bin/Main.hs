
import Crypto.Enigma


main :: IO ()
main = do
    msg <- getLine
    putStrLn $ enigma conf state msg
    where
      (conf, state) = intToSettingDefault 0


