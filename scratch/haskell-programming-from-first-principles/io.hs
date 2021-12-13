module IOScratch where

import qualified System.Environment as SE
import qualified System.IO as SIO
--------------------------------------------------------------------------------

docs :: String
docs = "Pass -e to encrypt and -d to decrypt."

encryptStdin :: IO ()
encryptStdin = do
  char <- SIO.hGetChar SIO.stdin
  -- encrypt char
  SIO.hPutStr SIO.stdout [char]

decryptStdin :: IO ()
decryptStdin = do
  char <- SIO.hGetChar SIO.stdin
  -- decrypt char
  SIO.hPutStr SIO.stdout [char]

main :: IO ()
main = do
  args <- SE.getArgs
  case args of
    [] ->
      putStrLn $ "You did not pass enough arguments. " ++ docs
    ["-e"] ->
      encryptStdin
    ["-d"] ->
      decryptStdin
    [x] ->
      putStrLn $ "You passed an unsupported option: " ++ x ++ ". " ++ docs
    _ ->
      putStrLn $ "You passed too many arguments. " ++ docs
