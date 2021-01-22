--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import qualified App
import qualified System.Envy as Envy
--------------------------------------------------------------------------------

main :: IO ()
main = do
  mEnv <- Envy.decodeEnv
  case mEnv of
    Left err -> putStrLn err
    Right env -> App.run env
