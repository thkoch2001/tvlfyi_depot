{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Options.Applicative
import Data.Semigroup ((<>))

import qualified Transforms
import qualified Keyboard
import qualified App
--------------------------------------------------------------------------------

data CommandArgs = CommandArgs
  { transforms :: String
  , passage :: String
  } deriving (Eq, Show)

parseArgs :: Parser CommandArgs
parseArgs =
  CommandArgs <$> strOption
                  ( long "transforms"
                 <> short 't'
                 <> help "String of transforms where (e.g. \"HHVS12VHVHS3\")" )
              <*> strOption
                  ( long "passage"
                 <> short 'p'
                 <> help "Input text to re-type" )

main :: IO ()
main = do
  CommandArgs{..} <- execParser opts
  case Transforms.fromString transforms of
    Nothing -> putStrLn "You must provide valid input (e.g. \"HHVS12VHVHS3\")"
    Just xs -> do
      let keyboard = foldl App.transform Keyboard.qwerty (Transforms.optimize xs)
      putStrLn $ "Typing: \"" ++ passage ++ "\"\nOn this keyboard:\n" ++ show keyboard
      case App.retypePassage passage keyboard of
        Nothing -> putStrLn $ "Looks like at least one of the characters in your input passage doesn't fit on our QWERTY keyboard: \n" ++ show Keyboard.qwerty
        Just result -> putStrLn $ "Result: " ++ result
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Transform a QWERTY keyboard using a string of commands")
