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
  } deriving (Eq, Show)

parseArgs :: Parser CommandArgs
parseArgs =
  CommandArgs <$> strOption
                  ( long "transforms"
                 <> short 't'
                 <> help "String of transforms where (e.g. \"HHVS12VHVHS3\")" )

main :: IO ()
main = do
  CommandArgs{..} <- execParser opts
  case Transforms.fromString transforms of
    Nothing -> putStrLn "You must provide valid input (e.g. \"HHVS12VHVHS3\")"
    Just xs -> print $ foldl App.transform Keyboard.qwerty xs
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Transform a QWERTY keyboard using a string of commands")
