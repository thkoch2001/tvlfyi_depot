{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Options.Applicative
import Data.Semigroup ((<>))

import qualified System.Environment as Env
--------------------------------------------------------------------------------

data CommandArgs = CommandArgs
  { hello :: String
  , quiet :: Bool
  , enthusiasm :: Int
  } deriving (Eq, Show)

parseArgs :: Parser CommandArgs
parseArgs =
  CommandArgs <$> strOption
                  ( long "hello"
                 <> metavar "TARGET"
                 <> help "Target for the greeting" )
              <*> switch
                  ( long "quiet"
                 <> short 'q'
                 <> help "Whether to be quiet" )
              <*> option auto
                  ( long "enthusiasm"
                 <> help "How enthusiastic to greet"
                 <> showDefault
                 <> value 1
                 <> metavar "INT" )

main :: IO ()
main = do
  args <- execParser opts
  greet args
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "header - a test for optparse-applicative" )

greet :: CommandArgs -> IO ()
greet CommandArgs{..} = putStrLn $ "Hello, " ++ hello ++ replicate enthusiasm '!'
