{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.User
import System.Process
import Text.Printf
--------------------------------------------------------------------------------

-- TODO(wpcarro): Support optional arguments.
-- TODO(wpcarro): Support default values for arguments.
data Arguments = Arguments
  { hostname :: String
  , depotPath :: FilePath
  , stateDir :: FilePath
  , gitRemote :: String
  , useCanon :: Bool
  , dryRun :: Bool
  } deriving (Eq, Show)

parseArgs :: Parser Arguments
parseArgs = Arguments
  <$> strOption
      ( long "hostname"
     <> metavar "HOST"
     <> help "The hostname for which a given NixOS will be built. This is passed to ops.nixos.findSystem, and it defaults to your current machine's hostname." )
  <*> strOption
      ( long "depot"
     <> metavar "PATH"
     <> help "Path to the local copy of the depot repository." )
  <*> strOption
      ( long "state-directory"
     <> metavar "PATH"
     <> help "Path to the directory hosting this tool's runtime state." )
  <*> strOption
      ( long "git-remote"
     <> metavar "URL"
     <> help "URL of git remote to fetch." )
  <*> switch
      ( long "canon"
     <> help "Rebuild your system from the latest revision on origin/canon." )
  <*> switch
      ( long "dry-run"
     <> help "Set this to avoid calling switch-to-configuration after rebuilding." )

buildNixOSFor :: Arguments -> IO ()
buildNixOSFor Arguments{..} = do
  system <- readProcess "nix-build" ["-E", ""] []
  readProcess "nix-env" ["-p", "/nix/var/nix/profiles/system", "--set", system] []
  if not dryRun then do
    readProcess (printf "%s/bin/switch-to-configuration" system) ["switch"] []
    pure ()
  else
    exitSuccess


handleLocal :: Arguments -> IO ()
handleLocal args@Arguments{..} =
  buildNixOSFor args

-- TODO(wpcarro): Refactor to use a reader monad.
handleRemote :: Arguments -> IO ()
handleRemote args@Arguments{..} = do
  createDirectory stateDir
  let depot = stateDir </> "depot.git"
      worktree = stateDir </> "build"

  readProcess "git" ["clone", "--bare", gitRemote, depot] []
  -- cleanup
  readProcess "git" ["worktree", "remove", worktree] []

  readProcess "git" ["-C", depot, "fetch", "origin"] []
  readProcess "git" ["-C", depot, "worktree", "add", "--force", worktree, "FETCH_HEAD"] []

  -- Without this switch-to-configuration attempts to install NixOS in stateDir
  setCurrentDirectory "/"
  buildNixOSFor args


main :: IO ()
main = do
  args@Arguments{..} <- execParser opts

  userID <- getRealUserID
  case userID of
    0 -> do
      -- TODO(wpcarro): read hostname if flag is unset
      putStrLn $ printf "Rebuilding NixOS for %q..." hostname
      if useCanon then handleRemote args else handleLocal args

    _ -> do
      putStrLn "Oh no! Only root is allowed to run upgrade-system!"
      exitFailure

  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Rebuild your NixOS from within depot"
     <> header "rebuild-system - depot's nixos-rebuild switch"
      )
