{-# LANGUAGE RecordWildCards #-}
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import Control.Exception
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.Exit
import System.FilePath
import System.Posix.User
import System.Process
import Text.Printf
--------------------------------------------------------------------------------

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
     <> showDefault
     <> value "/depot"
     <> help "Path to the local copy of the depot repository." )
  <*> strOption
      ( long "state-directory"
     <> metavar "PATH"
     <> showDefault
     <> value "/var/lib/rebuild-system"
     <> help "Path to the directory hosting this tool's runtime state." )
  <*> strOption
      ( long "git-remote"
     <> metavar "URL"
     <> showDefault
     <> value "https://cl.tvl.fyi/depot.git"
     <> help "URL of git remote to fetch." )
  <*> switch
      ( long "canon"
     <> help "Rebuild your system from the latest revision on origin/canon." )
  <*> switch
      ( long "dry-run"
     <> help "Set this to avoid calling switch-to-configuration after rebuilding." )

buildNixOSFor :: Bool -> FilePath -> String -> IO ()
buildNixOSFor dryRun depotPath hostname = do
  system <- rstrip <$> readProcess "nix-build" ["-E", printf "((import %s {}).ops.nixos.findSystem \"%s\").system" depotPath hostname, "--no-out-link", "--show-trace"] []
  if dryRun then
    exitSuccess
  else do
    callProcess "nix-env" ["-p", "/nix/var/nix/profiles/system", "--set", system]
    callProcess (printf "%s/bin/switch-to-configuration" system) ["switch"]
  where
    rstrip = reverse . drop 1 . reverse

handleLocal :: Arguments -> IO ()
handleLocal args@Arguments{..} =
  buildNixOSFor dryRun depotPath hostname

handleRemote :: Arguments -> IO ()
handleRemote args@Arguments{..} = do
  createDirectory stateDir
  callProcess "git" ["clone", "--bare", gitRemote, depot]
  (do callProcess "git" ["-C", depot, "fetch", "origin"]
      callProcess "git" ["-C", depot, "worktree", "add", "--force", worktree, "FETCH_HEAD"]
      setCurrentDirectory "/"
      buildNixOSFor dryRun worktree hostname)
    `finally` cleanup
  where
    depot = stateDir </> "depot.git"
    worktree = stateDir </> "build"
    cleanup = callProcess "git" ["-C", depot, "worktree", "remove", worktree]

main :: IO ()
main = do
  args@Arguments{..} <- execParser opts
  userID <- getRealUserID
  case userID of
    0 -> do
      putStrLn $ printf "Rebuilding NixOS for %s..." hostname
      if useCanon then handleRemote args else handleLocal args
    _ -> do
      putStrLn "Oh no! Only root is allowed to run upgrade-system!"
      exitFailure
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Rebuild your NixOS from within depot."
     <> header "rebuild-system - depot's nixos-rebuild switch"
      )
