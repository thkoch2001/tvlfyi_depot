{-# LANGUAGE QuasiQuotes #-}

module Main where

import Conduit ((.|))
import Data.Binary.Builder qualified as Builder
import Data.Conduit qualified as Cond
import Data.Conduit.Combinators qualified as Cond
import Data.Conduit.Process.Typed qualified as Cond
import Data.Conduit.Process.Typed qualified as Proc
import Data.List qualified as List
import Data.Text qualified as Text
import Network.HTTP.Types qualified as Http
import Network.Wai qualified as Wai
import Network.Wai.Conduit qualified as Wai.Conduit
import Network.Wai.Handler.Warp qualified as Warp
import PossehlAnalyticsPrelude
import System.Directory qualified as Dir
import System.FilePath ((</>))
import System.FilePath qualified as File
import System.Posix qualified as Unix

-- Webserver that returns folders under CWD as .zip archives (recursively)
main :: IO ()
main = do
  currentDirectory <- Dir.getCurrentDirectory >>= Dir.canonicalizePath
  run currentDirectory

run :: FilePath -> IO ()
run dir = do
  currentDirectory <- Dir.canonicalizePath dir
  putStderrLn $ [fmt|current {show currentDirectory}|]
  Warp.run 7070 $ \req respond -> do
    let respondHtml status content = respond $ Wai.responseLBS status [("Content-Type", "text/html")] content
    case req & Wai.pathInfo of
      [] -> respond $ Wai.responseLBS Http.status200 [("Content-Type", "text/html")] "any directory will be returned as .zip!"
      filePath -> do
        wantedFilepath <- Dir.canonicalizePath (currentDirectory </> (File.joinPath (filePath <&> textToString)))
        putStderrLn $ [fmt|wanted {show wantedFilepath}|]
        -- I hope this prevents any shenanigans lol
        if
            | (any (Text.elem '/') filePath) -> putStderrLn "tried %2F encoding" >> respondHtml Http.status400 "no"
            | not (currentDirectory `List.isPrefixOf` wantedFilepath) -> putStderrLn "tried parent dir with .." >> respondHtml Http.status400 "no^2"
            | otherwise -> do
                ex <- Unix.fileExist wantedFilepath
                if ex
                  then do
                    status <- Unix.getFileStatus wantedFilepath
                    if status & Unix.isDirectory
                      then do
                        zipDir <- zipDirectory wantedFilepath
                        Proc.withProcessWait zipDir $ \process -> do
                          let stream =
                                Proc.getStdout process
                                  .| Cond.map (\bytes -> Cond.Chunk $ Builder.fromByteString bytes)
                          -- TODO: how to handle broken zip? Is it just gonna return a 500? But the stream is already starting, so hard!
                          respond $ Wai.Conduit.responseSource Http.ok200 [("Content-Type", "application/zip")] stream
                      else respondHtml Http.status404 "not found"
                  else respondHtml Http.status404 "not found"
  where
    zipDirectory toZipDir = do
      putStderrLn [fmt|running $ zip {show ["--recurse-paths", "-", toZipDir]}|]
      pure $
        Proc.proc "zip" ["--recurse-paths", "-", toZipDir]
          & Proc.setStdout Cond.createSource
