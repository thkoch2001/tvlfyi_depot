{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
import Nix.Parser
import Nix.Expr.Types
import Nix.Expr.Types.Annotated
import System.Environment (getArgs)
import System.Exit (die)
import Data.Fix (Fix(..))
import qualified Data.Text as Text
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as A
import Data.Function ((&))
import qualified System.IO as IO
import qualified Text.Megaparsec.Pos as MP

main = do
  (nixFile:_) <- getArgs
  (parseNixFileLoc nixFile :: IO _) >>= \case
    Failure err -> do
      ePutStrLn $ show err
      die "oh no"
    Success expr -> do
      case snd $ match expr of
        NoArguments -> do
          ePutStrLn $ "NoArguments in " <> nixFile
          printPairs mempty
        YesLib vars -> do
          ePutStrLn $ "lib in " <> show vars <> " in " <> nixFile
          printPairs mempty
        NoLib vars srcSpan -> do
          ePutStrLn $ nixFile <> " needs lib added"
          printPairs
            $ "fileName" A..= nixFile
            <> "fromLine" A..= (srcSpan & spanBegin & sourceLine)
            <> "fromColumn" A..= (srcSpan & spanBegin & sourceColumn)
            <> "toLine" A..= (srcSpan & spanEnd & sourceLine)
            <> "toColumn" A..= (srcSpan & spanEnd & sourceColumn)

printPairs pairs = BL.putStrLn $ A.encodingToLazyByteString $ A.pairs pairs

ePutStrLn = IO.hPutStrLn IO.stderr

data Descend = YesDesc | NoDesc
  deriving Show
data Matched =  NoArguments | NoLib [VarName] SrcSpan | YesLib [VarName]
  deriving Show

match :: Fix (Compose (Ann SrcSpan) NExprF) -> (Descend, Matched)
match = \case
  (AnnE outerSpan (NAbs (ParamSet params _ _) (AnnE innerSpan _))) -> (NoDesc,
    let vars = map fst params in
    case (any (== "lib") vars) of
      True -> YesLib vars
      False ->
          -- The span of the arglist is from the beginning of the match
          -- to the beginning of the inner expression
          let varSpan = SrcSpan
                { spanBegin = outerSpan & spanBegin
                -- -1 to prevent the spans from overlapping
                , spanEnd = sourcePosMinus1 (innerSpan & spanBegin) }
          in NoLib vars varSpan)
  _ -> (NoDesc, NoArguments)

-- | Remove one from a source positon.
--
-- That means if the current position is at the very beginning of a line,
-- jump to the previous line.
sourcePosMinus1 :: SourcePos -> SourcePos
sourcePosMinus1 src@(SourcePos { sourceLine, sourceColumn }) =
  let
    col = MP.mkPos $ max (MP.unPos sourceColumn - 1) 1
    line = MP.mkPos $ case MP.unPos sourceColumn of
      1 -> max (MP.unPos sourceLine - 1) 1
      _ -> MP.unPos sourceLine
  in src
    { sourceLine = line
    , sourceColumn = col }
