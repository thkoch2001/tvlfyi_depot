{-# LANGUAGE QuasiQuotes #-}

module Netencode.Parse where

import Control.Category qualified
import Control.Selective (Selective)
import Data.Error.Tree
import Data.Fix (Fix (..))
import Data.Functor.Compose
import Data.List qualified as List
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Semigroupoid qualified as Semigroupiod
import Data.Semigroupoid qualified as Semigroupoid
import Data.Text qualified as Text
import Label
import Netencode qualified
import PossehlAnalyticsPrelude
import Prelude hiding (log)

newtype Parse from to
  = -- TODO: the way @Context = [Text]@ has to be forwarded to everything is kinda shitty.
    -- This is essentially just a difference list, and can probably be treated as a function in the output?
    Parse (([Text], from) -> Validation (NonEmpty ErrorTree) ([Text], to))
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ( Compose
                ((->) ([Text], from))
                (Validation (NonEmpty ErrorTree))
            )
            ((,) [Text])
        )

runParse :: Error -> Parse from to -> from -> Either ErrorTree to
runParse errMsg parser t =
  (["$"], t)
    & runParse' parser
    <&> snd
    & first (nestedMultiError errMsg)
    & validationToEither

runParse' :: Parse from to -> ([Text], from) -> Validation (NonEmpty ErrorTree) ([Text], to)
runParse' (Parse f) from = f from

instance Semigroupoid Parse where
  o p2 p1 = Parse $ \from -> case runParse' p1 from of
    Failure err -> Failure err
    Success to1 -> runParse' p2 to1

instance Category Parse where
  (.) = Semigroupoid.o
  id = Parse $ \t -> Success t

parseEither :: (([Text], from) -> Either ErrorTree ([Text], to)) -> Parse from to
parseEither f = Parse $ \from -> f from & eitherToListValidation

tAs :: (Netencode.TF (Fix Netencode.TF) -> Either ([Text] -> ErrorTree) to) -> Parse Netencode.T to
tAs f = parseEither ((\(context, Netencode.T (Fix tf)) -> f tf & bimap ($ context) (context,)))

key :: Text -> Parse (NEMap Text to) to
key name = parseEither $ \(context, rec) ->
  rec
    & NEMap.lookup name
    & annotate (errorTreeContext (showContext context) [fmt|Key "{name}" does not exist|])
    <&> (addContext name context,)

showContext :: [Text] -> Text
showContext context = context & List.reverse & Text.intercalate "."

addContext :: a -> [a] -> [a]
addContext = (:)

asText :: Parse Netencode.T Text
asText = tAs $ \case
  Netencode.Text t -> pure t
  other -> typeError "of text" other

asBytes :: Parse Netencode.T ByteString
asBytes = tAs $ \case
  Netencode.Bytes b -> pure b
  other -> typeError "of bytes" other

asRecord :: Parse Netencode.T (NEMap Text (Netencode.T))
asRecord = tAs $ \case
  Netencode.Record rec -> pure (rec <&> Netencode.T)
  other -> typeError "a record" other

typeError :: Text -> Netencode.TF ignored -> (Either ([Text] -> ErrorTree) b)
typeError should is = do
  let otherS = is <&> (\_ -> ("…" :: String)) & show
  Left $ \context -> errorTreeContext (showContext context) [fmt|Value is not {should}, but a {otherS}|]

orThrowParseError ::
  Parse (Either Error to) to
orThrowParseError = Parse $ \case
  (context, Left err) ->
    err
      & singleError
      & errorTreeContext (showContext context)
      & singleton
      & Failure
  (context, Right to) -> Success (context, to)
