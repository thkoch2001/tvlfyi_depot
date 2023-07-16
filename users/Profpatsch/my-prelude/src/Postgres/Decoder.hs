module Postgres.Decoder where

import Control.Applicative (Alternative)
import Data.Aeson qualified as Json
import Data.Aeson.BetterErrors qualified as Json
import Data.Error.Tree
import Data.Typeable (Typeable)
import Database.PostgreSQL.Simple.FromField qualified as PG
import Database.PostgreSQL.Simple.FromRow qualified as PG
import Json qualified
import Label
import PossehlAnalyticsPrelude

-- | A Decoder of postgres values. Allows embedding more complex parsers (like a 'Json.ParseT').
newtype Decoder a = Decoder (PG.RowParser a)
  deriving newtype (Functor, Applicative, Alternative, Monad)

-- | Turn any type that implements 'PG.fromField' into a 'Decoder'. Use type applications to prevent accidental conversions:
--
-- @
-- fromField @Text :: Decoder Text
-- @
fromField :: PG.FromField a => Decoder a
fromField = Decoder $ PG.fieldWith PG.fromField

-- | Turn any type that implements 'PG.fromField' into a 'Decoder' and wrap the result into the given 'Label'. Use type applications to prevent accidental conversions:
--
-- @
-- fromField @"myField" @Text :: Decoder (Label "myField" Text)
-- @
fromFieldLabel :: forall lbl a. PG.FromField a => Decoder (Label lbl a)
fromFieldLabel = label @lbl <$> fromField

-- | Parse fields out of a json value returned from the database.
--
-- ATTN: The whole json record has to be transferred before it is parsed,
-- so if you only need a tiny bit of it, use `->` and `->>` in your SQL statement
-- and return only the fields you need from the query.
--
-- In that case pay attention to NULL though:
--
-- @
-- SELECT '{"foo": {}}'::jsonb->>'foo' IS NULL
-- → TRUE
-- @
--
-- Also note: `->>` will coerce the json value to @text@, regardless of the content.
-- So the JSON object @{"foo": {}}"@ would be returned as the text: @"{\"foo\": {}}"@.
json :: Typeable a => Json.ParseT ErrorTree Identity a -> Decoder a
json parser = Decoder $ PG.fieldWith $ \field bytes -> do
  val <- PG.fromField @Json.Value field bytes
  case Json.parseValue parser val of
    Left err ->
      PG.returnError
        PG.ConversionFailed
        field
        (err & Json.parseErrorTree "Cannot decode jsonb column" & prettyErrorTree & textToString)
    Right a -> pure a
