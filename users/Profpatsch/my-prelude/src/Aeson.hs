{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module Aeson where

import Data.Aeson (Value (..))
import Data.Aeson.BetterErrors qualified as Json
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Error.Tree
import Data.Maybe (catMaybes)
import Data.Vector qualified as Vector
import Label
import PossehlAnalyticsPrelude
import Test.Hspec (describe, it, shouldBe)
import Test.Hspec qualified as Hspec

-- | Convert a 'Json.ParseError' to a corresponding 'ErrorTree'
parseErrorTree :: Error -> Json.ParseError Error -> ErrorTree
parseErrorTree contextMsg errs =
  errs
    & Json.displayError prettyError
    <&> newError
    & nonEmpty
    & \case
      Nothing -> singleError contextMsg
      Just errs' -> errorTree contextMsg errs'

-- | Parse a key from the object, à la 'Json.key', return a labelled value.
--
-- We don’t provide a version that infers the json object key,
-- since that conflates internal naming with the external API, which is dangerous.
--
-- @@
-- do
--   txt <- keyLabel @"myLabel" "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" Text)
-- @@
keyLabel ::
  forall label err m a.
  Monad m =>
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label a)
keyLabel = do
  keyLabel' (Proxy @label)

-- | Parse a key from the object, à la 'Json.key', return a labelled value.
-- Version of 'keyLabel' that requires a proxy.
--
-- @@
-- do
--   txt <- keyLabel' (Proxy @"myLabel") "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" Text)
-- @@
keyLabel' ::
  forall label err m a.
  Monad m =>
  Proxy label ->
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label a)
keyLabel' Proxy key parser = label @label <$> Json.key key parser

-- | Parse an optional key from the object, à la 'Json.keyMay', return a labelled value.
--
-- We don’t provide a version that infers the json object key,
-- since that conflates internal naming with the external API, which is dangerous.
--
-- @@
-- do
--   txt <- keyLabelMay @"myLabel" "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" (Maybe Text))
-- @@
keyLabelMay ::
  forall label err m a.
  Monad m =>
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label (Maybe a))
keyLabelMay = do
  keyLabelMay' (Proxy @label)

-- | Parse an optional key from the object, à la 'Json.keyMay', return a labelled value.
-- Version of 'keyLabelMay' that requires a proxy.
--
-- @@
-- do
--   txt <- keyLabelMay' (Proxy @"myLabel") "jsonKeyName" Json.asText
--   pure (txt :: Label "myLabel" (Maybe Text))
-- @@
keyLabelMay' ::
  forall label err m a.
  Monad m =>
  Proxy label ->
  Text ->
  Json.ParseT err m a ->
  Json.ParseT err m (Label label (Maybe a))
keyLabelMay' Proxy key parser = label @label <$> Json.keyMay key parser

-- | Like 'Json.key', but allows a list of keys that are tried in order.
--
-- This is intended for renaming keys in an object.
-- The first key is the most up-to-date version of a key, the others are for backward-compatibility.
--
-- If a key (new or old) exists, the inner parser will always be executed for that key.
keyRenamed :: Monad m => NonEmpty Text -> Json.ParseT err m a -> Json.ParseT err m a
keyRenamed (newKey :| oldKeys) inner =
  keyRenamedTryOldKeys oldKeys inner >>= \case
    Nothing -> Json.key newKey inner
    Just parse -> parse

-- | Like 'Json.keyMay', but allows a list of keys that are tried in order.
--
-- This is intended for renaming keys in an object.
-- The first key is the most up-to-date version of a key, the others are for backward-compatibility.
--
-- If a key (new or old) exists, the inner parser will always be executed for that key.
keyRenamedMay :: Monad m => NonEmpty Text -> Json.ParseT err m a -> Json.ParseT err m (Maybe a)
keyRenamedMay (newKey :| oldKeys) inner =
  keyRenamedTryOldKeys oldKeys inner >>= \case
    Nothing -> Json.keyMay newKey inner
    Just parse -> Just <$> parse

-- | Helper function for 'keyRenamed' and 'keyRenamedMay' that returns the parser for the first old key that exists, if any.
keyRenamedTryOldKeys :: Monad m => [Text] -> Json.ParseT err m a -> Json.ParseT err m (Maybe (Json.ParseT err m a))
keyRenamedTryOldKeys oldKeys inner = do
  oldKeys & traverse tryOld <&> catMaybes <&> nonEmpty <&> \case
    Nothing -> Nothing
    Just (old :| _moreOld) -> Just old
  where
    tryOld key =
      Json.keyMay key (pure ()) <&> \case
        Just () -> Just $ Json.key key inner
        Nothing -> Nothing

test_keyRenamed :: Hspec.Spec
test_keyRenamed = do
  describe "keyRenamed" $ do
    let parser = keyRenamed ("new" :| ["old"]) Json.asText
    let p = Json.parseValue @() parser
    it "accepts the new key and the old key" $ do
      p (Object (KeyMap.singleton "new" (String "text")))
        `shouldBe` (Right "text")
      p (Object (KeyMap.singleton "old" (String "text")))
        `shouldBe` (Right "text")
    it "fails with the old key in the error if the inner parser is wrong" $ do
      p (Object (KeyMap.singleton "old" Null))
        `shouldBe` (Left (Json.BadSchema [Json.ObjectKey "old"] (Json.WrongType Json.TyString Null)))
    it "fails with the new key in the error if the inner parser is wrong" $ do
      p (Object (KeyMap.singleton "new" Null))
        `shouldBe` (Left (Json.BadSchema [Json.ObjectKey "new"] (Json.WrongType Json.TyString Null)))
    it "fails if the key is missing" $ do
      p (Object KeyMap.empty)
        `shouldBe` (Left (Json.BadSchema [] (Json.KeyMissing "new")))
  describe "keyRenamedMay" $ do
    let parser = keyRenamedMay ("new" :| ["old"]) Json.asText
    let p = Json.parseValue @() parser
    it "accepts the new key and the old key" $ do
      p (Object (KeyMap.singleton "new" (String "text")))
        `shouldBe` (Right (Just "text"))
      p (Object (KeyMap.singleton "old" (String "text")))
        `shouldBe` (Right (Just "text"))
    it "allows the old and new key to be missing" $ do
      p (Object KeyMap.empty)
        `shouldBe` (Right Nothing)

-- | Create a json array from a list of json values.
jsonArray :: [Value] -> Value
jsonArray xs = xs & Vector.fromList & Array
