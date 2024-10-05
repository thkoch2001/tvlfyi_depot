{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.ByteString qualified as ByteString
import Data.Either
import Data.List qualified as List
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified
import MyPrelude
import Numeric.Natural
import Text.HTML.TagSoup qualified as Tag
import Text.Nicify
import Text.Read qualified as Read

parseNat :: Text -> Maybe Natural
parseNat = Read.readMaybe . textToString

printNice :: Show a => a -> IO ()
printNice = putStrLn . nicify . show

type Tag = Tag.Tag Text

main = do
  reverseHtml <- readStdinUtf8
  printNice $ List.sortOn snd $ packagesAndReverseDeps reverseHtml
  where
    readStdinUtf8 = bytesToTextUtf8Lenient <$> ByteString.getContents

-- | reads the table provided by https://packdeps.haskellers.com/reverse
-- figuring out all sections (starting with the link to the package name),
-- then figuring out the name of the package and the first column,
-- which is the number of reverse dependencies of the package
packagesAndReverseDeps :: Text -> [(Text, Natural)]
packagesAndReverseDeps reverseHtml = do
  let tags = Tag.parseTags reverseHtml
  let sections = Tag.partitions (isJust . reverseLink) tags
  let sectionName [] = "<unknown section>"
      sectionName (sect : _) = sect & reverseLink & fromMaybe "<unknown section>"
  let sectionNames = map sectionName sections
  mapMaybe
    ( \(name :: Text, sect) -> do
        reverseDeps <- firstNaturalNumber sect
        pure (sectionPackageName name sect, reverseDeps) :: Maybe (Text, Natural)
    )
    $ zip sectionNames sections
  where
    reverseLink = \case
      Tag.TagOpen "a" attrs -> findMaybe attrReverseLink attrs
      _ -> Nothing

    attrReverseLink = \case
      ("href", lnk) ->
        if
            | "packdeps.haskellers.com/reverse/" `Text.isInfixOf` lnk -> Just lnk
            | otherwise -> Nothing
      _ -> Nothing

    sectionPackageName :: Text -> [Tag] -> Text
    sectionPackageName sectionName = \case
      (_ : Tag.TagText name : _) -> name
      (_ : el : _) -> sectionName
      xs -> sectionName

    firstNaturalNumber :: [Tag] -> Maybe Natural
    firstNaturalNumber =
      findMaybe
        ( \case
            Tag.TagText t -> parseNat t
            _ -> Nothing
        )
