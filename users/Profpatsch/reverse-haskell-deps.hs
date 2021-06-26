{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
import qualified Text.HTML.TagSoup as Tag
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.List as List
import Data.Maybe
import Text.Nicify
import qualified Text.Read as Read
import Numeric.Natural
import Data.Either
import qualified Data.ByteString as ByteString
import qualified Data.Text.Encoding

parseNat :: Text.Text -> Maybe Natural
parseNat = Read.readMaybe . Text.unpack

printNice :: Show a => a -> IO ()
printNice = putStrLn . nicify . show

type Tag = Tag.Tag Text.Text

main = do
  reverseHtml <- readStdinUtf8
  printNice $ List.sortOn snd $ packagesAndReverseDeps reverseHtml

  where
    readStdinUtf8 = Data.Text.Encoding.decodeUtf8 <$> ByteString.getContents

-- | reads the table provided by https://packdeps.haskellers.com/reverse
-- figuring out all sections (starting with the link to the package name),
-- then figuring out the name of the package and the first column,
-- which is the number of reverse dependencies of the package
packagesAndReverseDeps reverseHtml = do
  let tags = Tag.parseTags reverseHtml
  let sections =  Tag.partitions (isJust . reverseLink) tags
  let sectionNames = map (fromJust . reverseLink . head) sections
  mapMaybe
    (\(name :: Text.Text, sect) -> do
        reverseDeps <- firstNaturalNumber sect
        pure (sectionPackageName name sect, reverseDeps) :: Maybe (Text.Text, Natural))
    $ zip sectionNames sections


  where
    reverseLink = \case
      Tag.TagOpen "a" attrs -> mapFind attrReverseLink attrs
      _ -> Nothing

    attrReverseLink = \case
      ("href", lnk) -> if
          | "packdeps.haskellers.com/reverse/" `Text.isInfixOf` lnk -> Just lnk
          | otherwise -> Nothing
      _ -> Nothing

    sectionPackageName :: Text -> [Tag] -> Text
    sectionPackageName sectionName = \case
      (_: Tag.TagText name : _) -> name
      (_: el : _) -> sectionName
      xs -> sectionName


    firstNaturalNumber :: [Tag] -> Maybe Natural
    firstNaturalNumber =
      mapFind (\case
        Tag.TagText t -> parseNat t
        _ -> Nothing)

    mapFind :: (a -> Maybe b) -> [a] -> Maybe b
    mapFind f xs = fromJust . f <$> List.find (isJust . f) xs
