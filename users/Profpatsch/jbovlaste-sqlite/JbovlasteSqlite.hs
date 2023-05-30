{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Internal qualified as Bytes
import Data.Error.Tree
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Label
import PossehlAnalyticsPrelude
import Text.XML (def)
import Text.XML qualified as Xml

main :: IO ()
main = do
  f <- file
  f.documentRoot
    & filterElementsRec noUsers
    & downTo (T2 (label @"maxdepth" 5) (label @"maxlistitems" 20))
    & toTree
    & prettyErrorTree
    & Text.putStrLn

file :: IO Xml.Document
file = Xml.readFile def "./jbovlaste-en.xml"

-- | Filter XML elements recursively based on the given predicate
filterElementsRec :: (Xml.Element -> Bool) -> Xml.Element -> Xml.Element
filterElementsRec f el =
  el
    { Xml.elementNodes =
        mapMaybe
          ( \case
              Xml.NodeElement el' ->
                if f el'
                  then Just $ Xml.NodeElement $ filterElementsRec f el'
                  else Nothing
              other -> Just other
          )
          el.elementNodes
    }

-- | no <user> allowed
noUsers :: Xml.Element -> Bool
noUsers el = el.elementName.nameLocalName /= "user"

downTo :: (T2 "maxdepth" Int "maxlistitems" Int) -> Xml.Element -> Xml.Element
downTo n el =
  if n.maxdepth > 0
    then
      el
        { Xml.elementNodes =
            ( do
                let eleven = take (n.maxlistitems + 1) $ map down el.elementNodes
                if List.length eleven == (n.maxlistitems + 1)
                  then eleven <> [Xml.NodeComment "snip!"]
                  else eleven
            )
        }
    else el {Xml.elementNodes = [Xml.NodeComment "snip!"]}
  where
    down =
      \case
        Xml.NodeElement el' ->
          Xml.NodeElement $
            downTo
              ( T2
                  (label @"maxdepth" $ n.maxdepth - 1)
                  (label @"maxlistitems" n.maxlistitems)
              )
              el'
        more -> more

toTree :: Xml.Element -> ErrorTree
toTree el = do
  let outer =
        if not $ null el.elementAttributes
          then [fmt|<{name el.elementName}: {attrs el.elementAttributes}>|]
          else [fmt|<{name el.elementName}>|]

  case el.elementNodes & filter (not . isEmptyContent) & nonEmpty of
    Nothing -> singleError (newError outer)
    Just (n :| []) | not $ isElementNode n -> singleError $ errorContext outer (nodeErrorNoElement n)
    Just nodes -> nestedMultiError (newError outer) (nodes <&> node)
  where
    isEmptyContent = \case
      Xml.NodeContent c -> c & Text.all Bytes.isSpaceChar8
      _ -> False
    isElementNode = \case
      Xml.NodeElement _ -> True
      _ -> False

    node :: Xml.Node -> ErrorTree
    node = \case
      Xml.NodeElement el' -> toTree el'
      other -> singleError $ nodeErrorNoElement other

    nodeErrorNoElement :: Xml.Node -> Error
    nodeErrorNoElement = \case
      Xml.NodeInstruction i -> [fmt|Instruction: {i & show}|]
      Xml.NodeContent c -> [fmt|"{c & Text.replace "\"" "\\\""}"|]
      Xml.NodeComment c -> [fmt|<!-- {c} -->|]
      Xml.NodeElement _ -> error "NodeElement not allowed here"

    name :: Xml.Name -> Text
    name n = [fmt|{n.namePrefix & fromMaybe ""}{n.nameLocalName}|]
    attrs :: Map Xml.Name Text -> Text
    attrs a = a & Map.toList <&> (\(k, v) -> [fmt|{name k}={v}|]) & Text.intercalate ", " & \s -> [fmt|({s})|]
