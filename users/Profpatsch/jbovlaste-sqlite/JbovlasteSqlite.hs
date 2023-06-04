{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Category qualified
import Control.Category qualified as Cat
import Control.Selective (Selective)
import Data.ByteString.Internal qualified as Bytes
import Data.Error.Tree
import Data.Functor.Compose
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Semigroup (Sum (..))
import Data.Semigroup.Traversable
import Data.Semigroupoid qualified as Semigroupoid
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Label
import PossehlAnalyticsPrelude
import Text.XML (def)
import Text.XML qualified as Xml
import Prelude hiding (maybe)
import Prelude qualified

main :: IO ()
main = do
  f <- file
  f.documentRoot
    & filterDown
    & toTree
    & prettyErrorTree
    & Text.putStrLn

filterDown :: Xml.Element -> Xml.Element
filterDown el =
  el
    & filterElementsRec noUsers
    & downTo (T2 (label @"maxdepth" 5) (label @"maxlistitems" 30))

test :: IO ()
test = do
  f <- file
  f.documentRoot
    & runParse
      "uh oh"
      ( ( element "dictionary" <&> (.elementNodes) <&> mapMaybe nodeElement <&> nonEmpty
        )
          >>> ( maybe $
                  ( first'
                      ( element "direction"
                          >>> ( ( do
                                    (attribute "from" >>> exactly showToText "lojban")
                                    *> (attribute "to" >>> exactly showToText "English")
                                    *> Cat.id
                                )
                              )
                      )
                      >>> dimap
                        (\x -> x.elementNodes <&> nodeElement)
                        (Map.fromListWith (<>) . catMaybes)
                        ( multiple
                            (\idx _ -> [fmt|{idx}|])
                            ( maybe $
                                element "valsi" >>> do
                                  (,)
                                    <$> attribute "type"
                                    <*> pure (Sum 1)
                            )
                        )
                  )
              )
      )
    & \case
      Left errs -> Text.putStrLn $ prettyErrorTree errs
      Right el -> do
        el
          & \case
            Nothing -> pure ()
            Just a ->
              a & print

-- <&> filterDown
-- <&> toTree
-- & nonEmpty
-- & \case
--   Nothing -> pure ()
--   Just tree ->
--     tree
--       & nestedMultiError "yay"
--       & prettyErrorTree
--       & Text.putStrLn

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
  case el.elementNodes & filter (not . isEmptyContent) & nonEmpty of
    Nothing -> singleError (newError (prettyXmlElement el))
    Just (n :| []) | not $ isElementNode n -> singleError $ errorContext (prettyXmlElement el) (nodeErrorNoElement n)
    Just nodes -> nestedMultiError (newError (prettyXmlElement el)) (nodes <&> node)
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

prettyXmlName :: Xml.Name -> Text
prettyXmlName n = [fmt|{n.namePrefix & fromMaybe ""}{n.nameLocalName}|]

prettyXmlElement :: Xml.Element -> Text
prettyXmlElement el =
  if not $ null el.elementAttributes
    then [fmt|<{prettyXmlName el.elementName}: {attrs el.elementAttributes}>|]
    else [fmt|<{prettyXmlName el.elementName}>|]
  where
    attrs :: Map Xml.Name Text -> Text
    attrs a = a & Map.toList <&> (\(k, v) -> [fmt|{prettyXmlName k}={v}|]) & Text.intercalate ", " & \s -> [fmt|({s})|]

nodeElement :: Xml.Node -> Maybe Xml.Element
nodeElement = \case
  Xml.NodeElement el -> Just el
  _ -> Nothing

newtype Context = Context [Text]
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

newtype Parse from to = Parse ((Context, from) -> Validation (NonEmpty ErrorTree) (Context, to))
  deriving
    (Functor, Applicative, Selective)
    via ( Compose
            ( Compose
                ((->) (Context, from))
                (Validation (NonEmpty ErrorTree))
            )
            ((,) Context)
        )

instance Semigroupoid Parse where
  o p2 p1 = Parse $ \from -> case runParse' p1 from of
    Failure err -> Failure err
    Success to1 -> runParse' p2 to1

instance Category Parse where
  (.) = Semigroupoid.o
  id = Parse $ \t -> Success t

instance Profunctor Parse where
  lmap f (Parse p) = Parse $ lmap (second f) p
  rmap = (<$>)

runParse :: Error -> Parse from to -> from -> Either ErrorTree to
runParse errMsg parser t =
  (Context ["$"], t)
    & runParse' parser
    <&> snd
    & first (nestedMultiError errMsg)
    & validationToEither

runParse' :: Parse from to -> (Context, from) -> Validation (NonEmpty ErrorTree) (Context, to)
runParse' (Parse f) from = f from

showContext :: Context -> Text
showContext (Context context) = context & List.reverse & Text.intercalate "."

addContext :: Text -> Context -> Context
addContext = coerce . (:)

element :: Text -> Parse Xml.Element Xml.Element
element name = Parse $ \(ctx, el) ->
  if el.elementName.nameLocalName == name
    then Success (ctx & addContext (prettyXmlName el.elementName), el)
    else Failure $ singleton [fmt|Expected element named <{name}> but got {el & prettyXmlElement} at {showContext ctx}|]

attribute :: Text -> Parse Xml.Element Text
attribute name = Parse $ \(ctx, el) ->
  case el.elementAttributes & Map.mapKeys (.nameLocalName) & Map.lookup name of
    Just a -> Success (ctx & addContext [fmt|{{attr:{name}}}|], a)
    Nothing -> Failure $ singleton [fmt|Attribute "{name}" missing at {showContext ctx}|]

-- | 'oneOf' but only one value possible
exactly :: Eq from => (from -> Text) -> from -> Parse from from
exactly errDisplay from = Parse $ \(ctx, from') ->
  if from == from'
    then Success (ctx, from')
    else Failure $ singleton [fmt|Field has to be exactly {errDisplay from}, was: {errDisplay from'} at {showContext ctx}|]

multiple :: (Natural -> a1 -> Error) -> Parse a1 a2 -> Parse [a1] [a2]
multiple errorFn inner = dimap nonEmpty (Prelude.maybe [] toList) (maybe $ multipleNE errorFn inner)

multipleNE :: (Natural -> from -> Error) -> Parse from to -> Parse (NonEmpty from) (NonEmpty to)
multipleNE errorFn inner = Parse $ \(ctx, from) ->
  from
    & zipIndex
    & traverse (\(idx, f) -> runParse' inner (ctx, f) & first (singleton . nestedMultiError (errorFn idx f)))
    -- we assume that, since the same parser is used everywhere, the context will be the same as well (TODO: correct?)
    & second (\((ctx', y) :| ys) -> (ctx', y :| (snd <$> ys)))

maybe :: Parse from to -> Parse (Maybe from) (Maybe to)
maybe inner = Parse $ \(ctx, m) -> case m of
  Nothing -> Success (ctx, Nothing)
  Just a -> runParse' inner (ctx, a) & second (fmap Just)

first' :: Parse from to -> Parse (NonEmpty from) to
first' inner = Parse $ \(ctx, xs) ->
  xs
    <&> (\x -> runParse' inner (ctx, x))
    & traverse1
      ( \case
          Success a -> Left a
          Failure e -> Right e
      )
    & \case
      Left a -> Success a
      Right errs ->
        errs
          & zipIndex
          <&> (\(idx, errs') -> nestedMultiError [fmt|{idx}|] errs')
          & nestedMultiError [fmt|None of these sub-parsers succeeded|]
          & singleton
          & Failure

zipNonEmpty :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zipNonEmpty (x :| xs) (y :| ys) = (x, y) :| zip xs ys

zipIndex :: NonEmpty b -> NonEmpty (Natural, b)
zipIndex = zipNonEmpty (1 :| [2 :: Natural ..])
