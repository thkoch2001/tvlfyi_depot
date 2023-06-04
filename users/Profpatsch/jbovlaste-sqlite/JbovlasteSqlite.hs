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
import Data.Monoid (First (..))
import Data.Semigroup.Traversable
import Data.Semigroupoid qualified as Semigroupoid
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import FieldParser (FieldParser)
import FieldParser qualified as Field
import Label
import PossehlAnalyticsPrelude
import Pretty
import Text.XML (def)
import Text.XML qualified as Xml
import Validation (partitionValidations)
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

data Valsi = Valsi
  { definition :: Text,
    definitionId :: Natural,
    typ :: Text,
    selmaho :: Maybe Text,
    notes :: Maybe Text,
    glosswords :: [T2 "word" Text "sense" (Maybe Text)],
    keywords :: [T3 "word" Text "place" Natural "sense" (Maybe Text)]
  }
  deriving stock (Show)

test :: IO ()
test = do
  f <- file
  f.documentRoot
    & runParse
      "uh oh"
      ( ( element "dictionary" <&> (.elementNodes) <&> mapMaybe nodeElementMay
        )
          >>> ( ( find
                    ( element "direction"
                        >>> ( ( do
                                  (attribute "from" >>> exactly showToText "lojban")
                                  *> (attribute "to" >>> exactly showToText "English")
                                  *> Cat.id
                              )
                            )
                    )
                    >>> dimap
                      (\x -> x.elementNodes <&> nodeElementMay)
                      (catMaybes)
                      ( multiple
                          (\idx _ -> [fmt|{idx}|])
                          ( maybe $
                              (element "valsi")
                                >>> do
                                  let subNodes =
                                        ( Cat.id
                                            <&> (.elementNodes)
                                            <&> mapMaybe nodeElementMay
                                        )

                                  let subElementContent elName =
                                        subNodes
                                          >>> ( (find (element elName))
                                                  <&> (.elementNodes)
                                              )
                                          >>> exactlyOne
                                          >>> content
                                  let optionalSubElementContent elName =
                                        subNodes
                                          >>> ((findAll (element elName) >>> zeroOrOne))
                                          >>> (maybe (lmap (.elementNodes) exactlyOne >>> content))

                                  typ <- attribute "type"
                                  selmaho <- optionalSubElementContent "selmaho"
                                  definition <- subElementContent "definition"
                                  definitionId <- subElementContent "definitionid" >>> fieldParser Field.decimalNatural
                                  notes <- optionalSubElementContent "notes"
                                  glosswords <-
                                    (subNodes >>> findAll (element "glossword"))
                                      >>> ( multiple (\idx _ -> [fmt|{idx}|]) $ do
                                              word <- label @"word" <$> (attribute "word")
                                              sense <- label @"sense" <$> (attributeMay "sense")
                                              pure $ T2 word sense
                                          )
                                  keywords <-
                                    (subNodes >>> findAll (element "keyword"))
                                      >>> ( multiple (\idx _ -> [fmt|{idx}|]) $ do
                                              word <- label @"word" <$> (attribute "word")
                                              place <- label @"place" <$> (attribute "place" >>> fieldParser Field.decimalNatural)
                                              sense <- label @"sense" <$> (attributeMay "sense")
                                              pure $ T3 word place sense
                                          )

                                  pure $ Valsi {..}
                          )
                      )
                )
              )
      )
    & \case
      Left errs -> Text.putStrLn $ prettyErrorTree errs
      Right el -> do
        el & traverse_ printPretty

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

nodeElementMay :: Xml.Node -> Maybe Xml.Element
nodeElementMay = \case
  Xml.NodeElement el -> Just el
  _ -> Nothing

newtype Context = Context (Maybe [Text])
  deriving stock (Show)
  deriving (Semigroup, Monoid) via (First [Text])

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
  (Context (Just ["$"]), t)
    & runParse' parser
    <&> snd
    & first (nestedMultiError errMsg)
    & validationToEither

runParse' :: Parse from to -> (Context, from) -> Validation (NonEmpty ErrorTree) (Context, to)
runParse' (Parse f) from = f from

showContext :: Context -> Text
showContext (Context context) = context & fromMaybe [] & List.reverse & Text.intercalate "."

addContext :: Text -> Context -> Context
addContext x (Context mxs) = Context (Just $ x : (mxs & fromMaybe []))

element :: Text -> Parse Xml.Element Xml.Element
element name = Parse $ \(ctx, el) ->
  if el.elementName.nameLocalName == name
    then Success (ctx & addContext (prettyXmlName el.elementName), el)
    else Failure $ singleton [fmt|Expected element named <{name}> but got {el & prettyXmlElement} at {showContext ctx}|]

content :: Parse Xml.Node Text
content = Parse $ \(ctx, node) -> case node of
  Xml.NodeContent t -> Success (ctx, t)
  -- TODO: give an example of the node content?
  n -> Failure $ singleton [fmt|Expected a content node, but got a {n & nodeType} node, at {showContext ctx}|]
    where
      nodeType = \case
        Xml.NodeContent _ -> "content" :: Text
        Xml.NodeComment _ -> "comment"
        Xml.NodeInstruction _ -> "instruction"
        Xml.NodeElement _ -> "element"

attribute :: Text -> Parse Xml.Element Text
attribute name = Parse $ \(ctx, el) ->
  case el.elementAttributes & Map.mapKeys (.nameLocalName) & Map.lookup name of
    Just a -> Success (ctx & addContext [fmt|{{attr:{name}}}|], a)
    Nothing -> Failure $ singleton [fmt|Attribute "{name}" missing at {showContext ctx}|]

attributeMay :: Text -> Parse Xml.Element (Maybe Text)
attributeMay name = Parse $ \(ctx, el) ->
  case el.elementAttributes & Map.mapKeys (.nameLocalName) & Map.lookup name of
    Just a -> Success (ctx & addContext [fmt|{{attr:{name}}}|], Just a)
    Nothing -> Success (ctx, Nothing)

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

exactlyOne :: Parse [from] from
exactlyOne = Parse $ \(ctx, xs) -> case xs of
  [] -> Failure $ singleton [fmt|Expected exactly 1 element, but got 0, at {ctx & showContext}|]
  [one] -> Success (ctx, one)
  _more -> Failure $ singleton [fmt|Expected exactly 1 element, but got 2, at {ctx & showContext}|]

zeroOrOne :: Parse [from] (Maybe from)
zeroOrOne = Parse $ \(ctx, xs) -> case xs of
  [] -> Success (ctx, Nothing)
  [one] -> Success (ctx, Just one)
  _more -> Failure $ singleton [fmt|Expected exactly 1 element, but got 2, at {ctx & showContext}|]

find :: Parse from to -> Parse [from] to
find inner = Parse $ \(ctx, xs) -> case xs of
  [] -> failure [fmt|Wanted to get the first sub-parser that succeeds, but there were no elements in the list, at {ctx & showContext}|]
  (y : ys) -> runParse' (findNE' inner) (ctx, y :| ys)

findNE' :: Parse from to -> Parse (NonEmpty from) to
findNE' inner = Parse $ \(ctx, xs) ->
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

findAll :: Parse from to -> Parse [from] [to]
findAll inner = Parse $ \(ctx, xs) ->
  xs
    <&> (\x -> runParse' inner (ctx, x))
    & partitionValidations
    & \case
      (_miss, []) ->
        -- in this case we just arbitrarily forward the original context …
        Success (ctx, [])
      (_miss, (hitCtx, hit) : hits) -> Success (hitCtx, hit : (hits <&> snd))

fieldParser :: FieldParser from to -> Parse from to
fieldParser fp = Parse $ \(ctx, from) -> case Field.runFieldParser fp from of
  Right a -> Success (ctx, a)
  Left err -> Failure $ singleton (singleError err)

zipNonEmpty :: NonEmpty a -> NonEmpty b -> NonEmpty (a, b)
zipNonEmpty (x :| xs) (y :| ys) = (x, y) :| zip xs ys

zipIndex :: NonEmpty b -> NonEmpty (Natural, b)
zipIndex = zipNonEmpty (1 :| [2 :: Natural ..])
