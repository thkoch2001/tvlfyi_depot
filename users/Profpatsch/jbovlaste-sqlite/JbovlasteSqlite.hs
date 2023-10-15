{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Conduit ((.|))
import Conduit qualified as Cond
import Control.Category qualified as Cat
import Control.Foldl qualified as Fold
import Data.ByteString.Internal qualified as Bytes
import Data.Error.Tree
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import Database.SQLite.Simple qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite.Simple.QQ qualified as Sqlite
import FieldParser qualified as Field
import Label
import Parse
import PossehlAnalyticsPrelude
import Text.XML (def)
import Text.XML qualified as Xml
import Prelude hiding (init, maybe)

main :: IO ()
main = do
  f <- file
  f.documentRoot
    & filterDown
    & toTree
    & prettyErrorTree
    & Text.putStrLn

test :: IO ()
test = do
  withEnv $ \env -> do
    migrate env
    f <- file
    parseJbovlasteXml f
      & \case
        Left errs -> Text.putStrLn $ prettyErrorTree errs
        Right valsi -> insertValsi env valsi

filterDown :: Xml.Element -> Xml.Element
filterDown el =
  el
    & filterElementsRec noUsers
    & downTo (T2 (label @"maxdepth" 5) (label @"maxlistitems" 30))

data Valsi = Valsi
  { word :: Text,
    definition :: Text,
    definitionId :: Natural,
    typ :: Text,
    selmaho :: Maybe Text,
    notes :: Maybe Text,
    glosswords :: [T2 "word" Text "sense" (Maybe Text)],
    keywords :: [T3 "word" Text "place" Natural "sense" (Maybe Text)]
  }
  deriving stock (Show)

insertValsi :: Env -> [Valsi] -> IO ()
insertValsi env vs = do
  Sqlite.withTransaction env.envData $
    do
      valsiIds <-
        Cond.yieldMany vs
          .| Cond.mapMC
            ( \v ->
                Sqlite.queryNamed
                  @(Sqlite.Only Int64)
                  env.envData
                  [Sqlite.sql|
                     INSERT INTO valsi
                       (word , definition , type , selmaho , notes )
                       VALUES
                       (:word, :definition, :type, :selmaho, :notes)
                       RETURNING (id)
                   |]
                  [ ":word" Sqlite.:= v.word,
                    ":definition" Sqlite.:= v.definition,
                    ":type" Sqlite.:= v.typ,
                    ":selmaho" Sqlite.:= v.selmaho,
                    ":notes" Sqlite.:= v.notes
                  ]
                  >>= \case
                    [one] -> pure one
                    _ -> error "more or less than one result"
            )
          .| Cond.sinkList
          & Cond.runConduit
      for_ (zip valsiIds vs) $ \(Sqlite.Only vId, v) -> do
        for_ v.glosswords $ \g -> do
          Sqlite.executeNamed
            env.envData
            [Sqlite.sql|
                      INSERT INTO glosswords
                        (valsi_id , word , sense )
                        VALUES
                        (:valsi_id, :word, :sense)
                    |]
            [ ":valsi_id" Sqlite.:= vId,
              ":word" Sqlite.:= g.word,
              ":sense" Sqlite.:= g.sense
            ]
      for_ (zip valsiIds vs) $ \(Sqlite.Only vId, v) -> do
        for_ v.keywords $ \g -> do
          Sqlite.executeNamed
            env.envData
            [Sqlite.sql|
                      INSERT INTO keywords
                        (valsi_id , word , place , sense )
                        VALUES
                        (:valsi_id, :word, :place, :sense)
                    |]
            [ ":valsi_id" Sqlite.:= vId,
              ":word" Sqlite.:= g.word,
              ":place" Sqlite.:= (g.place & fromIntegral @Natural @Int),
              ":sense" Sqlite.:= g.sense
            ]

migrate :: (HasField "envData" p Sqlite.Connection) => p -> IO ()
migrate env = do
  let x q = Sqlite.execute env.envData q ()
  x
    [Sqlite.sql|
      CREATE TABLE IF NOT EXISTS valsi (
        id integer PRIMARY KEY,
        word text NOT NULL,
        definition text NOT NULL,
        type text NOT NULL,
        selmaho text NULL,
        notes text NULL
      )
     |]
  x
    [Sqlite.sql|
      CREATE TABLE IF NOT EXISTS glosswords (
        id integer PRIMARY KEY,
        valsi_id integer NOT NULL,
        word text NOT NULL,
        sense text NULL,
        FOREIGN KEY(valsi_id) REFERENCES valsi(id)
      )
    |]
  x
    [Sqlite.sql|
      CREATE TABLE IF NOT EXISTS keywords (
        id integer PRIMARY KEY,
        valsi_id integer NOT NULL,
        word text NOT NULL,
        place integer NOT NULL,
        sense text NULL,
        FOREIGN KEY(valsi_id) REFERENCES valsi(id)
      )
    |]

data Env = Env
  { envData :: Sqlite.Connection
  }

withEnv :: (Env -> IO a) -> IO a
withEnv inner = do
  withSqlite "./jbovlaste.sqlite" $ \envData -> inner Env {..}

withSqlite :: String -> (Sqlite.Connection -> IO a) -> IO a
withSqlite fileName inner = Sqlite.withConnection fileName $ \conn -> do
  -- Sqlite.setTrace conn (Just (\msg -> Text.hPutStrLn IO.stderr [fmt|{fileName}: {msg}|]))
  Sqlite.execute conn [Sqlite.sql|PRAGMA foreign_keys = ON|] ()
  inner conn

parseJbovlasteXml :: (HasField "documentRoot" r Xml.Element) => r -> Either ErrorTree [Valsi]
parseJbovlasteXml xml =
  xml.documentRoot
    & runParse
      "cannot parse jbovlaste.xml"
      parser
  where
    parser =
      (element "dictionary" <&> (.elementNodes) <&> mapMaybe nodeElementMay)
        >>> ( find
                ( element "direction"
                    >>> do
                      (attribute "from" >>> exactly showToText "lojban")
                      *> (attribute "to" >>> exactly showToText "English")
                      *> Cat.id
                )
                <&> (\x -> x.elementNodes <&> nodeElementMay)
            )
        >>> (multiple (maybe valsi) <&> catMaybes)
    valsi =
      element "valsi"
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

          word <- attribute "word"
          typ <- attribute "type"
          selmaho <- optionalSubElementContent "selmaho"
          definition <- subElementContent "definition"
          definitionId <- subElementContent "definitionid" >>> fieldParser Field.decimalNatural
          notes <- optionalSubElementContent "notes"
          glosswords <-
            (subNodes >>> findAll (element "glossword"))
              >>> ( multiple $ do
                      word' <- label @"word" <$> (attribute "word")
                      sense <- label @"sense" <$> (attributeMay "sense")
                      pure $ T2 word' sense
                  )
          keywords <-
            (subNodes >>> findAll (element "keyword"))
              >>> ( multiple $ do
                      word' <- label @"word" <$> (attribute "word")
                      place <- label @"place" <$> (attribute "place" >>> fieldParser Field.decimalNatural)
                      sense <- label @"sense" <$> (attributeMay "sense")
                      pure $ T3 word' place sense
                  )

          pure $ Valsi {..}

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

instance
  ( Sqlite.FromField t1,
    Sqlite.FromField t2,
    Sqlite.FromField t3
  ) =>
  Sqlite.FromRow (T3 l1 t1 l2 t2 l3 t3)
  where
  fromRow = do
    T3
      <$> (label @l1 <$> Sqlite.field)
      <*> (label @l2 <$> Sqlite.field)
      <*> (label @l3 <$> Sqlite.field)

foldRows ::
  forall row params b.
  (Sqlite.FromRow row, Sqlite.ToRow params) =>
  Sqlite.Connection ->
  Sqlite.Query ->
  params ->
  Fold.Fold row b ->
  IO b
foldRows conn qry params = Fold.purely f
  where
    f :: forall x. (x -> row -> x) -> x -> (x -> b) -> IO b
    f acc init extract = do
      x <- Sqlite.fold conn qry params init (\a r -> pure $ acc a r)
      pure $ extract x
