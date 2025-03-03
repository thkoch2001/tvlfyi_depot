{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Netencode where

import Control.Applicative (many)
import Data.Attoparsec.ByteString qualified as Atto
import Data.Attoparsec.ByteString.Char8 qualified as Atto.Char
import Data.ByteString qualified as ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as ByteString.Lazy
import Data.Fix (Fix (Fix))
import Data.Fix qualified as Fix
import Data.Functor.Classes (Eq1 (liftEq))
import Data.Int (Int16, Int32, Int64, Int8)
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEMap
import Data.Semigroup qualified as Semi
import Data.String (IsString)
import Data.Word (Word16, Word32, Word64)
import GHC.Exts (fromString)
import Hedgehog qualified as Hedge
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import PossehlAnalyticsPrelude
import Text.Show.Deriving
import Prelude hiding (sum)

-- | Netencode type base functor.
--
-- Recursive elements have a @rec@.
data TF rec
  = -- | Unit value
    Unit
  | -- | Boolean (2^1)
    N1 Bool
  | -- | Byte (2^3)
    N3 Word8
  | -- | 64-bit Natural (2^6)
    N6 Word64
  | -- | 64-bit Integer (2^6)
    I6 Int64
  | -- | Unicode Text
    Text Text
  | -- | Arbitrary Bytestring
    Bytes ByteString
  | -- | A constructor of a(n open) Sum
    Sum (Tag Text rec)
  | -- | Record
    Record (NEMap Text rec)
  | -- | List
    List [rec]
  deriving stock (Show, Eq, Functor)

instance Eq1 TF where
  liftEq _ Unit Unit = True
  liftEq _ (N1 b) (N1 b') = b == b'
  liftEq _ (N3 w8) (N3 w8') = w8 == w8'
  liftEq _ (N6 w64) (N6 w64') = w64 == w64'
  liftEq _ (I6 i64) (I6 i64') = i64 == i64'
  liftEq _ (Text t) (Text t') = t == t'
  liftEq _ (Bytes b) (Bytes b') = b == b'
  liftEq eq (Sum t) (Sum t') = eq (t.tagVal) (t'.tagVal)
  liftEq eq (Record m) (Record m') = liftEq eq m m'
  liftEq eq (List xs) (List xs') = liftEq eq xs xs'
  liftEq _ _ _ = False

-- | A tagged value
data Tag tag val = Tag
  { tagTag :: tag,
    tagVal :: val
  }
  deriving stock (Show, Eq, Functor)

$(Text.Show.Deriving.deriveShow1 ''Tag)
$(Text.Show.Deriving.deriveShow1 ''TF)

-- | The Netencode type
newtype T = T {unT :: Fix TF}
  deriving stock (Eq, Show)

-- | Create a unit
unit :: T
unit = T $ Fix Unit

-- | Create a boolean
n1 :: Bool -> T
n1 = T . Fix . N1

-- | Create a byte
n3 :: Word8 -> T
n3 = T . Fix . N3

-- | Create a 64-bit natural
n6 :: Word64 -> T
n6 = T . Fix . N6

-- | Create a 64-bit integer
i6 :: Int64 -> T
i6 = T . Fix . I6

-- | Create a UTF-8 unicode text
text :: Text -> T
text = T . Fix . Text

-- | Create an arbitrary bytestring
bytes :: ByteString -> T
bytes = T . Fix . Bytes

-- | Create a tagged value from a tag name and a value
tag :: Text -> T -> T
tag key val = T $ Fix $ Sum $ coerce @(Tag Text T) @(Tag Text (Fix TF)) $ Tag key val

-- | Create a record from a non-empty map
record :: NEMap Text T -> T
record = T . Fix . Record . coerce @(NEMap Text T) @(NEMap Text (Fix TF))

-- | Create a list
list :: [T] -> T
list = T . Fix . List . coerce @[T] @([Fix TF])

-- | Stable encoding of a netencode value. Record keys will be sorted lexicographically ascending.
netencodeEncodeStable :: T -> Builder
netencodeEncodeStable (T fix) = Fix.foldFix (netencodeEncodeStableF id) fix

-- | Stable encoding of a netencode functor value. Record keys will be sorted lexicographically ascending.
--
-- The given function is used for encoding the recursive values.
netencodeEncodeStableF :: (rec -> Builder) -> TF rec -> Builder
netencodeEncodeStableF inner tf = builder go
  where
    -- TODO: directly pass in BL?
    innerBL = fromBuilder . inner
    go = case tf of
      Unit -> "u,"
      N1 False -> "n1:0,"
      N1 True -> "n1:1,"
      N3 w8 -> "n3:" <> fromBuilder (Builder.word8Dec w8) <> ","
      N6 w64 -> "n6:" <> fromBuilder (Builder.word64Dec w64) <> ","
      I6 i64 -> "i6:" <> fromBuilder (Builder.int64Dec i64) <> ","
      Text t ->
        let b = fromText t
         in "t" <> builderLenDec b <> ":" <> b <> ","
      Bytes b -> "b" <> builderLenDec (fromByteString b) <> ":" <> fromByteString b <> ","
      Sum (Tag key val) -> encTag key val
      Record m ->
        -- NEMap uses Map internally, and that folds in lexicographic ascending order over the key.
        -- Since these are `Text` in our case, this is stable.
        let mBuilder = m & NEMap.foldMapWithKey encTag
         in "{" <> builderLenDec mBuilder <> ":" <> mBuilder <> "}"
      List xs ->
        let xsBuilder = xs <&> innerBL & mconcat
         in "[" <> builderLenDec xsBuilder <> ":" <> xsBuilder <> "]"
      where
        encTag key val =
          let bKey = fromText key
           in "<" <> builderLenDec bKey <> ":" <> bKey <> "|" <> innerBL val

-- | A builder that knows its own size in bytes
newtype BL = BL (Builder, Semi.Sum Natural)
  deriving newtype (Monoid, Semigroup)

instance IsString BL where
  fromString s =
    BL
      ( fromString @Builder s,
        fromString @ByteString s
          & ByteString.length
          & intToNatural
          & fromMaybe 0
          & Semi.Sum
      )

-- | Retrieve the builder
builder :: BL -> Builder
builder (BL (b, _)) = b

-- | Retrieve the bytestring length
builderLen :: BL -> Natural
builderLen (BL (_, len)) = Semi.getSum $ len

-- | Take a 'BL' and create a new 'BL' that represents the length as a decimal integer
builderLenDec :: BL -> BL
builderLenDec (BL (_, len)) =
  let b = Builder.intDec $ (len & Semi.getSum & fromIntegral @Natural @Int)
   in b & fromBuilder

-- | Create a 'BL' from a 'Builder'.
--
-- Not efficient, goes back to a lazy bytestring to get the length
fromBuilder :: Builder -> BL
fromBuilder b =
  BL
    ( b,
      b
        & Builder.toLazyByteString
        & ByteString.Lazy.length
        & fromIntegral @Int64 @Natural
        & Semi.Sum
    )

-- | Create a 'BL' from a 'ByteString'.
fromByteString :: ByteString -> BL
fromByteString b =
  BL
    ( Builder.byteString b,
      b
        & ByteString.length
        & fromIntegral @Int @Natural
        & Semi.Sum
    )

-- | Create a 'BL' from a 'Text'.
fromText :: Text -> BL
fromText t = t & textToBytesUtf8 & fromByteString

-- | Parser for a netencode value.
netencodeParser :: Atto.Parser T
netencodeParser = T <$> go
  where
    go = Fix <$> netencodeParserF go

-- | Parser for one level of a netencode value. Requires a parser for the recursion.
netencodeParserF :: Atto.Parser rec -> Atto.Parser (TF rec)
netencodeParserF inner = do
  typeTag <- Atto.Char.anyChar
  case typeTag of
    't' -> Text <$> textParser
    'b' -> Bytes <$> bytesParser
    'u' -> unitParser
    '<' -> Sum <$> tagParser
    '{' -> Record <$> recordParser
    '[' -> List <$> listParser
    'n' -> naturalParser
    'i' -> I6 <$> intParser
    c -> fail ([c] <> " is not a valid netencode tag")
  where
    bytesParser = do
      len <- boundedDecimalFail Atto.<?> "bytes is missing a digit specifying the length"
      _ <- Atto.Char.char ':' Atto.<?> "bytes did not have : after length"
      bytes' <- Atto.take len
      _ <- Atto.Char.char ',' Atto.<?> "bytes did not end with ,"
      pure bytes'

    textParser = do
      len <- boundedDecimalFail Atto.<?> "text is missing a digit specifying the length"
      _ <- Atto.Char.char ':' Atto.<?> "text did not have : after length"
      text' <-
        Atto.take len <&> bytesToTextUtf8 >>= \case
          Left err -> fail [fmt|cannot decode text as utf8: {err & prettyError}|]
          Right t -> pure t
      _ <- Atto.Char.char ',' Atto.<?> "text did not end with ,"
      pure text'

    unitParser = do
      _ <- Atto.Char.char ',' Atto.<?> "unit did not end with ,"
      pure $ Unit

    tagParser = do
      len <- boundedDecimalFail Atto.<?> "tag is missing a digit specifying the length"
      _ <- Atto.Char.char ':' Atto.<?> "tag did not have : after length"
      tagTag <-
        Atto.take len <&> bytesToTextUtf8 >>= \case
          Left err -> fail [fmt|cannot decode tag key as utf8: {err & prettyError}|]
          Right t -> pure t
      _ <- Atto.Char.char '|' Atto.<?> "tag was missing the key/value separator (|)"
      tagVal <- inner
      pure $ Tag {..}

    recordParser = do
      -- TODO: the record does not use its inner length because we are descending into the inner parsers.
      -- This is a smell! In theory it can be used to skip parsing the whole inner keys.
      _len <- boundedDecimalFail Atto.<?> "record is missing a digit specifying the length"
      _ <- Atto.Char.char ':' Atto.<?> "record did not have : after length"
      record' <-
        many (Atto.Char.char '<' >> tagParser) <&> nonEmpty >>= \case
          Nothing -> fail "record is not allowed to have 0 elements"
          Just tags ->
            pure $
              tags
                <&> (\t -> (t.tagTag, t.tagVal))
                -- later keys are preferred if they are duplicates, according to the standard
                & NEMap.fromList
      _ <- Atto.Char.char '}' Atto.<?> "record did not end with }"
      pure record'

    listParser = do
      -- TODO: the list does not use its inner length because we are descending into the inner parsers.
      -- This is a smell! In theory it can be used to skip parsing the whole inner keys.
      _len <- boundedDecimalFail Atto.<?> "list is missing a digit specifying the length"
      _ <- Atto.Char.char ':' Atto.<?> "list did not have : after length"
      -- TODO: allow empty lists?
      list' <- many inner
      _ <- Atto.Char.char ']' Atto.<?> "list did not end with ]"
      pure list'

    intParser = do
      let p :: forall parseSize. (Bounded parseSize, Integral parseSize) => (Integer -> Atto.Parser Int64)
          p n = do
            _ <- Atto.Char.char ':' Atto.<?> [fmt|i{n & show} did not have : after length|]
            isNegative <- Atto.option False (Atto.Char.char '-' <&> \_c -> True)
            int <-
              boundedDecimal @parseSize >>= \case
                Nothing -> fail [fmt|cannot parse into i{n & show}, the number is too big (would overflow)|]
                Just i ->
                  pure $
                    if isNegative
                      then -- TODO: this should alread be done in the decimal parser, @minBound@ cannot be parsed cause it’s one more than @(-maxBound)@!
                        (-i)
                      else i
            _ <- Atto.Char.char ',' Atto.<?> [fmt|i{n & show} did not end with ,|]
            pure $ fromIntegral @parseSize @Int64 int
      digit <- Atto.Char.digit
      case digit of
        -- TODO: separate parser for i1 and i2 that makes sure the boundaries are right!
        '1' -> p @Int8 1
        '2' -> p @Int8 2
        '3' -> p @Int8 3
        '4' -> p @Int16 4
        '5' -> p @Int32 5
        '6' -> p @Int64 6
        '7' -> fail [fmt|i parser only supports numbers up to size 6, was 7|]
        '8' -> fail [fmt|i parser only supports numbers up to size 6, was 8|]
        '9' -> fail [fmt|i parser only supports numbers up to size 6, was 9|]
        o -> fail [fmt|i number with length {o & show} not possible|]

    naturalParser = do
      let p :: forall parseSize finalSize. (Bounded parseSize, Integral parseSize, Num finalSize) => (Integer -> Atto.Parser finalSize)
          p n = do
            _ <- Atto.Char.char ':' Atto.<?> [fmt|n{n & show} did not have : after length|]
            int <-
              boundedDecimal @parseSize >>= \case
                Nothing -> fail [fmt|cannot parse into n{n & show}, the number is too big (would overflow)|]
                Just i -> pure i

            _ <- Atto.Char.char ',' Atto.<?> [fmt|n{n & show} did not end with ,|]
            pure $ fromIntegral @parseSize @finalSize int
      let b n = do
            _ <- Atto.Char.char ':' Atto.<?> [fmt|n{n & show} did not have : after length|]
            bool <-
              (Atto.Char.char '0' >> pure False)
                <|> (Atto.Char.char '1' >> pure True)
            _ <- Atto.Char.char ',' Atto.<?> [fmt|n{n & show} did not end with ,|]
            pure bool

      digit <- Atto.Char.digit
      case digit of
        -- TODO: separate parser for n1 and n2 that makes sure the boundaries are right!
        '1' -> N1 <$> b 1
        '2' -> N3 <$> p @Word8 @Word8 2
        '3' -> N3 <$> p @Word8 @Word8 3
        '4' -> N6 <$> p @Word16 @Word64 4
        '5' -> N6 <$> p @Word32 @Word64 5
        '6' -> N6 <$> p @Word64 @Word64 6
        '7' -> fail [fmt|n parser only supports numbers up to size 6, was 7|]
        '8' -> fail [fmt|n parser only supports numbers up to size 6, was 8|]
        '9' -> fail [fmt|n parser only supports numbers up to size 6, was 9|]
        o -> fail [fmt|n number with length {o & show} not possible|]

-- | Parser for a bounded decimal that does not overflow the decimal.
--
--  via https://www.extrema.is/blog/2021/10/20/parsing-bounded-integers
boundedDecimal :: forall a. (Bounded a, Integral a) => Atto.Parser (Maybe a)
boundedDecimal = do
  i :: Integer <- decimal
  pure $
    if (i :: Integer) > fromIntegral (maxBound :: a)
      then Nothing
      else Just $ fromIntegral i
  where
    -- Copied from @Attoparsec.Text@ and adjusted to bytestring
    decimal :: (Integral a2) => Atto.Parser a2
    decimal = ByteString.foldl' step 0 <$> Atto.Char.takeWhile1 Atto.Char.isDigit
      where
        step a c = a * 10 + fromIntegral (c - 48)
{-# SPECIALIZE boundedDecimal :: Atto.Parser (Maybe Int) #-}
{-# SPECIALIZE boundedDecimal :: Atto.Parser (Maybe Int64) #-}
{-# SPECIALIZE boundedDecimal :: Atto.Parser (Maybe Word8) #-}
{-# SPECIALIZE boundedDecimal :: Atto.Parser (Maybe Word64) #-}

-- | 'boundedDecimal', but fail the parser if the decimal overflows.
boundedDecimalFail :: Atto.Parser Int
boundedDecimalFail =
  boundedDecimal >>= \case
    Nothing -> fail "decimal out of range"
    Just a -> pure a

-- | Hedgehog generator for a netencode value.
genNetencode :: Hedge.MonadGen m => m T
genNetencode =
  Gen.recursive
    Gen.choice
    [ -- these are bundled into one Gen, so that scalar elements get chosen less frequently, and the generator produces nicely nested examples
      Gen.frequency
        [ (1, pure unit),
          (1, n1 <$> Gen.bool),
          (1, n3 <$> Gen.element [0, 1, 5]),
          (1, n6 <$> Gen.element [0, 1, 5]),
          (1, i6 <$> Gen.element [-1, 1, 5]),
          (2, text <$> Gen.text (Range.linear 1 10) Gen.lower),
          (2, bytes <$> Gen.bytes (Range.linear 1 10))
        ]
    ]
    [ do
        key <- Gen.text (Range.linear 3 10) Gen.lower
        val <- genNetencode
        pure $ tag key val,
      record
        <$> ( let k = Gen.text (Range.linear 3 10) Gen.lower
                  v = genNetencode
               in NEMap.insertMap
                    <$> k
                    <*> v
                    <*> ( (Gen.map (Range.linear 0 3)) $
                            (,) <$> k <*> v
                        )
            )
    ]

-- | Hedgehog property: encoding a netencode value and parsing it again returns the same result.
prop_netencodeRoundtrip :: Hedge.Property
prop_netencodeRoundtrip = Hedge.property $ do
  enc <- Hedge.forAll genNetencode
  ( Atto.parseOnly
      netencodeParser
      ( netencodeEncodeStable enc
          & Builder.toLazyByteString
          & toStrictBytes
      )
    )
    Hedge.=== (Right enc)
