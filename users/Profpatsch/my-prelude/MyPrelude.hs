{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyPrelude
  ( -- * Text conversions
    Text,
    ByteString,
    Word8,
    fmt,
    textToString,
    stringToText,
    showToText,
    textToBytesUtf8,
    textToBytesUtf8Lazy,
    bytesToTextUtf8,
    bytesToTextUtf8Lazy,
    bytesToTextUtf8Lenient,
    bytesToTextUtf8LenientLazy,
    bytesToTextUtf8Unsafe,
    bytesToTextUtf8UnsafeLazy,
    toStrict,
    toLazy,
    toStrictBytes,
    toLazyBytes,
    charToWordUnsafe,

    -- * IO
    putStrLn,
    putStderrLn,
    exitWithMessage,

    -- * WIP code
    todo,

    -- * Control flow
    (&),
    (<&>),
    (<|>),
    foldMap1,
    foldMap',
    join,
    when,
    unless,
    guard,
    ExceptT,
    runExceptT,
    MonadError,
    throwError,
    MonadIO,
    liftIO,
    MonadReader,
    asks,
    Bifunctor,
    first,
    second,
    bimap,
    foldMap,
    fold,
    foldl',
    mapMaybe,
    findMaybe,
    Traversable,
    for,
    for_,
    traverse,
    traverse_,
    traverseFold,
    traverseFold1,
    traverseFoldDefault,

    -- * Data types
    Coercible,
    coerce,
    Proxy (Proxy),
    Map,
    annotate,
    Validation (Success, Failure),
    failure,
    successes,
    failures,
    eitherToValidation,
    eitherToListValidation,
    validationToEither,
    These (This, That, These),
    eitherToThese,
    eitherToListThese,
    validationToThese,
    thenThese,
    thenValidate,
    NonEmpty ((:|)),
    singleton,
    nonEmpty,
    nonEmptyDef,
    toList,
    toNonEmptyDefault,
    maximum1,
    minimum1,
    Generic,
    Semigroup,
    sconcat,
    Monoid,
    mconcat,
    Void,
    absurd,
    Identity (Identity, runIdentity),
    Natural,
    intToNatural,
    Contravariant,
    contramap,
    (>$<),
    (>&<),
    Profunctor,
    dimap,
    lmap,
    rmap,
    Semigroupoid,
    Category,
    (<<<),
    (>>>),

    -- * Enum definition
    inverseFunction,
    inverseMap,

    -- * Error handling
    HasCallStack,
    module Data.Error,
    smushErrors,
  )
where

import Control.Applicative ((<|>))
import Control.Category (Category, (<<<), (>>>))
import Control.Monad (guard, join, unless, when)
import Control.Monad.Except
  ( ExceptT,
    MonadError,
    runExceptT,
    throwError,
  )
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (Identity (Identity))
import Control.Monad.Reader (MonadReader, asks)
import Data.Bifunctor (Bifunctor, bimap, first, second)
import Data.ByteString
  ( ByteString,
  )
import qualified Data.ByteString.Lazy
import qualified Data.Char
import Data.Coerce (Coercible, coerce)
import Data.Data (Proxy (Proxy))
import Data.Error
import Data.Foldable (Foldable (foldMap', toList), fold, foldl', for_, traverse_)
import qualified Data.Foldable as Foldable
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Functor.Contravariant (Contravariant (contramap), (>$<))
import Data.Functor.Identity (Identity (runIdentity))
import Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import Data.Map.Strict
  ( Map,
  )
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Maybe as Maybe
import Data.Profunctor (Profunctor, dimap, lmap, rmap)
import Data.Semigroup (Max (Max, getMax), Min (Min, getMin), sconcat)
import Data.Semigroup.Foldable (Foldable1 (fold1), foldMap1)
import Data.Semigroup.Traversable (Traversable1)
import Data.Semigroupoid (Semigroupoid)
import Data.Text
  ( Text,
  )
import qualified Data.Text
import qualified Data.Text.Encoding
import qualified Data.Text.Encoding.Error
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Data.These (These (That, These, This))
import Data.Traversable (for)
import Data.Void (Void, absurd)
import Data.Word (Word8)
import GHC.Exception (errorCallWithCallStackException)
import GHC.Exts (RuntimeRep, TYPE, raise#)
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import GHC.Stack (HasCallStack)
import PyF (fmt)
import qualified System.Exit
import qualified System.IO
import Validation
  ( Validation (Failure, Success),
    eitherToValidation,
    failure,
    failures,
    successes,
    validationToEither,
  )

-- | Forward-applying 'contramap', like '&'/'$' and '<&>'/'<$>' but for '>$<'.
(>&<) :: Contravariant f => f b -> (a -> b) -> f a
(>&<) = flip contramap

infixl 5 >&<

-- | encode a Text to a UTF-8 encoded Bytestring
textToBytesUtf8 :: Text -> ByteString
textToBytesUtf8 = Data.Text.Encoding.encodeUtf8

-- | encode a lazy Text to a UTF-8 encoded lazy Bytestring
textToBytesUtf8Lazy :: Data.Text.Lazy.Text -> Data.ByteString.Lazy.ByteString
textToBytesUtf8Lazy = Data.Text.Lazy.Encoding.encodeUtf8

bytesToTextUtf8 :: ByteString -> Either Error Text
bytesToTextUtf8 = first exceptionToError . Data.Text.Encoding.decodeUtf8'

bytesToTextUtf8Lazy :: Data.ByteString.Lazy.ByteString -> Either Error Data.Text.Lazy.Text
bytesToTextUtf8Lazy = first exceptionToError . Data.Text.Lazy.Encoding.decodeUtf8'

-- | decode a Text from a ByteString that is assumed to be UTF-8 (crash if that is not the case)
bytesToTextUtf8Unsafe :: ByteString -> Text
bytesToTextUtf8Unsafe = Data.Text.Encoding.decodeUtf8

-- | decode a Text from a ByteString that is assumed to be UTF-8 (crash if that is not the case)
bytesToTextUtf8UnsafeLazy :: Data.ByteString.Lazy.ByteString -> Data.Text.Lazy.Text
bytesToTextUtf8UnsafeLazy = Data.Text.Lazy.Encoding.decodeUtf8

-- | decode a Text from a ByteString that is assumed to be UTF-8,
-- replace non-UTF-8 characters with the replacment char U+FFFD.
bytesToTextUtf8Lenient :: Data.ByteString.ByteString -> Data.Text.Text
bytesToTextUtf8Lenient =
  Data.Text.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

-- | decode a lazy Text from a lazy ByteString that is assumed to be UTF-8,
-- replace non-UTF-8 characters with the replacment char U+FFFD.
bytesToTextUtf8LenientLazy :: Data.ByteString.Lazy.ByteString -> Data.Text.Lazy.Text
bytesToTextUtf8LenientLazy =
  Data.Text.Lazy.Encoding.decodeUtf8With Data.Text.Encoding.Error.lenientDecode

-- | Make a lazy text strict
toStrict :: Data.Text.Lazy.Text -> Text
toStrict = Data.Text.Lazy.toStrict

-- | Make a strict text lazy
toLazy :: Text -> Data.Text.Lazy.Text
toLazy = Data.Text.Lazy.fromStrict

toStrictBytes :: Data.ByteString.Lazy.ByteString -> ByteString
toStrictBytes = Data.ByteString.Lazy.toStrict

toLazyBytes :: ByteString -> Data.ByteString.Lazy.ByteString
toLazyBytes = Data.ByteString.Lazy.fromStrict

textToString :: Text -> String
textToString = Data.Text.unpack

stringToText :: String -> Text
stringToText = Data.Text.pack

showToText :: (Show a) => a -> Text
showToText = stringToText . show

-- | Unsafe conversion between 'Char' and 'Word8'. This is a no-op and
-- silently truncates to 8 bits Chars > '\255'. It is provided as
-- convenience for ByteString construction.
--
-- Use if you want to get the 'Word8' representation of a character literal.
-- Don’t use on arbitrary characters!
--
-- >>> charToWordUnsafe ','
-- 44
charToWordUnsafe :: Char -> Word8
charToWordUnsafe = fromIntegral . Data.Char.ord
{-# INLINE charToWordUnsafe #-}

-- | Single element in a (non-empty) list.
singleton :: a -> NonEmpty a
singleton a = a :| []

-- | If the given list is empty, use the given default element and return a non-empty list.
nonEmptyDef :: a -> [a] -> NonEmpty a
nonEmptyDef def xs =
  xs & nonEmpty & \case
    Nothing -> def :| []
    Just ne -> ne

-- | Construct a non-empty list, given a default value if the ist list was empty.
toNonEmptyDefault :: a -> [a] -> NonEmpty a
toNonEmptyDefault def xs = case xs of
  [] -> def :| []
  (x : xs') -> x :| xs'

-- | @O(n)@. Get the maximum element from a non-empty structure.
maximum1 :: (Foldable1 f, Ord a) => f a -> a
maximum1 xs = xs & foldMap1 Max & getMax

-- | @O(n)@. Get the minimum element from a non-empty structure.
minimum1 :: (Foldable1 f, Ord a) => f a -> a
minimum1 xs = xs & foldMap1 Min & getMin

-- | Annotate a 'Maybe' with an error message and turn it into an 'Either'.
annotate :: err -> Maybe a -> Either err a
annotate err = \case
  Nothing -> Left err
  Just a -> Right a

-- | Find the first element for which pred returns `Just a`, and return the `a`.
--
-- Example:
-- @
-- >>> :set -XTypeApplications
-- >>> import qualified Text.Read
--
-- >>> findMaybe (Text.Read.readMaybe @Int) ["foo"]
-- Nothing
-- >>> findMaybe (Text.Read.readMaybe @Int) ["foo", "34.40", "34", "abc"]
-- Just 34
findMaybe :: Foldable t => (a -> Maybe b) -> t a -> Maybe b
findMaybe mPred list =
  let pred' x = Maybe.isJust $ mPred x
   in case Foldable.find pred' list of
        Just a -> mPred a
        Nothing -> Nothing

-- | Like 'eitherToValidation', but puts the Error side into a NonEmpty list
-- to make it combine with other validations.
eitherToListValidation :: Either a c -> Validation (NonEmpty a) c
eitherToListValidation = first singleton . eitherToValidation

-- | Convert an 'Either' to a 'These'.
eitherToThese :: Either err a -> These err a
eitherToThese (Left err) = This err
eitherToThese (Right a) = That a

-- | Like 'eitherToThese', but puts the Error side into a NonEmpty list
-- to make it combine with other theses.
eitherToListThese :: Either err a -> These (NonEmpty err) a
eitherToListThese (Left e) = This (singleton e)
eitherToListThese (Right a) = That a

-- | Convert a 'Validation' to a 'These'.
validationToThese :: Validation err a -> These err a
validationToThese (Failure err) = This err
validationToThese (Success a) = That a

-- | Nested '>>=' of a These inside some other @m@.
--
-- Use if you want to collect errors and successes, and want to chain multiple function returning 'These'.
thenThese ::
  (Monad m, Semigroup err) =>
  (a -> m (These err b)) ->
  m (These err a) ->
  m (These err b)
thenThese f x = do
  th <- x
  join <$> traverse f th

-- | Nested validating bind-like combinator inside some other @m@.
--
-- Use if you want to collect errors, and want to chain multiple functions returning 'Validation'.
thenValidate ::
  (Monad m) =>
  (a -> m (Validation err b)) ->
  m (Validation err a) ->
  m (Validation err b)
thenValidate f x =
  eitherToValidation <$> do
    x' <- validationToEither <$> x
    case x' of
      Left err -> pure $ Left err
      Right a -> validationToEither <$> f a

-- | Put the text to @stderr@.
putStderrLn :: Text -> IO ()
putStderrLn msg =
  System.IO.hPutStrLn System.IO.stderr $ textToString msg

exitWithMessage :: Text -> IO a
exitWithMessage msg = do
  putStderrLn msg
  System.Exit.exitWith $ System.Exit.ExitFailure (-1)

-- | Run some function producing applicative over a traversable data structure,
-- then collect the results in a Monoid.
--
-- Very helpful with side-effecting functions returning @(Validation err a)@:
--
-- @
-- let
--   f :: Text -> IO (Validation (NonEmpty Error) Text)
--   f t = pure $ if t == "foo" then Success t else Failure (singleton ("not foo: " <> t))
--
-- in traverseFold f [ "foo", "bar", "baz" ]
--   == Failure ("not foo bar" :| ["not foo baz"])
-- @
--
-- … since @(Semigroup err => Validation err a)@ is a @Semigroup@/@Monoid@ itself.
traverseFold :: (Applicative ap, Traversable t, Monoid m) => (a -> ap m) -> t a -> ap m
traverseFold f xs =
  -- note: could be weakened to (Foldable t) via `getAp . foldMap (Ap . f)`
  fold <$> traverse f xs
{-# INLINE traverseFold #-}

-- | Like 'traverseFold', but fold over a semigroup instead of a Monoid, by providing a starting element.
traverseFoldDefault :: (Applicative ap, Traversable t, Semigroup m) => m -> (a -> ap m) -> t a -> ap m
traverseFoldDefault def f xs = foldDef def <$> traverse f xs
  where
    foldDef = foldr (<>)
{-# INLINE traverseFoldDefault #-}

-- | Same as 'traverseFold', but with a 'Semigroup' and 'Traversable1' restriction.
traverseFold1 :: (Applicative ap, Traversable1 t, Semigroup s) => (a -> ap s) -> t a -> ap s
-- note: cannot be weakened to (Foldable1 t) because there is no `Ap` for Semigroup (No `Apply` typeclass)
traverseFold1 f xs = fold1 <$> traverse f xs
{-# INLINE traverseFold1 #-}

-- | Use this in places where the code is still to be implemented.
--
-- It always type-checks and will show a warning at compile time if it was forgotten in the code.
--
-- Use instead of 'error' and 'undefined' for code that hasn’t been written.
--
-- Uses the same trick as https://hackage.haskell.org/package/protolude-0.3.0/docs/src/Protolude.Error.html#error
{-# WARNING todo "'todo' (undefined code) remains in code" #-}
todo :: forall (r :: RuntimeRep). forall (a :: TYPE r). HasCallStack => a
todo = raise# (errorCallWithCallStackException "This code was not yet implemented: TODO" ?callStack)

-- TODO: use a Text.Builder?

-- | Pretty print a bunch of errors, on multiple lines, prefixed by the given message,
-- then turn the result back into an 'Error'.
--
-- Example:
--
-- smushErrors "There was a problem with the frobl"
--   [ (anyhow "frobz")
--   , (errorContext "oh no" (anyhow "barz"))
--   ]
--
-- ==>
-- "There was a problem with the frobl\n\
-- - frobz\n\
-- - oh no: barz\n"
-- @
--
-- TODO how do we make this compatible with/integrate it into the Error library?
smushErrors :: Foldable t => Text -> t Error -> Error
smushErrors msg errs =
  errs
    -- hrm, pretty printing and then creating a new error is kinda shady
    & foldMap (\err -> "\n- " <> prettyError err)
    & newError
    & errorContext msg

-- | Convert an integer to a 'Natural' if possible
--
-- Named the same as the function from "GHC.Natural", but does not crash.
intToNatural :: Integral a => a -> Maybe Natural
intToNatural i =
  if i < 0
    then Nothing
    else Just $ fromIntegral i

-- | @inverseFunction f@ creates a function that is the inverse of a given function
-- @f@. It does so by constructing 'M.Map' internally for each value @f a@. The
-- implementation makes sure that the 'M.Map' is constructed only once and then
-- shared for every call.
--
-- __Memory usage note:__ don't inverse functions that have types like 'Int'
-- as their result. In this case the created 'M.Map' will have huge size.
--
-- The complexity of reversed mapping is \(\mathcal{O}(\log n)\).
--
-- __Performance note:__ make sure to specialize monomorphic type of your functions
-- that use 'inverseFunction' to avoid 'M.Map' reconstruction.
--
-- One of the common 'inverseFunction' use-case is inverting the 'show' or a 'show'-like
-- function.
--
-- >>> data Color = Red | Green | Blue deriving (Show, Enum, Bounded)
-- >>> parse = inverseFunction show :: String -> Maybe Color
-- >>> parse "Red"
-- Just Red
-- >>> parse "Black"
-- Nothing
--
-- __Correctness note:__ 'inverseFunction' expects /injective function/ as its argument,
-- i.e. the function must map distinct arguments to distinct values.
--
-- Typical usage of this function looks like this:
--
-- @
-- __data__ GhcVer
--    = Ghc802
--    | Ghc822
--    | Ghc844
--    | Ghc865
--    | Ghc881
--    __deriving__ ('Eq', 'Ord', 'Show', 'Enum', 'Bounded')
--
-- showGhcVer :: GhcVer -> 'Text'
-- showGhcVer = \\__case__
--    Ghc802 -> "8.0.2"
--    Ghc822 -> "8.2.2"
--    Ghc844 -> "8.4.4"
--    Ghc865 -> "8.6.5"
--    Ghc881 -> "8.8.1"
--
-- parseGhcVer :: 'Text' -> 'Maybe' GhcVer
-- parseGhcVer = 'inverseFunction' showGhcVer
--
-- Taken from relude’s @Relude.Extra.Enum@.
inverseFunction ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  (a -> k) ->
  (k -> Maybe a)
inverseFunction f k = Map.lookup k $ inverseMap f

-- | Like `inverseFunction`, but instead of returning the function
-- it returns a mapping from all possible outputs to their possible inputs.
--
-- This has the same restrictions of 'inverseFunction'.
inverseMap ::
  forall a k.
  (Bounded a, Enum a, Ord k) =>
  (a -> k) ->
  Map k a
inverseMap f =
  universe
    <&> (\a -> (f a, a))
    & Map.fromList
  where
    universe :: (Bounded a, Enum a) => [a]
    universe = [minBound .. maxBound]
