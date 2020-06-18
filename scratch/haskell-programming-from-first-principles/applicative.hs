module ApplicativeScratch where

import Data.Function ((&))

import Control.Applicative (liftA3)
import qualified Data.List as List
import qualified GHC.Base as Base

--------------------------------------------------------------------------------

-- xs :: [(Integer, Integer)]
-- xs = zip [1..3] [4..6]

-- added :: Maybe Integer
-- added =
--   (+3) <$> (lookup 3 xs)

--------------------------------------------------------------------------------

-- y :: Maybe Integer
-- y = lookup 3 xs

-- z :: Maybe Integer
-- z = lookup 2 xs

-- tupled :: Maybe (Integer, Integer)
-- tupled = Base.liftA2 (,) y z

--------------------------------------------------------------------------------

-- x :: Maybe Int
-- x = List.elemIndex 3 [1..5]

-- y :: Maybe Int
-- y = List.elemIndex 4 [1..5]

-- maxed :: Maybe Int
-- maxed = Base.liftA2 max x y

--------------------------------------------------------------------------------

xs = [1..3]
ys = [4..6]

x :: Maybe Integer
x = lookup 3 $ zip xs ys

y :: Maybe Integer
y = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = sum <$> Base.liftA2 (,) x y

--------------------------------------------------------------------------------

newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

--------------------------------------------------------------------------------

newtype Constant a b =
  Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant x) = Constant x

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (Constant x) <*> (Constant y) = Constant (x <> y)

--------------------------------------------------------------------------------

one = const <$> Just "Hello" <*> Just "World"

two :: Maybe (Integer, Integer, String, [Integer])
two = (,,,) <$> (Just 90)
            <*> (Just 10)
            <*> (Just "Tierness")
            <*> (Just [1..3])

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs =
    (f <$> xs) <> (fs <*> xs)

toList :: List a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

--------------------------------------------------------------------------------

newtype ZipList' a =
  ZipList' [a]
  deriving (Eq, Show)

-- instance Eq a => EqProp (ZipList' a) where
--   (ZipList' lhs) =-= (ZipList' rhs) =
--     (take 1000 lhs) `eq` (take 1000 rhs)

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure x = ZipList' (repeat x)
  (ZipList' fs) <*> (ZipList' xs) =
    ZipList' $ zipWith ($) fs xs

--------------------------------------------------------------------------------

data Validation e a
  = Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap f (Failure x) = Failure x
  fmap f (Success x) = Success (f x)

instance Monoid e => Applicative (Validation e) where
  pure = undefined
  (Success f) <*> (Success x) = Success (f x)
  _ <*> (Failure x) = Failure x
  (Failure x) <*> _ = Failure x

data Error
  = DivideByZero
  | StackOverflow
  deriving (Eq, Show)

--------------------------------------------------------------------------------

stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos xs ys zs =
  liftA3 (,,) xs ys zs

--------------------------------------------------------------------------------

data Pair a = Pair a a deriving Show

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f g) <*> (Pair x y) = Pair (f x) (g x)

p :: Pair Integer
p = Pair 1 2

--------------------------------------------------------------------------------

data Two a b = Two a b

instance Functor (Two a) where
  fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
  pure x = Two mempty x
  _ <*> _ = undefined

--------------------------------------------------------------------------------

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (Three a b f) <*> (Three x y z) = Three (a <> x) (b <> y) (f z)

--------------------------------------------------------------------------------

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' x y z) = Three' x (f y) (f z)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (Three' a f g) <*> (Three' x y z) = Three' (a <> x) (f y) (g z)
