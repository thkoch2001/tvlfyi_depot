module MonadScratch where

import Data.Function ((&))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Control.Applicative (liftA2)
import qualified Control.Monad as Monad

--------------------------------------------------------------------------------

bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = Monad.join $ fmap f x

--------------------------------------------------------------------------------

fTrigger :: Functor f => f (Int, String, [Int])
fTrigger = undefined

aTrigger :: Applicative a => a (Int, String, [Int])
aTrigger = undefined

mTrigger :: Monad m => m (Int, String, [Int])
mTrigger = undefined

--------------------------------------------------------------------------------

data Sum a b
  = Fst a
  | Snd b
  deriving (Eq, Show)

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = frequency [ (1, Fst <$> arbitrary)
                        , (1, Snd <$> arbitrary)
                        ]

instance Functor (Sum a) where
  fmap f (Fst x) = Fst x
  fmap f (Snd x) = Snd (f x)

instance Applicative (Sum a) where
  pure x = Snd x
  (Snd f) <*> (Snd x) = Snd (f x)
  (Snd f) <*> (Fst x) = Fst x
  (Fst x) <*> _ = Fst x

instance Monad (Sum a) where
  (Fst x) >>= _ = Fst x
  (Snd x) >>= f = f x

--------------------------------------------------------------------------------

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Arbitrary (Nope a) where
  arbitrary = pure NopeDotJpg

instance EqProp (Nope a) where
  (=-=) = eq

instance Functor Nope where
  fmap f _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  NopeDotJpg >>= f = NopeDotJpg

--------------------------------------------------------------------------------

data BahEither b a
  = PLeft a
  | PRight b
  deriving (Eq, Show)

instance (Arbitrary b, Arbitrary a) => Arbitrary (BahEither b a) where
  arbitrary = frequency [ (1, PLeft <$> arbitrary)
                        , (1, PRight <$> arbitrary)
                        ]

instance (Eq a, Eq b) => EqProp (BahEither a b) where
  (=-=) = eq

instance Functor (BahEither b) where
  fmap f (PLeft x) = PLeft (f x)
  fmap _ (PRight x) = PRight x

instance Applicative (BahEither b) where
  pure = PLeft
  (PRight x) <*> _ = PRight x
  (PLeft f) <*> (PLeft x) = PLeft (f x)
  _ <*> (PRight x) = PRight x

instance Monad (BahEither b) where
  (PRight x) >>= _ = PRight x
  (PLeft x) >>= f = f x

--------------------------------------------------------------------------------

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity x) = Identity (f x)

instance Monad Identity where
  (Identity x) >>= f = f x

--------------------------------------------------------------------------------

data List a
  = Nil
  | Cons a (List a)
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = frequency [ (1, pure Nil)
                        , (1, Cons <$> arbitrary <*> arbitrary)
                        ]

instance Eq a => EqProp (List a) where
  (=-=) = eq

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs)

instance Semigroup (List a) where
  Nil <> xs = xs
  xs <> Nil = xs
  (Cons x xs) <> ys =
    Cons x (xs <> ys)

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  (Cons f fs) <*> xs =
    (f <$> xs) <> (fs <*> xs)

instance Monad List where
  Nil >>= _ = Nil
  (Cons x xs) >>= f = (f x) <> (xs >>= f)

--------------------------------------------------------------------------------

j :: Monad m => m (m a) -> m a
j = Monad.join

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = Monad.liftM

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 = Monad.liftM2

a :: Monad m => m a -> m (a -> b) -> m b
a = flip (<*>)

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh xs f = flipType $ f <$> xs

flipType :: Monad m => [m a] -> m [a]
flipType [] = pure mempty
flipType (m:ms) =
  m >>= (\x -> (x:) <$> flipType ms)
