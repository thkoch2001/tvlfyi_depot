module TraversableScratch where

import qualified Data.Foldable as F

import Test.QuickCheck

newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity x) = Identity (f x)

instance Foldable Identity where
  foldMap f (Identity x) = f x

instance Traversable Identity where
  traverse f (Identity x) = Identity <$> f x

--------------------------------------------------------------------------------

data Optional a
  = Nada
  | Some a
  deriving (Eq, Show)

instance Functor Optional where
  fmap f Nada = Nada
  fmap f (Some x) = Some (f x)

instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Some x) = f x

instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Some x) = Some <$> f x

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Foldable List where
  foldMap f Nil = mempty
  foldMap f (Cons x xs) = mappend (f x) (foldMap f xs)

instance Traversable List where
  sequenceA Nil = pure Nil
  sequenceA (Cons x xs) = Cons <$> x <*> sequenceA xs

--------------------------------------------------------------------------------

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three x y z) = Three x y (f z)

instance Foldable (Three a b) where
  foldMap f (Three _ _ z) = f z

instance Traversable (Three a b) where
  sequenceA (Three x y z) = (\z' -> Three x y z') <$> z

--------------------------------------------------------------------------------

data Pair a b = Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair x y) = Pair x (f y)

instance Foldable (Pair a) where
  foldMap f (Pair x y) = f y

instance Traversable (Pair a) where
  sequenceA (Pair x y) = (\y' -> Pair x y') <$> y

--------------------------------------------------------------------------------

data Big a b = Big a b b
  deriving (Eq, Show)

instance Functor (Big a) where
  fmap f (Big x y z) = Big x (f y) (f z)

instance Foldable (Big a) where
  foldMap f (Big x y z) = f y <> f z

instance Traversable (Big a) where
  sequenceA (Big x y z) = (\y' z' -> Big x y' z') <$> y <*> z

--------------------------------------------------------------------------------

data Bigger a b = Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger w x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  sequenceA (Bigger w x y z) = (\x' y' z' -> Bigger w x' y' z') <$> x <*> y <*> z

--------------------------------------------------------------------------------

data Tree a
  = Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node lhs x rhs) = Node (fmap f lhs) (f x) (fmap f rhs)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node lhs x rhs) = (foldMap f lhs) <> (f x) <> (foldMap f rhs)

instance Traversable Tree where
  sequenceA Empty = pure Empty
  sequenceA (Leaf x) = Leaf <$> x
  sequenceA (Node lhs x rhs) = Node <$> sequenceA lhs <*> x <*> sequenceA rhs
