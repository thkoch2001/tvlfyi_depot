module ComposingTypesScratch where

import Data.Function ((&))
import Data.Bifunctor

import qualified Data.Foldable as F

--------------------------------------------------------------------------------

newtype Identity a =
  Identity { getIdentity :: a }
  deriving (Eq, Show)

newtype Compose f g a =
  Compose { getCompose :: f (g a) }
  deriving (Eq, Show)

--------------------------------------------------------------------------------

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose getCompose) = Compose $ (fmap . fmap) f getCompose

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = x & pure & pure & Compose
  fgf <*> fga = undefined

--------------------------------------------------------------------------------

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap toMonoid x = undefined

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse = undefined

--------------------------------------------------------------------------------

data Deux a b = Deux a b deriving (Show, Eq)

instance Bifunctor Deux where
  bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a deriving (Show, Eq)

instance Bifunctor Const where
  bimap f _ (Const x) = Const (f x)

data Drei a b c = Drei a b c deriving (Show, Eq)

instance Bifunctor (Drei a) where
  bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b deriving (Show, Eq)

instance Bifunctor (SuperDrei a) where
  bimap f g (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a deriving (Show, Eq)

instance Bifunctor (SemiDrei a) where
  bimap _ _ (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
  bimap f g (Quadzzz w x y z) = Quadzzz w x (f y) (g z)

-- | Analogue for Either
data LeftRight a b
  = Failure a
  | Success b
  deriving (Show, Eq)

instance Bifunctor LeftRight where
  bimap f _ (Failure x) = Failure (f x)
  bimap _ g (Success y) = Success (g y)
