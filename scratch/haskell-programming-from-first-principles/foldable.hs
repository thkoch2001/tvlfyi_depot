module FoldableScratch where

import Data.Function ((&))

--------------------------------------------------------------------------------

sum :: (Foldable t, Num a) => t a -> a
sum xs =
  foldr (+) 0 xs

product :: (Foldable t, Num a) => t a -> a
product xs =
  foldr (*) 1 xs

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem y xs =
  foldr (\x acc -> if acc then acc else y == x) False xs

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum xs =
  foldr (\x acc ->
           case acc of
             Nothing   -> Just x
             Just curr -> Just (min curr x)) Nothing xs

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum xs =
  foldr (\x acc ->
           case acc of
             Nothing   -> Nothing
             Just curr -> Just (max curr x)) Nothing xs

-- TODO: How could I use QuickCheck to see if Prelude.null and this null return
-- the same results for the same inputs?
null :: (Foldable t) => t a -> Bool
null xs =
  foldr (\_ _ -> False) True xs

length :: (Foldable t) => t a -> Int
length xs =
  foldr (\_ acc -> acc + 1) 0 xs

toList :: (Foldable t) => t a -> [a]
toList xs =
  reverse $ foldr (\x acc -> x : acc) [] xs

fold :: (Foldable t, Monoid m) => t m -> m
fold xs =
  foldr mappend mempty xs

foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap f xs =
  foldr (\x acc -> mappend (f x) acc) mempty xs

--------------------------------------------------------------------------------

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Foldable List where
  foldr f acc (Cons x rest) = foldr f (f x acc) rest
  foldr f acc Nil = acc

fromList :: [a] -> List a
fromList [] = Nil
fromList (x:rest) = Cons x (fromList rest)

--------------------------------------------------------------------------------

data Constant a b = Constant b deriving (Eq, Show)

-- TODO: Is this correct?
instance Foldable (Constant a) where
  foldr f acc (Constant x) = f x acc

--------------------------------------------------------------------------------

data Two a b = Two a b deriving (Eq, Show)

instance Foldable (Two a) where
  foldr f acc (Two x y) = f y acc

--------------------------------------------------------------------------------

data Three a b c = Three a b c deriving (Eq, Show)

instance Foldable (Three a b) where
  foldr f acc (Three x y z) = f z acc

--------------------------------------------------------------------------------

data Three' a b = Three' a b b deriving (Eq, Show)

instance Foldable (Three' a) where
  foldr f acc (Three' x y z) = acc & f z & f y

--------------------------------------------------------------------------------

data Four' a b = Four' a b b b deriving (Eq, Show)

instance Foldable (Four' a) where
  foldr f acc (Four' w x y z) = acc & f z & f y & f x

--------------------------------------------------------------------------------

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF pred xs =
  foldr (\x acc -> if pred x then pure x `mappend` acc else acc) mempty xs
