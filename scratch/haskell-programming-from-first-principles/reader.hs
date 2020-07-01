module Reader where

import Data.Char
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Control.Applicative as A
import qualified Data.Maybe as MB

cap :: String -> String
cap xs = xs <&> toUpper

rev :: String -> String
rev = reverse

compose :: String -> String
compose xs = xs & rev . cap

fmapped :: String -> String
fmapped xs = xs & rev <$> cap

tupled :: String -> (String, String)
tupled xs = A.liftA2 (,) cap rev $ xs

tupled' :: String -> (String, String)
tupled' = do
  capResult <- cap
  revResult <- rev
  pure (revResult, capResult)

--------------------------------------------------------------------------------

newtype Reader r a = Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

--------------------------------------------------------------------------------

newtype HumanName = HumanName String
  deriving (Eq, Show)

newtype DogName = DogName String
  deriving (Eq, Show)

newtype Address = Address String
  deriving (Eq, Show)

data Person
  = Person
  { humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)

data Dog
  = Dog
  { dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)

pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")

chris :: Person
chris =
  Person (HumanName "Chris Allen")
         (DogName "Papu")
         (Address "Austin")

getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
  A.liftA2 Dog dogName address

--------------------------------------------------------------------------------

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f x y =
  f <$> x <*> y

asks :: (r -> a) -> Reader r a
asks f = Reader f

--------------------------------------------------------------------------------

instance Functor (Reader a) where
  fmap f (Reader ab) = Reader $ f . ab

instance Applicative (Reader a) where
  pure x = Reader $ \_ -> x
  (Reader rab) <*> (Reader ra) = Reader $ do
    ab <- rab
    fmap ab ra

--------------------------------------------------------------------------------

instance Monad (Reader r) where
  return = pure
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader x) >>= f = undefined

--------------------------------------------------------------------------------

x = [1..3]
y = [4..6]
z = [7..9]

xs :: Maybe Integer
xs = zip x y & lookup 3

ys :: Maybe Integer
ys = zip y z & lookup 6

zs :: Maybe Integer
zs = zip x y & lookup 4

z' :: Integer -> Maybe Integer
z' n = zip x y & lookup n

x1 :: Maybe (Integer, Integer)
x1 = A.liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = A.liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

summed :: Num a => (a, a) -> a
summed (x, y) = x + y

bolt :: Integer -> Bool
bolt x = x > 3 && x < 8

main :: IO ()
main = do
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ bolt 7
  print $ bolt <$> z
  print $ sequenceA [(>3), (<8) ,even] 7
