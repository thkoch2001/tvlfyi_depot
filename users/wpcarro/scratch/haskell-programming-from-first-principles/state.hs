module StateScratch where

--------------------------------------------------------------------------------
import System.Random
-- import Control.Monad.Trans.State
import Data.Function ((&))

import qualified Control.Applicative as Ap
import qualified Control.Monad as M
--------------------------------------------------------------------------------

data Die
  = DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Integer -> Maybe Die
intToDie 1 = Just DieOne
intToDie 2 = Just DieTwo
intToDie 3 = Just DieThree
intToDie 4 = Just DieFour
intToDie 5 = Just DieFive
intToDie 6 = Just DieSix
intToDie _ = Nothing

rollDie :: Moi StdGen Die
rollDie = do
  (n, s) <- randomR (1, 6)
  case intToDie n of
    Just d  -> pure (d, s)
    Nothing -> pure (DieOne, s)

rollsToGetN :: Integer -> StdGen -> [Die]
rollsToGetN n g = go 0 [] g
  where
    go sum result gen
      | sum >= n = result
      | otherwise =
        let (dice, nextGen) = randomR (1, 6) gen
        in case intToDie dice of
          Nothing -> go (sum + dice) result nextGen
          Just d  -> go (sum + dice) (d : result) nextGen

--------------------------------------------------------------------------------

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi run) =
    Moi $ \s -> let (x, t) = run s
                in (f x, t)

instance Applicative (Moi s) where
  pure x = Moi $ \s -> (x, s)
  (Moi f) <*> (Moi run) =
    Moi $ \s -> let (g, t) = f s
                    (x, u) = run t
                in (g x, u)

instance Monad (Moi s) where
  (Moi run1) >>= f =
    Moi $ \s -> let (x, t) = run1 s
                    (Moi run2) = f x
                in run2 t

--------------------------------------------------------------------------------

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod`  5 == 0 = "Buzz"
           | n `mod`  3 == 0 = "Fizz"
           | otherwise       = show n

--------------------------------------------------------------------------------

get :: Moi s s
get = Moi $ \s -> (s, s)

put :: s -> Moi s ()
put x = Moi $ \s -> ((), x)

exec :: Moi s a -> s -> s
exec (Moi run) x = x & run & snd

eval :: Moi s a -> s -> a
eval (Moi run) x = x & run & fst

modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
