--------------------------------------------------------------------------------
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
module Xanthous.Random
  ( Choose(..)
  , ChooseElement(..)
  , Weighted(..)
  , evenlyWeighted
  , weightedBy
  , subRand
  , chance
  , chooseSubset
  , chooseRange
  , FiniteInterval(..)
  ) where
--------------------------------------------------------------------------------
import           Xanthous.Prelude
--------------------------------------------------------------------------------
import           Data.List.NonEmpty (NonEmpty(..))
import           Control.Monad.Random (Rand, evalRand, mkStdGen, StdGen, RandT, liftRandT, Random)
import           Data.Functor.Compose
import           Data.Random.Shuffle.Weighted
import           Data.Random.Distribution
import           Data.Random.Distribution.Uniform
import           Data.Random.Distribution.Uniform.Exclusive
import           Data.Random.Sample
import           Data.Interval ( Interval, lowerBound', Extended (Finite)
                               , upperBound', Boundary (Closed), lowerBound, upperBound
                               )
import Data.Random (RandomGen, RVar)
--------------------------------------------------------------------------------

class Monad m => MyRandom (m :: Type -> Type) where
  getRandomR :: Random a => (a, a) -> m a
  getRandom :: Random a => m a
  runRVar :: RVar a -> m a
  getRandomDistribution :: Distribution d t => d t -> m t

class Choose a where
  type RandomResult a
  choose :: MyRandom m => a -> m (RandomResult a)

newtype ChooseElement a = ChooseElement a

instance MonoFoldable a => Choose (ChooseElement a) where
  type RandomResult (ChooseElement a) = Maybe (Element a)
  choose (ChooseElement xs) = do
    chosenIdx <- getRandomR (0, olength xs - 1)
    let pick _ (Just x) = Just x
        pick (x, i) Nothing
          | i == chosenIdx = Just x
          | otherwise = Nothing
    pure $ ofoldr pick Nothing $ zip (toList xs) [0..]

instance MonoFoldable a => Choose (NonNull a) where
  type RandomResult (NonNull a) = Element a
  choose
    = fmap (fromMaybe (error "unreachable")) -- why not lol
    . choose
    . ChooseElement
    . toNullable

instance Choose (NonEmpty a) where
  type RandomResult (NonEmpty a) = a
  choose = choose . fromNonEmpty @[_]

instance Choose (a, a) where
  type RandomResult (a, a) = a
  choose (x, y) = choose (x :| [y])

newtype Weighted w t a = Weighted (t (w, a))
  deriving (Functor, Foldable) via (t `Compose` (,) w)

deriving newtype instance Eq (t (w, a)) => Eq (Weighted w t a)
deriving newtype instance Show (t (w, a)) => Show (Weighted w t a)
deriving newtype instance NFData (t (w, a)) => NFData (Weighted w t a)

instance Traversable t => Traversable (Weighted w t) where
  traverse f (Weighted twa) = Weighted <$> (traverse . traverse) f twa

evenlyWeighted :: [a] -> Weighted Int [] a
evenlyWeighted = Weighted . itoList

-- | Weight the elements of some functor by a function. Larger values of 'w' per
-- its 'Ord' instance will be more likely to be generated
weightedBy :: Functor t => (a -> w) -> t a -> Weighted w t a
weightedBy weighting xs = Weighted $ (weighting &&& id) <$> xs

sampleRandT :: (Distribution d t, RandomGen g, Applicative m) => d t -> RandT g m t
sampleRandT dt = liftRandT $ \g -> pure $ samplePure dt g

instance (Num w, Ord w, Distribution Uniform w, Excludable w)
       => Choose (Weighted w [] a) where
  type RandomResult (Weighted w [] a) = Maybe a
  choose (Weighted ws) = runRVar $ headMay <$> weightedSample 1 ws

instance (Num w, Ord w, Distribution Uniform w, Excludable w)
       => Choose (Weighted w NonEmpty a) where
  type RandomResult (Weighted w NonEmpty a) = a
  choose (Weighted ws) =
    runRVar
    $ fromMaybe (error "unreachable") . headMay
    <$> weightedSample 1 (toList ws)

subRand :: MyRandom m => Rand StdGen a -> m a
subRand sub = evalRand sub . mkStdGen <$> getRandom

-- | Has a @n@ chance of returning 'True'
--
-- eg, chance 0.5 will return 'True' half the time
chance
  :: (Num w, Ord w, Distribution Uniform w, Excludable w, Monad m, MyRandom m)
  => w
  -> m Bool
chance n = choose $ weightedBy (bool 1 (n * 2)) bools

-- | Choose a random subset of *about* @w@ of the elements of the given
-- 'Witherable' structure
chooseSubset :: ( Num w, Ord w, Distribution Uniform w, Excludable w
               , Witherable t
               , MyRandom m
               ) => w -> t a -> m (t a)
chooseSubset = filterA . const . chance

-- | Choose a random @n@ in the given interval
chooseRange
  :: ( MyRandom m
    , Distribution Uniform n
    , Enum n
    , Bounded n
    , Ord n
    )
  => Interval n
  -> m (Maybe n)
chooseRange int = traverse getRandomDistribution distribution
  where
    (lower, lowerBoundary) = lowerBound' int
    lowerR = case lower of
      Finite x -> if lowerBoundary == Closed
                 then x
                 else succ x
      _ -> minBound
    (upper, upperBoundary) = upperBound' int
    upperR = case upper of
      Finite x -> if upperBoundary == Closed
                 then x
                 else pred x
      _ -> maxBound
    distribution
      | lowerR <= upperR = Just $ Uniform lowerR upperR
      | otherwise = Nothing

instance ( Distribution Uniform n
         , Enum n
         , Bounded n
         , Ord n
         )
         => Choose (Interval n) where
  type RandomResult (Interval n) = n
  choose dt = fromMaybe (error "Invalid interval") <$> chooseRange dt

newtype FiniteInterval a
  = FiniteInterval { unwrapFiniteInterval :: Interval a }

instance ( Distribution Uniform n
         )
         => Choose (FiniteInterval n) where
  type RandomResult (FiniteInterval n) = n
  -- TODO broken with open/closed right now
  choose
    = getRandomDistribution
    . uncurry Uniform
    . over both getFinite
    . (lowerBound &&& upperBound)
    . unwrapFiniteInterval
    where
      getFinite (Finite x) = x
      getFinite _ = error "Infinite value"

--------------------------------------------------------------------------------

bools :: NonEmpty Bool
bools = True :| [False]
