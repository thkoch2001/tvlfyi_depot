module BasicLibrariesScratch where

import Data.Function ((&))

--------------------------------------------------------------------------------
newtype DList a = DL { unDL :: [a] -> [a] }

instance (Show a) => Show (DList a) where
  show (DL x) = "DL " ++ show (x [])

-- | Create an empty difference list.
emptyDList :: DList a
emptyDList = DL $ \xs -> xs
{-# INLINE emptyDList #-}

-- | Create a difference list with `x` as the only member.
singleton :: a -> DList a
singleton x =  DL $ \xs -> x : xs
{-# INLINE singleton #-}

-- | Convert the DList into a list.
toList :: DList a -> [a]
toList (DL unDL) = unDL mempty
{-# INLINE toList #-}

-- | Add an element to the end of a DList.
infixr `snoc`
snoc :: a -> DList a -> DList a
snoc x (DL xs) = DL $ \ys -> xs (x : ys)
{-# INLINE snoc #-}

-- | Add an element to the beginning of a DList.
infixr `cons`
cons :: a -> DList a -> DList a
cons x (DL xs) = DL $ \ys -> x : xs ys
{-# INLINE cons #-}

-- | Combine two DLists together.
append :: DList a -> DList a -> DList a
append (DL xs) (DL ys) = DL $ \zs -> zs & ys & xs
{-# INLINE append #-}

--------------------------------------------------------------------------------
data Queue a =
  Queue { one :: [a]
        , two :: [a]
        } deriving (Show, Eq)

emptyQueue :: Queue a
emptyQueue = Queue mempty mempty

enqueue :: a -> Queue a -> Queue a
enqueue x (Queue en de) = Queue (x:en) de

dequeue :: Queue a -> Maybe (a, Queue a)
dequeue (Queue [] []) = Nothing
dequeue (Queue en []) =
  let (d:de) = reverse en
  in Just (d, Queue de [])
dequeue (Queue en (d:de)) = Just (d, Queue en de)
