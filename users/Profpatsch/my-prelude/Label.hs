{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Label
  ( Label,
    label,
    label',
    getLabel,
    T2 (..),
    T3 (..),
  )
where

import Data.Data (Proxy (..))
import Data.Function ((&))
import Data.Typeable (Typeable)
import GHC.Records (HasField (..))
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

-- | A labelled value.
--
-- Use 'label'/'label'' to construct,
-- then use dot-syntax to get the inner value.
newtype Label (label :: Symbol) value = Label value
  deriving stock (Eq, Ord)
  deriving newtype (Typeable, Semigroup, Monoid)

instance (KnownSymbol label, Show value) => Show (Label label value) where
  showsPrec d (Label val) =
    showParen (d > 10) $
      showString "Label @"
        . showsPrec 11 (symbolVal (Proxy @label))
        . showString " "
        . showsPrec 11 val

-- | Attach a label to a value; should be used with a type application to name the label.
--
-- @@
-- let f = label @"foo" 'f' :: Label "foo" Char
-- in f.foo :: Char
-- @@
--
-- Use dot-syntax to get the labelled value.
label :: forall label value. value -> Label label value
label value = Label value

-- | Attach a label to a value; Pass it a proxy with the label name in the argument type.
-- This is intended for passing through the label value;
-- you can also use 'label'.
--
--
-- @@
-- let f = label' (Proxy @"foo") 'f' :: Label "foo" Char
-- in f.foo :: Char
-- @@
--
-- Use dot-syntax to get the labelled value.
label' :: forall label value. (Proxy label) -> value -> Label label value
label' Proxy value = Label value

-- | Fetches the labelled value.
instance HasField label (Label label value) value where
  getField :: (Label label value) -> value
  getField (Label value) = value

-- | Fetch a value from a record, like 'getField', but also keep it wrapped by its label.
getLabel :: forall label record a. HasField label record a => record -> Label label a
getLabel rec = rec & getField @label & label @label

-- | A named 2-element tuple. Since the elements are named, you can access them with `.`.
--
-- @@
-- let t2 = T2 (label @"myfield" 'c') (label @"otherfield" True) :: T2 "myfield" Char "otherfield" Bool
-- in (
--   t2.myfield :: Char,
--   t2.otherfield :: Bool
-- )
-- @@
data T2 (l1 :: Symbol) t1 (l2 :: Symbol) t2 = T2 (Label l1 t1) (Label l2 t2)

-- | Access the first field by label
instance HasField l1 (T2 l1 t1 l2 t2) t1 where
  getField (T2 t1 _) = getField @l1 t1

-- | Access the second field by label
instance HasField l2 (T2 l1 t1 l2 t2) t2 where
  getField (T2 _ t2) = getField @l2 t2

instance (Semigroup t1, Semigroup t2) => Semigroup (T2 l1 t1 l2 t2) where
  T2 t1 t2 <> T2 t1' t2' = T2 (t1 <> t1') (t2 <> t2')

instance (Monoid t1, Monoid t2) => Monoid (T2 l1 t1 l2 t2) where
  mempty = T2 mempty mempty

-- | A named 3-element tuple. Since the elements are named, you can access them with `.`. See 'T2' for an example.
data T3 (l1 :: Symbol) t1 (l2 :: Symbol) t2 (l3 :: Symbol) t3 = T3 (Label l1 t1) (Label l2 t2) (Label l3 t3)

-- | Access the first field by label
instance HasField l1 (T3 l1 t1 l2 t2 l3 t3) t1 where
  getField (T3 t1 _ _) = getField @l1 t1

-- | Access the second field by label
instance HasField l2 (T3 l1 t1 l2 t2 l3 t3) t2 where
  getField (T3 _ t2 _) = getField @l2 t2

-- | Access the third field by label
instance HasField l3 (T3 l1 t1 l2 t2 l3 t3) t3 where
  getField (T3 _ _ t3) = getField @l3 t3

instance (Semigroup t1, Semigroup t2, Semigroup t3) => Semigroup (T3 l1 t1 l2 t2 l3 t3) where
  T3 t1 t2 t3 <> T3 t1' t2' t3' = T3 (t1 <> t1') (t2 <> t2') (t3 <> t3')

instance (Monoid t1, Monoid t2, Monoid t3) => Monoid (T3 l1 t1 l2 t2 l3 t3) where
  mempty = T3 mempty mempty mempty
