module MonadTransformersScratch where

import Data.Function ((&))
--------------------------------------------------------------------------------

newtype MaybeT m a =
  MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor f) => Functor (MaybeT f) where
  fmap f (MaybeT run) =
    MaybeT $ (fmap . fmap) f run

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = x & pure & pure & MaybeT
  _ <*> _ = undefined

instance (Monad m) => Monad (MaybeT m) where
  return = pure
  (MaybeT ma) >>= f = MaybeT $ do
    maybeX <- ma
    case maybeX of
      Nothing -> pure Nothing
      Just x -> x & f & runMaybeT

--------------------------------------------------------------------------------

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mEither) =
    EitherT $ (fmap . fmap) f mEither

instance (Applicative m) => Applicative (EitherT e m) where
  pure x = EitherT $ (pure . pure) x
  EitherT mEitherF <*> EitherT mEitherX =
    EitherT $ (fmap (<*>) mEitherF) <*> mEitherX

instance (Monad m) => Monad (EitherT e m) where
  return = pure
  EitherT mEitherX >>= f = EitherT $ do
    eitherX <- mEitherX
    case eitherX of
      Left x -> pure $ Left x
      Right x -> runEitherT $ f x

swapEither :: Either l r -> Either r l
swapEither (Left x) = Right x
swapEither (Right x) = Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mEitherX) =
  EitherT $ fmap swapEither mEitherX

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT aToMC bToMC (EitherT mEitherX) = do
  eitherX <- mEitherX
  case eitherX of
    Left x -> aToMC x
    Right x -> bToMC x

--------------------------------------------------------------------------------

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  fmap f (ReaderT rma) =
    ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = x & pure & pure & ReaderT
  ReaderT f <*> ReaderT x = ReaderT $ fmap (<*>) f <*> x

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
  ReaderT rma >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r
