module MonadTransformersScratch where

import Control.Monad
import qualified Control.Monad.Trans.Maybe as M
import qualified Control.Monad.Trans.Reader as R
import qualified Control.Monad.Trans.State as S
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

-- instance (Monad m) => Monad (ReaderT r m) where
--   return = pure
--   ReaderT rma >>= f =
--     ReaderT $ \r -> do
--       a <- rma r
--       runReaderT (f a) r
-- --------------------------------------------------------------------------------

rDec :: Num a => R.Reader a a
rDec = R.ReaderT $ \x -> pure $ x + 1

rShow :: Show a => R.Reader a String
rShow = R.ReaderT $ \x -> pure $ show x

rPrintAndInc :: (Num a, Show a) => R.ReaderT a IO a
rPrintAndInc = R.ReaderT $ \x ->
  putStrLn ("Hi: " ++ show x) >> pure (x + 1)

sPrintIncAccum :: (Num a, Show a) => S.StateT a IO String
sPrintIncAccum = S.StateT $ \x -> do
  putStrLn ("Hi: " ++ show x)
  pure (show x, x + 1)

--------------------------------------------------------------------------------

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: M.MaybeT IO String
maybeExcite = M.MaybeT $ do
  x <- getLine
  putStrLn ""
  case isValid x of
    False -> pure Nothing
    True -> pure $ Just x

doExcite :: IO ()
doExcite = do
  putStr "Say something *exciting*: "
  excite <- M.runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "Gonna need some more excitement..."
    Just x  -> putStrLn "Now THAT'S exciting...nice!"

--------------------------------------------------------------------------------

data Participant
  = Man
  | Machine
  deriving (Show, Eq)

newtype Hand = Hand (Integer, Integer) deriving (Show, Eq)

newtype Score = Score (Integer, Integer) deriving (Show, Eq)

getLineLn :: String -> IO String
getLineLn prompt = do
  putStr prompt
  x <- getLine
  putStrLn ""
  pure x

promptGuess :: IO Hand
promptGuess = do
  fingers <- getLineLn "How many fingers (0-5): "
  guess <- getLineLn "Guess: "
  pure $ Hand (read guess, read fingers)

aiGuess :: IO Hand
aiGuess = pure $ Hand (2, 3)

whoWon :: Hand -> Hand -> Maybe Participant
whoWon (Hand (guessA, fingersA)) (Hand (guessB, fingersB))
  | guessA == guessB && guessA == (fingersA + fingersB) = Nothing
  | guessA == (fingersA + fingersB) = Just Man
  | guessB == (fingersA + fingersB) = Just Machine
  | otherwise = Nothing

initScore :: Score
initScore = Score (0, 0)

printScore :: Score -> IO ()
printScore (Score (man, machine)) =
  putStrLn $ "Man: " ++ show man ++ " Machine: " ++ show machine

startMorra :: S.StateT Score IO ()
startMorra = S.StateT $ \(Score (man, machine)) -> do
  Hand (guessA, fingersA) <- promptGuess
  Hand (guessB, fingersB) <- aiGuess
  putStrLn $ "P: " ++ show fingersA ++ "," ++ show guessA
  putStrLn $ "C: " ++ show fingersB ++ "," ++ show guessB
  case whoWon (Hand (guessA, fingersA)) (Hand (guessB, fingersB)) of
    Nothing -> do
      putStrLn "Nobody won..."
      printScore (Score (man, machine))
      pure ((), Score (man, machine))
    Just Man -> do
      putStrLn "Man won!"
      printScore (Score (man + 1, machine))
      pure ((), Score (man + 1, machine))
    Just Machine -> do
      putStrLn "Oh no... Machine won..."
      printScore (Score (man, machine + 1))
      pure ((), Score (man, machine + 1))

playMorra = S.runStateT (forever startMorra) initScore
