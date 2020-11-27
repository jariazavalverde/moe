module Data.Detector(
    Detector,
    DetectorT,
    mkDetectorT,
    mkDetector,
    runDetectorT,
    runDetector,
    initDetectorT
) where

import Control.Applicative(liftA2)
import Control.Monad.Reader(ReaderT(..), runReader, runReaderT)
import Data.Functor.Identity(Identity(..))

-- | DetectorT Media Monad Emotion
type DetectorT r m a = ReaderT r m a

-- | Detector Media Emotion
type Detector r a = DetectorT r Identity a

-- | Make a detector from a function
mkDetectorT :: Monad m => (r -> m a) -> DetectorT r m a
mkDetectorT = ReaderT

mkDetector :: (r -> a) -> Detector r a
mkDetector f = ReaderT (fmap Identity f)

-- | Run a detector
runDetectorT :: Monad m => DetectorT r m a -> r -> m a
runDetectorT = runReaderT

runDetector :: Detector r a -> r -> a
runDetector = runReader

-- | Make a detector from a function and an effect to initialize it
initDetectorT :: Monad m => (a -> DetectorT r m b) -> m a -> DetectorT r m b
initDetectorT detector init = mkDetectorT
    (\media -> do config <- init
                  runDetectorT (detector config) media)

instance (Applicative f, Monoid m) => Semigroup (ReaderT r f m) where
    (<>) = liftA2 mappend

instance (Applicative f, Monoid m) => Monoid (ReaderT r f m) where
    mempty = pure mempty