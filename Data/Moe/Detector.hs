module Data.Moe.Detector(
    Media,
    Detector,
    DetectorT,
    mkDetectorT,
    mkDetector,
    runDetectorT,
    runDetector,
    initDetectorT
) where

import Control.Applicative(liftA2)
import Control.Monad.Reader(Reader, ReaderT(..), runReader, runReaderT)
import Data.Functor.Identity(Identity(..))

-- | Media type
type Media = String

-- | DetectorT Monad Emotion
type DetectorT m = ReaderT Media m

-- | Detector Emotion
type Detector = DetectorT Identity

-- | Make a detector from a function
mkDetectorT :: Monad m => (Media -> m e) -> DetectorT m e
mkDetectorT = ReaderT

mkDetector :: (Media -> e) -> Detector e
mkDetector f = ReaderT (fmap Identity f)

-- | Run a detector
runDetectorT :: Monad m => DetectorT m e -> Media -> m e
runDetectorT = runReaderT

runDetector :: Detector e -> Media -> e
runDetector = runReader

-- | Make a detector from a function and an effect to initialize it
initDetectorT :: Monad m => (a -> DetectorT m b) -> m a -> DetectorT m b
initDetectorT detector init = mkDetectorT
    (\media -> do config <- init
                  runDetectorT (detector config) media)

instance (Applicative f, Monoid m) => Monoid (ReaderT r f m) where
    mempty = pure mempty
    mappend = liftA2 mappend