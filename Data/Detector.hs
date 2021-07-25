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
import Control.Monad.Trans.Class(MonadTrans(..))
import Data.Functor.Identity(Identity(..))

-- | DetectorT Media Monad Emotion
data DetectorT r m a = DetectorT { runDetectorT :: r -> m a }

-- | Detector Media Emotion
type Detector r a = DetectorT r Identity a

-- | Make a detector from a function
mkDetectorT :: Monad m => (r -> m a) -> DetectorT r m a
mkDetectorT = DetectorT

mkDetector :: (r -> a) -> Detector r a
mkDetector f = DetectorT (fmap Identity f)

-- | Run a detector
runDetector :: Detector r a -> r -> a
runDetector = fmap runIdentity  . runDetectorT

-- | Make a detector from a function and an effect to initialize it
initDetectorT :: Monad m => (a -> DetectorT r m b) -> m a -> DetectorT r m b
initDetectorT detector init = mkDetectorT
    (\media -> do config <- init
                  runDetectorT (detector config) media)

instance Functor f => Functor (DetectorT r f) where
    fmap f d = DetectorT (\r -> let x = runDetectorT d r in fmap f x)

instance Applicative f => Applicative (DetectorT r f) where
    pure x = DetectorT (\_ -> pure x)
    df <*> dx = DetectorT (\r -> let f = runDetectorT df r
                                     x = runDetectorT dx r
                                  in f <*> x)

instance Monad m => Monad (DetectorT r m) where
    d >>= f = DetectorT (\r -> do x <- runDetectorT d r
                                  runDetectorT (f x) r)

instance MonadTrans (DetectorT r) where
    lift m = DetectorT (const m)