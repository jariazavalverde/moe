module Data.Moe(
    Media,
    Detector(..),
    initialize
) where

import Control.Applicative(Alternative(..))
import Control.Monad(MonadPlus(..))

type Media = String

-- | Detector Monad Emotions
data Detector m e = Detector {
    runDetector :: Media -> m e
}

instance Functor m => Functor (Detector m) where
    fmap f (Detector run) = Detector (fmap f . run)

instance Applicative m => Applicative (Detector m) where
    pure x = Detector (\media -> pure x)
    f <*> x = Detector (\media -> runDetector f media <*> runDetector x media)

instance Monad m => Monad (Detector m) where
    return x = Detector (\media -> return x)
    x >>= f = Detector (\media -> do emotions <- runDetector x media
                                     runDetector (f emotions) media)

instance Alternative m => Alternative (Detector m) where
    empty = Detector (\media -> empty)
    x <|> y = Detector (\media -> runDetector x media <|> runDetector y media)

instance (Monad m, Alternative m) => MonadPlus (Detector m)

initialize :: Monad m => (a -> Detector m b) -> m a -> Detector m b
initialize detector init = Detector (\media -> do config <- init
                                                  runDetector (detector config) media)