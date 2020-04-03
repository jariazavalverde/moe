module Data.Moe(
    Media,
    Detector(..),
    initialize
) where

import Control.Applicative(Alternative(..))

type Media = String

-- | Detector Monad Emotions
data Detector m e = Detector {
    runDetector :: Media -> m e
}

instance Functor m => Functor (Detector m) where
    fmap f (Detector run) = Detector (fmap f . run)

instance Applicative m => Applicative (Detector m) where
    pure x = Detector (\media -> pure x)
    Detector f <*> Detector x = Detector (\media -> f media <*> x media)

instance Monad m => Monad (Detector m) where
    return x = Detector (\media -> return x)
    Detector run >>= f = Detector (\media -> do emotions <- run media
                                                runDetector (f emotions) media)

instance Alternative m => Alternative (Detector m) where
    empty = Detector (\media -> empty)
    x <|> y = Detector (\media -> runDetector x media <|> runDetector y media)

initialize :: Monad m => (a -> Detector m b) -> m a -> Detector m b
initialize detector init = Detector (\media -> do config <- init
                                                  runDetector (detector config) media)