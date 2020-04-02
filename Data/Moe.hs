module Data.Moe(
    Media,
    Detector(..),
    runDetector
) where

import Control.Monad(ap)
import Control.Applicative(Alternative(..))

type Media = String

-- | Detector Monad Options Emotions
data Detector m o e = Detector {
    initialize :: m o,
    extractEmotions :: o -> Media -> m e
}

instance Functor m => Functor (Detector m o) where
    fmap f (Detector init extract) = Detector
        init
        (\opts media -> fmap f $ extract opts media)

instance (Monad m, Alternative m, Monoid o) => Applicative (Detector m o) where
    pure = return
    (<*>) = ap

instance (Monad m, Alternative m, Monoid o) => Monad (Detector m o) where
    return x = Detector (return mempty) (\opts media -> return x)
    Detector init extract >>= f = Detector
        init
        (\opts media -> do emotions <- extract opts media
                           let detector = f emotions
                           do opts <- initialize detector
                              extractEmotions detector opts media)

instance (Monad m, Alternative m, Monoid o, Monoid (m o)) => Alternative (Detector m o) where
    empty = Detector (return mempty) (\opts media -> empty)
    Detector i1 e1 <|> Detector i2 e2 = Detector
        (mappend i1 i2)
        (\opts media -> e1 opts media <|> e2 opts media)

runDetector :: Monad m => Detector m o e -> Media -> m e
runDetector detector media = do opts <- initialize detector
                                extractEmotions detector opts media