module Data.Moe(
    PAD,
    Media,
    Detector(..),
    Padeable(..),
    runDetector,
    normalize
) where

import Control.Applicative(Alternative(..))

type PAD = (Double, Double, Double)
type Media = String

-- | Detector Monad Options Emotions
data Detector m o e = Detector {
    initialize :: m o,
    extractEmotions :: o -> Media -> m e
}

class Padeable e where
    toPAD :: e -> PAD

instance Functor m => Functor (Detector m o) where
    fmap f (Detector init extract) = Detector
        init
        (\opts media -> fmap f $ extract opts media)

instance (Monad m, Monoid o) => Applicative (Detector m o) where
    pure x = Detector (return mempty) (\opts media -> return x)
    Detector fi fe <*> Detector vi ve = Detector
        (do x <- fi
            y <- vi
            return $ mappend x y)
        (\opts media -> do f <- fe opts media
                           x <- ve opts media
                           return $ f x)

instance (Monad m, Monoid o) => Monad (Detector m o) where
    return = pure
    Detector init extract >>= f = Detector
        init
        (\opts media -> do emotions <- extract opts media
                           let detector = f emotions
                           do opts <- initialize detector
                              extractEmotions detector opts media)

instance (Monad m, Alternative m, Monoid o, Monoid (m o)) => Alternative (Detector m o) where
    empty = Detector empty (\opts media -> empty)
    Detector i1 e1 <|> Detector i2 e2 = Detector
        (mappend i1 i2)
        (\opts media -> e1 opts media <|> e2 opts media)

normalize :: Double -> Double -> PAD -> PAD
normalize min max (a,b,c) =  let [x,y,z] = map (\x -> (x-min)/(max-min)) [a,b,c] in (x,y,z)

runDetector :: (Monad m, Padeable e) => Detector m o e -> Media -> m PAD
runDetector detector media = do opts <- initialize detector
                                emotions <- extractEmotions detector opts media
                                return $ toPAD emotions

