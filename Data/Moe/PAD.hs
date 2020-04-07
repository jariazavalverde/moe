{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Moe.PAD(
    PAD(..),
    Emotion(..),
    normalize,
    aggregate,
    mapPAD,
    zipPADWith
) where

data PAD = PAD {
    pleasure :: Double,
    arousal :: Double,
    dominance :: Double
} deriving (Eq, Show)

instance Num PAD where
    (+) = zipPADWith (+)
    (-) = zipPADWith (-)
    (*) = zipPADWith (*)
    abs = mapPAD abs
    signum = mapPAD signum
    fromInteger x = let x' = fromIntegral x in PAD x' x' x'

instance Fractional PAD where
    fromRational x = let x' = fromRational x in PAD x' x' x'
    (/) = zipPADWith (/)

class Emotion e where
    toPAD :: e -> PAD

instance Emotion String where
    toPAD x = let (a,b,c) = read x in PAD a b c

normalize :: Double -> Double -> PAD -> PAD
normalize min max (PAD a b c) =  let [x,y,z] = map (\x -> (x-min)/(max-min)) [a,b,c] in (PAD x y z)

mapPAD :: (Double -> Double) -> PAD -> PAD
mapPAD f (PAD a b c) = PAD (f a) (f b) (f c)

zipPADWith :: (Double -> Double -> Double) -> PAD -> PAD -> PAD
zipPADWith f (PAD a b c) (PAD x y z) = PAD (f a x) (f b y) (f c z)

aggregate :: Foldable t => t PAD -> PAD
aggregate xs = let (PAD x y z) = foldl1 (\(PAD as bs cs) (PAD a b c) -> PAD (as+a) (bs+b) (cs+c)) xs
                   len = fromIntegral $ length xs
               in PAD (x/len) (y/len) (z/len)