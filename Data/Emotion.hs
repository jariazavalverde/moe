{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Emotion(
    PAD(..),
    Ekman(..),
    Emotion(..),
    normalize,
    aggregate,
    mapPAD,
    zipPADWith
) where

import Data.Matrix(Matrix, fromLists, fromList, (!))

data PAD = PAD {
    pleasure :: Double,
    arousal :: Double,
    dominance :: Double
} deriving (Eq, Show)

data Ekman = Ekman {
    anger :: Double,
    disgust :: Double,
    fear :: Double,
    joy :: Double,
    sadness :: Double,
    surprise :: Double
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
    toPAD e = let (Ekman a d f h s u) = toEkman e
                  --pad = zhiguo * fromList 5 1 [a,d,f,h,s]
                  --pleasure = h - 0.4*d -0.2*s -0.1*a - 0.3*f 
                  --arousal = -0.7*s + 0.3*d + 0.3*u + 0.3*a + 0.2*f - 0.2*h
                  --dominance = -0.7*f + 0.3*d + 0.7*a + 0.3*h
                  pleasure = -0.51*a -0.4*d - 0.64*f + 0.4*h - 0.4*s
                  arousal = 0.59*a +0.2*d + 0.6*f + 0.2*h - 0.2*s
                  dominance = 0.25*a + 0.1*d - 0.43*f + 0.15*h - 0.5*s
        in PAD pleasure arousal dominance
    toEkman :: e -> Ekman
    toEkman e = let (PAD p a d) = toPAD e
                    anger = sqrt $ (p-(-0.51))^2 + (a-0.59)^2 + (d-0.25)^2
                    disgust = sqrt $ (p-(-0.4))^2 + (a-0.2)^2 + (d-0.1)^2
                    fear = sqrt $ (p-(-0.64))^2 + (a-0.6)^2 + (d-(-0.43))^2
                    joy = sqrt $ (p-0.4)^2 + (a-0.2)^2 + (d-0.15)^2
                    sadness = sqrt $ (p-(-0.4))^2 + (a-(-0.2))^2 + (d-(-0.5))^2
                    surprise = 0
                    total = anger + disgust + fear + joy + sadness + surprise
        in Ekman (anger/total) (disgust/total) (fear/total) (joy/total) (sadness/total) (surprise/total)
    {-# MINIMAL toPAD | toEkman #-}

instance Emotion String where
    toPAD x = let (a,b,c) = read x in PAD a b c
    toEkman x = let (a,b,c,d,e,f) = read x in Ekman a b c d e f

instance Emotion PAD where
    toPAD = id

instance Emotion Ekman where
    toEkman = id

instance Emotion () where
    toPAD _ = PAD 0 0 0
    toEkman _ = Ekman 0 0 0 0 0 0

zhiguo :: Matrix Double
zhiguo = fromLists [
    [-0.51, -0.40, -0.64,  0.40, -0.40],
    [ 0.59,  0.20,  0.60,  0.20, -0.20],
    [ 0.25,  0.10, -0.43,  0.15, -0.50]]

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