module Data.Emotion(
    PAD(..),
    mapPAD,
    zipPADWith,
    normalizePAD,
    Ekman(..),
    mapEkman,
    zipEkmanWith,
    normalizeEkman,
    Emotion(..)
) where

import  Data.Matrix(Matrix, inverse, transpose, fromLists, toList)

-- | PAD
data PAD = PAD {
    pleasure :: Double,
    arousal :: Double,
    dominance :: Double
} deriving (Eq, Ord, Read, Show)

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

mapPAD :: (Double -> Double) -> PAD -> PAD
mapPAD f (PAD a b c) = PAD (f a) (f b) (f c)

zipPADWith :: (Double -> Double -> Double) -> PAD -> PAD -> PAD
zipPADWith f (PAD a b c) (PAD x y z) = PAD (f a x) (f b y) (f c z)

normalizePAD :: Double -> Double -> PAD -> PAD
normalizePAD min max =  mapPAD (\x -> (x-min)/(max-min))

-- | Ekman
data Ekman = Ekman {
    anger :: Double,
    disgust :: Double,
    fear :: Double,
    joy :: Double,
    sadness :: Double,
    surprise :: Double,
    neutral :: Double
} deriving (Eq, Ord, Read, Show)

instance Num Ekman where
    (+) = zipEkmanWith (+)
    (-) = zipEkmanWith (-)
    (*) = zipEkmanWith (*)
    abs = mapEkman abs
    signum = mapEkman signum
    fromInteger x = let x' = fromIntegral x in Ekman x' x' x' x' x' x' x'

instance Fractional Ekman where
    fromRational x = let x' = fromRational x in Ekman x' x' x' x' x' x' x'
    (/) = zipEkmanWith (/)

mapEkman :: (Double -> Double) -> Ekman -> Ekman
mapEkman h (Ekman a b c d e f g) = Ekman (h a) (h b) (h c) (h d) (h e) (h f) (h g)

zipEkmanWith :: (Double -> Double -> Double) -> Ekman -> Ekman -> Ekman
zipEkmanWith h (Ekman a b c d e f g) (Ekman a' b' c' d' e' f' g') =
    Ekman (h a a') (h b b') (h c c') (h d d') (h e e') (h f f') (h g g')

normalizeEkman :: Double -> Double -> Ekman -> Ekman
normalizeEkman min max = mapEkman (\x -> (x-min)/(max-min))

-- | Emotion
class Emotion e where
    toPAD :: e -> PAD
    toPAD e = let (Ekman e1 e2 e3 e4 e5 e6 e7) = toEkman e
                  u = fromLists [[e1,e2,e3,e4,e5,e6,e7]]
                  v = linearMap e
                  v' = case inverse (v * transpose v) of
                      Left msg -> error msg
                      Right i  -> transpose v * i
                  [p,a,d] = toList (u * v')
               in PAD p a d
    toEkman :: e -> Ekman
    toEkman e = let (PAD p a d) = toPAD e
                    u = fromLists [[p,a,d]]
                    v = linearMap e
                    [e1,e2,e3,e4,e5,e6,e7] = toList (u * v)
                in Ekman e1 e2 e3 e4 e5 e6 e7
    linearMap :: e -> Matrix Double
    linearMap _ = fromLists
        {--[[-0.51, -0.40, -0.64,  0.40, -0.40],
         [ 0.59,  0.20,  0.60,  0.20, -0.20],
         [ 0.25,  0.10, -0.43,  0.15, -0.50]]--}
        [[-0.29, -0.14, -0.19,  0.46, -0.30,  0.24,  0.52],
         [ 0.19, -0.08,  0.14,  0.07, -0.11,  0.15,  0.53],
         [-0.02, -0.02, -0.10,  0.19, -0.18,  0.08,  0.50]]
    {-# MINIMAL toPAD | toEkman #-}
 
 

instance Emotion PAD where
    toPAD = id

instance Emotion Ekman where
    toEkman = id

instance Emotion () where
    toPAD _ = PAD 0 0 0
    toEkman _ = Ekman 0 0 0 0 0 0 0