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
    surprise :: Double
} deriving (Eq, Ord, Read, Show)

instance Num Ekman where
    (+) = zipEkmanWith (+)
    (-) = zipEkmanWith (-)
    (*) = zipEkmanWith (*)
    abs = mapEkman abs
    signum = mapEkman signum
    fromInteger x = let x' = fromIntegral x in Ekman x' x' x' x' x' x'

instance Fractional Ekman where
    fromRational x = let x' = fromRational x in Ekman x' x' x' x' x' x'
    (/) = zipEkmanWith (/)

mapEkman :: (Double -> Double) -> Ekman -> Ekman
mapEkman g (Ekman a b c d e f) = Ekman (g a) (g b) (g c) (g d) (g e) (g f)

zipEkmanWith :: (Double -> Double -> Double) -> Ekman -> Ekman -> Ekman
zipEkmanWith g (Ekman a b c d e f) (Ekman a' b' c' d' e' f') =
    Ekman (g a a') (g b b') (g c c') (g d d') (g e e') (g f f')

normalizeEkman :: Double -> Double -> Ekman -> Ekman
normalizeEkman min max = mapEkman (\x -> (x-min)/(max-min))

-- | Emotion
class Emotion e where
    toPAD :: e -> PAD
    toPAD e = let (Ekman a d f h s u) = toEkman e
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

instance Emotion PAD where
    toPAD = id

instance Emotion Ekman where
    toEkman = id

instance Emotion () where
    toPAD _ = PAD 0 0 0
    toEkman _ = Ekman 0 0 0 0 0 0