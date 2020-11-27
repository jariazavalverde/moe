{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import Data.Detector
import Data.Detector.FaceApp(faceapp, faceappFrom)
import Data.Detector.Dummy
import Data.Emotion
import Data.Channel
import Data.Channel.Face
import Data.Channel.Voice

run1 :: IO PAD
run1 = do putStrLn "Enter a media:"
          media <- getLine
          runDetectorT (toPAD <$> faceappFrom "credentials/faceapp.key") (Face media)

isHappy :: (Monad m, Emotion e) => m e -> m Bool
isHappy d = (> 0.5) . joy . toEkman <$> d

isAngry :: (Monad m, Emotion e) => m e -> m Bool
isAngry = isHappy

isHappyOrAngry :: (Monad m, Emotion e) => m e -> m Bool
isHappyOrAngry d = (||) <$> isHappy d <*> isAngry d

d1 :: Face String :<: r => DetectorT r IO PAD
d1 = toPAD <$> faceappFrom "credentials/faceapp.key"

d2 :: (Face String :<: r, () :<: r) => DetectorT r IO (PAD,())
d2 = do x <- d1
        y <- dummy
        return (x,y)

mean :: (Monad m, Emotion a, Emotion b) => m a -> m b -> m PAD 
mean d1 d2 = do x <- toPAD <$> d1
                y <- toPAD <$> d2
                return ((x+y)/2)