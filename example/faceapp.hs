{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad.Writer
import Data.Detector
import Data.Detector.FaceApp(faceapp, faceappFrom, faceappIO)
import Data.Detector.VoiceApp(voiceapp, voiceappFrom, voiceappIO)
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
isHappy d = (> 0) . joy . toEkman <$> d

isAngry :: (Monad m, Emotion e) => m e -> m Bool
isAngry d = (> 0) . anger . toEkman <$> d

isHappyOrAngry :: (Monad m, Emotion e) => m e -> m Bool
isHappyOrAngry d = (||) <$> isHappy d <*> isAngry d

d1 :: Face String :<: r => DetectorT r IO PAD
d1 = toPAD <$> faceappFrom "credentials/faceapp.key"

d2 :: (Face String :<: r, () :<: r) => DetectorT r IO (PAD,())
d2 = do x <- d1
        y <- dummy
        return (x,y)

mean :: (Num a, Fractional a, Traversable t, Monad m) => t (m a) -> m a
mean ds = do xs <- sequence ds
             return $ sum xs / (fromIntegral (length xs))

wd :: (Face String :<: r, Voice String :<: r) => WriterT [String] (DetectorT r IO) PAD
wd = do face <- lift $ faceappFrom "credentials/faceapp.key"
        tell ["get face data: " ++ show face]
        voice <- lift $ voiceappFrom "credentials/voiceapp.key" (0,5)
        tell ["get voice data: " ++ show voice]
        let mean = (toPAD face + toPAD voice) / 2
        tell ["(face + voice)/2: " ++ show mean]
        return mean

iod :: (Face String :<: r, Voice String :<: r) => DetectorT r IO PAD
iod = do api_key <- lift getLine
         api_secret <- lift getLine
         face <- dummy
         lift $ putStrLn ("get face data: " ++ show face)
         voice <- dummy
         lift $ putStrLn ("get voice data: " ++ show voice)
         let mean = (toPAD face + toPAD voice) / 2
         lift $ putStrLn ("(face + voice)/2: " ++ show mean)
         return mean