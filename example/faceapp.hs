import Control.Applicative((<|>), optional)
import Control.Monad(mfilter, filterM, guard)
import Data.Moe
import Data.Moe.PAD
import Data.Moe.Detector.FaceApp

run1 :: IO PAD
run1 = do putStrLn "Enter a media:"
          media <- getLine
          login <- faceappLogin
          x <- runDetector (toPAD <$> faceappWith login) media
          y <- runDetector (toPAD <$> faceappWith login) media
          return $ aggregate [x,y]

detector1 :: Detector IO PAD
detector1 = do x <- toPAD <$> faceapp
               y <- toPAD <$> faceapp
               return $ aggregate [x,y]

detector2 :: Detector IO FaceAppData
detector2 = faceapp <|> detector2

detector3 :: Detector IO (Maybe FaceAppData)
detector3 = optional faceapp

isPleasant :: (Monad m, Emotion e) => Detector m e -> Double -> Detector m Bool
isPleasant detector eps = do a <- pleasure . toPAD <$> detector
                             return (a > eps)

pleasant :: Emotion e => Detector IO e -> Detector IO (Maybe e)
pleasant = optional . mfilter (\emotions -> let (PAD a _ _) = toPAD emotions in a >= 0.5)

pleasant' :: Emotion e => Detector IO e -> Detector IO (Maybe e)
pleasant' detector = optional $
                        do emotions <- detector
                           guard $ pleasure (toPAD emotions) > 0.5
                           return emotions

moetip4 :: (Monad m, Emotion e, Foldable m) => [Detector m e] -> Detector m PAD
moetip4 xs = fmap aggregate $ sequence (map (fmap toPAD) xs)

moetip5 :: (Applicative f, Emotion e) => Detector f e -> Detector f e -> Detector f PAD
moetip5 f g = (/ 2) <$> ((+) <$> fmap toPAD f <*> fmap toPAD g)