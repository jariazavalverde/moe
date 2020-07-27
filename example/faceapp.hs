import Control.Applicative((<|>), optional)
import Control.Monad(mfilter, filterM, guard)
import Control.Monad.Writer(WriterT(..), Sum(..), Product(..))
import Data.Moe.Detector
import Data.Moe.PAD
import Data.Moe.Detector.FaceApp

run1 :: IO PAD
run1 = do putStrLn "Enter a media:"
          media <- getLine
          login <- faceappLogin
          x <- runDetectorT (toPAD <$> faceappWith login) media
          y <- runDetectorT (toPAD <$> faceappWith login) media
          return $ aggregate [x,y]

detector1 :: DetectorT IO PAD
detector1 = do x <- toPAD <$> faceapp
               y <- toPAD <$> faceapp
               return $ aggregate [x,y]

detector2 :: DetectorT IO FaceAppData
detector2 = faceapp <|> detector2

detector3 :: DetectorT IO (Maybe FaceAppData)
detector3 = optional faceapp

isPleasant :: (Monad m, Emotion e) => DetectorT m e -> Double -> DetectorT m Bool
isPleasant detector eps = do a <- pleasure . toPAD <$> detector
                             return (a > eps)

pleasant :: Emotion e => DetectorT IO e -> DetectorT IO (Maybe e)
pleasant = optional . mfilter (\emotions -> pleasure (toPAD emotions) >= 0.5)

pleasant' :: Emotion e => DetectorT IO e -> DetectorT IO (Maybe e)
pleasant' detector = optional $
                        do emotions <- detector
                           guard $ pleasure (toPAD emotions) > 0.5
                           return emotions

moetip4 :: (Monad m, Emotion e, Foldable m) => [DetectorT m e] -> DetectorT m PAD
moetip4 xs = fmap aggregate $ sequence (map (fmap toPAD) xs)

moetip5 :: (Applicative f, Emotion e) => DetectorT f e -> DetectorT f e -> DetectorT f PAD
moetip5 f g = (/ 2) <$> ((+) <$> fmap toPAD f <*> fmap toPAD g)

moetip6 :: (Monad m, Emotion e) => m e -> WriterT [PAD] m Bool
moetip6 detector = WriterT $ do emotions <- toPAD <$> detector
                                return (pleasure emotions > 0.5, [emotions])

moetip6' :: WriterT [PAD] (DetectorT IO) Bool
moetip6' = do emotions1 <- moetip6 $ faceappFrom "credentials/faceapp.txt"
              emotions2 <- moetip6 $ faceappFrom "credentials/faceapp.txt"
              return (emotions1 && emotions2)

moetip7 :: (Monad m, Emotion e) => m e -> WriterT (m (Sum PAD)) m Bool
moetip7 detector = WriterT $ do emotions <- toPAD <$> detector
                                return (pleasure emotions > 0.5, (Sum . toPAD) <$> detector)

moetip7' :: WriterT (DetectorT IO (Sum PAD)) (DetectorT IO) Bool
moetip7' = do emotions1 <- moetip7 $ faceappFrom "credentials/faceapp.txt"
              emotions2 <- moetip7 $ faceappFrom "credentials/faceapp.txt"
              return (emotions1 && emotions2)