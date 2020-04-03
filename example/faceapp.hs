import Control.Applicative((<|>), optional)
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