import Data.Moe
import Data.Moe.PAD
import Data.Moe.Detector.FaceApp

main = do media <- getLine
          x <- runDetector faceapp media
          y <- runDetector faceapp media
          return (x,y)

main2 = do putStrLn "Enter a media:"
           media <- getLine
           opts <- initialize faceapp
           x <- extractEmotions (toPAD <$> faceapp) opts media
           y <- extractEmotions (toPAD <$> faceapp) opts media
           return $ aggregate [x,y]