{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Moe.Detector.FaceApp(
    faceapp,
    FaceAppData(..)
) where

import Network.Wreq
import Network.HTTP.Client(HttpException)
import Control.Lens
import Data.Text(Text, pack)
import Data.ByteString.Lazy.Internal(ByteString)
import Data.Aeson.Lens(key, nth)
import Data.Aeson(Object, ToJSON(..), FromJSON, defaultOptions, genericToEncoding, encode, decode)
import Data.Scientific(toRealFloat)
import GHC.Generics(Generic)
import Data.Maybe(fromJust, isJust)
import Control.Exception(try)
import Control.Monad(guard)
import Control.Applicative((<|>), empty)
import Data.Moe(Media, Detector(..))
import Data.Moe.PAD(PAD, Padeable(..), normalize)

data FaceAppData = FaceAppData {
    happiness :: Double,
    disgust :: Double,
    sadness :: Double,
    surprise :: Double,
    fear :: Double,
    anger :: Double
} deriving (Generic, Show)

instance ToJSON FaceAppData
instance FromJSON FaceAppData

faceappInit :: IO [(String,String)]
faceappInit = do putStrLn "Enter api key:"
                 api_key <- getLine
                 putStrLn "Enter api secret:"
                 secret_key <- getLine
                 return [("api_key",api_key),("api_secret",secret_key)]

faceappExtract :: [(String,String)] -> Media -> IO FaceAppData
faceappExtract opts media =
    do guard $ isJust (lookup "api_key" opts) &&  isJust (lookup "api_secret" opts)
       r <- try $ post "https://api-us.faceplusplus.com/facepp/v3/detect" [
            "api_key" := pack (fromJust $ lookup "api_key" opts),
            "api_secret" := pack (fromJust $ lookup "api_secret" opts),
            "return_attributes" := pack "emotion",
            "image_url" := pack media] :: IO (Either HttpException (Response ByteString))
       case r of
           Left ex   -> empty
           Right val -> do guard $ val ^. responseStatus . statusCode == 200
                           case val ^? responseBody . key "faces" . nth 0 . key "attributes" . key "emotion" of
                               Just json -> return $ fromJust (decode (encode json))
                               Nothing   -> empty

instance Padeable FaceAppData where
    toPAD x = normalize 0 100 (
                  1.0 * happiness x + 0.4 * disgust x + 0.2 * sadness x + 0.1 * anger x + 0.3 * fear x,
                  0.7 * sadness x + disgust x + surprise x + anger x + fear x + happiness x,
                  0.7 * fear x + 0.3 * disgust x + 0.7 * anger x + 0.3 * happiness x)

faceapp :: Detector IO [(String,String)] FaceAppData
faceapp = Detector faceappInit faceappExtract