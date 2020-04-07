{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Moe.Detector.FaceApp(
    faceapp,
    faceappLogin,
    faceappWith,
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
import Data.Moe(Media, Detector(..), initialize)
import Data.Moe.PAD(PAD(..), Emotion(..), normalize)

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

instance Emotion FaceAppData where
    toPAD x = normalize 0 100 (PAD
                  (1.0 * happiness x + 0.4 * disgust x + 0.2 * sadness x + 0.1 * anger x + 0.3 * fear x)
                  (0.7 * sadness x + disgust x + surprise x + anger x + fear x + happiness x)
                  (0.7 * fear x + 0.3 * disgust x + 0.7 * anger x + 0.3 * happiness x))

faceappWith :: (String,String) -> Detector IO FaceAppData
faceappWith (api_key,api_secret) = Detector (\media ->
    do r <- try $ post "https://api-us.faceplusplus.com/facepp/v3/detect" [
            "api_key" := pack api_key,
            "api_secret" := pack api_secret,
            "return_attributes" := pack "emotion",
            "image_url" := pack media] :: IO (Either HttpException (Response ByteString))
       case r of
           Left ex   -> empty
           Right val -> do guard $ val ^. responseStatus . statusCode == 200
                           case val ^? responseBody . key "faces" . nth 0 . key "attributes" . key "emotion" of
                               Just json -> return $ fromJust (decode (encode json))
                               Nothing   -> empty)

faceappLogin :: IO (String,String)
faceappLogin = do putStrLn "Enter api key:"
                  api_key <- getLine
                  putStrLn "Enter api secret:"
                  secret_key <- getLine
                  return (api_key,secret_key)

faceapp :: Detector IO FaceAppData
faceapp = initialize faceappWith faceappLogin