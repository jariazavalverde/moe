{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Detector.FaceApp(
    faceapp,
    faceappFrom,
    faceappIO,
    FaceAppData(..)
) where

import Network.Wreq
import Network.HTTP.Client(HttpException)
import Control.Lens
import Data.Text(Text, pack)
import Data.ByteString.Lazy.Internal(ByteString)
import Data.Aeson.Lens(key, nth)
import Data.Aeson(Object, ToJSON(..), FromJSON, encode, decode)
import Data.Scientific(toRealFloat)
import GHC.Generics(Generic)
import Data.Maybe(fromJust)
import Control.Exception(try)
import Control.Monad(guard)
import Control.Applicative((<|>), empty)
import Data.Detector(DetectorT, mkDetectorT, initDetectorT)
import Data.Emotion(Ekman(Ekman), Emotion(..))
import Data.Channel((:<:)(..))
import Data.Channel.Face(Face(..))

data FaceAppData = FaceAppData {
    happiness :: Double,
    disgust :: Double,
    sadness :: Double,
    surprise :: Double,
    fear :: Double,
    anger :: Double,
    neutral :: Double
} deriving (Generic, Show, Eq)

instance ToJSON FaceAppData
instance FromJSON FaceAppData

instance Emotion FaceAppData where
    toEkman e = Ekman
        (anger e / 50 - 1)
        (disgust e / 50 - 1)
        (fear e / 50 - 1)
        (happiness e / 50 - 1)
        (sadness e / 50 - 1)
        (surprise e / 50 - 1)
        (neutral e / 50 - 1)

faceapp :: Face String :<: r => (String, String) -> DetectorT r IO FaceAppData
faceapp (api_key, api_secret) = mkDetectorT (\input ->
    do r <- try $ post "https://api-us.faceplusplus.com/facepp/v3/detect" [
            "api_key" := pack api_key,
            "api_secret" := pack api_secret,
            "return_attributes" := pack "emotion",
            "image_url" := pack (getFaceChannel $ channel input)] :: IO (Either HttpException (Response ByteString))
       case r of
           Left ex   -> print ex >> empty
           Right val -> do guard $ val ^. responseStatus . statusCode == 200
                           case val ^? responseBody . key "faces" . nth 0 . key "attributes" . key "emotion" of
                               Just json -> return $ fromJust (decode (encode json))
                               Nothing   -> empty)

faceappLogin :: IO (String, String)
faceappLogin = do putStrLn "Enter api key:"
                  api_key <- getLine
                  putStrLn "Enter api secret:"
                  api_secret <- getLine
                  return (api_key,api_secret)

faceappFrom :: Face String :<: r => String -> DetectorT r IO FaceAppData
faceappFrom path = initDetectorT faceapp $ do content <- readFile path
                                              let (api_key:api_secret:_) = lines content
                                              return (api_key,api_secret)

faceappIO :: Face String :<: r => DetectorT r IO FaceAppData
faceappIO = initDetectorT faceapp faceappLogin