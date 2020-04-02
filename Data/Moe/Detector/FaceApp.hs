{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Moe.Detector.FaceApp(
    faceapp
) where

import Prelude hiding (lookup)
import Network.Wreq
import Control.Lens
import Data.Text(Text, pack)
import Data.Aeson.Lens(key, nth)
import Data.Aeson(Result(..), Object, Value(..), fromJSON)
import Data.HashMap.Strict(lookup)
import Data.Scientific(toRealFloat)
import Data.Moe(PAD, Media, Detector(..), Padeable(..), normalize)

faceappInit :: IO (String, String)
faceappInit = do putStrLn "Enter api key:"
                 api_key <- getLine
                 putStrLn "Enter secret key:"
                 secret_key <- getLine
                 return (api_key, secret_key)

faceappExtract :: (String, String) -> Media -> IO (Result Object)
faceappExtract opts media =
    do r <- post "https://api-us.faceplusplus.com/facepp/v3/detect" [
           "api_key" := pack (fst opts),
           "api_secret" := pack (snd opts),
           "return_attributes" := pack "emotion",
           "image_url" := pack media]
       return (case r ^? responseBody . key "faces" . nth 0 . key "attributes" . key "emotion" of
           Just json -> fromJSON json
           Nothing   -> Error "unknown error")

instance Padeable  (Result Object) where
    toPAD (Error _) = (0.0, 0.0, 0.0)
    toPAD (Success xs) = let get x = case lookup x xs of
                                         Just (Number x) -> toRealFloat x
                                         otherwise       -> 0.0
                         in normalize 0 100 (
                             1.0 * get "happiness" + 0.4 * get "disgust" + 0.2 * get "sadness" + 0.1 * get "anger" + 0.3 * get "fear",
                             0.7 * get "sadness" + get "disgust" + get "surprise" + get "anger" + get "fear" + get "happiness",
                             0.7 * get "fear" + 0.3 * get "disgust" + 0.7 * get "anger" + 0.3 * get "happiness")

faceapp :: Detector IO (String, String) (Result Object)
faceapp = Detector faceappInit faceappExtract