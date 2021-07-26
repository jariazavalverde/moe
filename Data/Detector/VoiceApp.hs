{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports #-}

module Data.Detector.VoiceApp(
    voiceapp,
    voiceappFrom,
    voiceappIO,
    VoiceAppData(..),
    VoiceAppDataItem(..)
) where

import Network.Wreq
import Network.HTTP.Client(HttpException)
import Control.Lens
import Data.Text(Text, pack)
import Data.ByteString.Lazy.Internal(ByteString)
import Data.Aeson.Lens(key, nth)
import Data.Aeson(Object, ToJSON(..), FromJSON, decode)
import Data.Scientific(toRealFloat)
import GHC.Generics(Generic)
import Data.WAVE(getWAVEFile, waveFrameRate, waveHeader)
import "base64" Data.ByteString.Base64(encodeBase64)
import System.Process(runCommand, waitForProcess)
import System.Posix.Temp(mkstemp)
import Data.Maybe(fromJust)
import Control.Exception(try)
import Control.Monad(guard)
import Control.Applicative((<|>), empty)
import qualified Data.ByteString as BS
import Data.Detector(DetectorT, mkDetectorT, initDetectorT)
import Data.Emotion(Ekman(..), Emotion(..), normalizeEkman)
import Data.Channel((:<:)(..))
import Data.Channel.Voice(Voice(..))

data VoiceAppDataItem = VoiceAppDataItem {
    emotion :: String,
    start :: Double,
    end :: Double
} deriving (Generic, Show, Eq)

newtype VoiceAppData = VoiceAppData { getVoiceAppData :: [VoiceAppDataItem] }
    deriving (Generic, Show, Eq)

data VoiceAppSend = VoiceAppSend {
    enconding :: Text,
    sampleRate :: Int,
    content :: Text
} deriving (Generic, Show, Eq)

instance ToJSON VoiceAppDataItem
instance ToJSON VoiceAppData
instance ToJSON VoiceAppSend
instance FromJSON VoiceAppDataItem
instance FromJSON VoiceAppData

instance Emotion VoiceAppDataItem where
    toEkman (VoiceAppDataItem "sad" x y) = 0{sadness = y-x}
    toEkman (VoiceAppDataItem "happy" x y) = 0{joy = y-x}
    toEkman (VoiceAppDataItem "anger" x y) = 0{anger = y-x}
    toEkman (VoiceAppDataItem "excited" x y) = 0{surprise = y-x}
    toEkman (VoiceAppDataItem "frustration" x y) = 0{disgust = y-x}
    toEkman (VoiceAppDataItem _ _ _) = 0

instance Emotion VoiceAppData where
    toEkman (VoiceAppData xs) = normalizeEkman 0 (end $ last xs) $ sum (map toEkman xs)

voiceapp :: Voice String :<: r => String -> (Double,Double) -> DetectorT r IO VoiceAppData
voiceapp api_key (start,end) = mkDetectorT (\input -> 
    do let path = getVoiceChannel $ channel input
       (tmp, _) <- mkstemp path
       process <- runCommand $ "ffmpeg -i " ++ path ++ " -ss " ++ show start ++ " -to " ++ show end ++ " -c copy " ++ tmp ++ ".wav >/dev/null 2>/dev/null"
       waitForProcess process
       wave <- getWAVEFile (tmp ++ ".wav")
       contents <- BS.readFile (tmp ++ ".wav")
       let encoding = "WAVE"
           sampleRate = waveFrameRate (waveHeader wave)
       r <- try $ post ("https://proxy.api.deepaffects.com/audio/generic/api/v2/sync/recognise_emotion?apikey=" ++ api_key) (toJSON $ VoiceAppSend
             (pack encoding) sampleRate (encodeBase64 contents)) :: IO (Either HttpException (Response ByteString))
       case r of
           Left ex   -> print ex >> empty
           Right val -> do guard $ val ^. responseStatus . statusCode == 200
                           case val ^? responseBody of
                               Just json -> return $ VoiceAppData (fromJust (decode json) :: [VoiceAppDataItem])
                               Nothing   -> empty)

voiceappLogin :: IO String
voiceappLogin = putStrLn "Enter api key:" >> getLine

voiceappFrom :: Voice String :<: r => String -> (Double,Double) -> DetectorT r IO VoiceAppData
voiceappFrom path int = initDetectorT (`voiceapp` int) (readFile path)

voiceappIO :: Voice String :<: r => (Double,Double) -> DetectorT r IO VoiceAppData
voiceappIO int = initDetectorT (`voiceapp` int) voiceappLogin