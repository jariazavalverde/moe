# Moe

## A monadic multimodal emotion recognition framework to detect and combine emotions in Haskell

### Usage

Moe represents emotion detectors as functions that take a media source and return a result `e` (see [Data.Moe](Data/Moe.hs)). Often these detectors consist of HTTP calls to web API's, so the result of a detector is wrapped into an underlying monad `m` (usually `IO`).

```haskell
data Detector m e = Detector {
    runDetector :: Media -> m e
}
```

Moe uses the [PAD emotional state model](https://en.wikipedia.org/wiki/PAD_emotional_state_model) to describe and measure emotional states. The `Padeable` class (see [Data.Moe.PAD](Data/Moe/PAD.hs)) allows the results of different detectors to be aggregated together.

```haskell
type PAD = (Double, Double, Double)

class Padeable e where
    toPAD :: e -> PAD
```


### Monadic detectors

In Moe, `Detector`s are monads, so we can create new detectors from others in a concise and elegant way:

```haskell
isPleasant :: (Monad m, Padeable e) => Detector m e -> Double -> Detector m Bool
isPleasant detector eps = do (a,_,_) <- toPAD <$> detector
                             return (a > eps)

ghci> runDetector (filterM (isPleasant detector) [0.3,0.6,0.9]) "path/to/image"
[0.3,0.6]
```


### Example

Moe comes with some detectors implemented by default (see [Data.Moe.Detector](Data/Moe/Detector)), such as Face++ (see [Data.Moe.Detector.FaceApp](Data/Moe/Detector/FaceApp.hs)). Face++ analyzes and identifies emotion of detected faces, including confidence scores for several kinds of emotions. This module exports a datatype for representing results of Face++:

```haskell
data FaceAppData = FaceAppData {
    happiness :: Double,
    disgust :: Double,
    sadness :: Double,
    surprise :: Double,
    fear :: Double,
    anger :: Double
} deriving (Generic, Show)
```

and three functions:

```haskell
faceapp :: Detector IO FaceAppData
faceappWith :: (String,String) -> Detector IO FaceAppData
faceappLogin :: IO (String,String)
```

`faceapp` is a detector that gets the `api_key` and `api_secret` from the standard input and analyzes a given image using the Face++'s API. `faceappWith` is like `faceapp`, but takes the `api_key` and `api_secret` as arguments.

```haskell
ghci> runDetector faceapp "path/to/image"
Enter api key:
********************************
Enter api secret:
********************************
FaceAppData {happiness = 99.935, disgust = 1.0e-3, sadness = 1.0e-3, surprise = 1.0e-3, fear = 0.0, anger = 0.0}
```
