# Moe

## A monadic multimodal emotion recognition framework to detect and combine emotions in Haskell

### Monadic detectors

We need a mechanism for passing a fixed input to several detectors, so we model a detector as a **reader monad**, that is, a computation which can read values from a shared environment.

```haskell
data DetectorT r m a = DetectorT {
    runDetectorT :: r -> m a
}
```

The `DetectorT` type constructor is parameterised with three types: the type `r` of the input that the detector takes to analyze emotions (the shared environment, ie., the channels), an underlaying monad `m` for describing the effects of the detector, and the type `a` of the output produced by the detector. We are interested in collecting the results of external emotion recognition services, so the detectors will usually require the `IO` monad to sending HTTP requests.

### Channels

We model a channel as a data type that acts as a container for the information to be analyzed by detectors. If we want to add new channels to our detectors, we just have to define a new data type.

```haskell
newtype Face a = Face { getFaceChannel :: a }
newtype Voice a = Voice { getVoiceChannel :: a }
```

### Emotions

In order to work with generic combinators that are useful for all detectors regardless of their API and the results they return, we define a common interface to extract information about emotions.

#### PAD

One of the approaches used to represent emotions is the **dimensional approach**. According to this representation form, an emotion can be expressed as a point in a space of several dimensions, usually a 3D one. In this space, each axis represents an aspect of an emotion. The **PAD** format is one of the most common dimensional approaches, in which an emotion can be described with three coordinates. The first coordinate (**pleasure**, also called valence) states how positive or negative an emotion is; the second coordinate states the level of **arousal** linked to that emotion; finally, the last coordinate states how **dominant** or passive the person expressing that emotion feels. Each coordinate is usually expressed with a number between `-1` and `1`.

```haskell
data PAD = PAD {
    pleasure :: Double,
    arousal :: Double,
    dominance :: Double
} deriving (Eq, Show, Read)
```

#### Ekman

As a more comprehensive system to represent emotions, **categorical models** were proposed. According to this approach, an emotion is represented using a label from a closed set. In practice, these labels are usually paired with a number between `0` and `1` which expresses how confident the detector is about the presence of an emotion in piece of media. One of the most common set of labels used is the set of six basic emotions proposed by Ekman, according to whom there are six universally recognised emotions, which are happiness, sadness, disgust, anger, surprise and fear. Detectors usually include a seventh label, neutral, to indicate the absence of the other six emotions.

```haskell
data Ekman = Ekman {
    anger :: Double,
    disgust :: Double,
    fear :: Double,
    joy :: Double,
    sadness :: Double,
    surprise :: Double,
    neutral :: Double
} deriving (Eq, Show, Read)
```

#### Emotion typeclass

We define the `Emotion` typeclass to generalize data types `e` that represent emotions, using three functions: `toPAD`, `toEkman` and `linearMap`. The first two functions convert a value of type `e` into a `PAD` or `Ekman` data, respectively. The `linearMap` function returns the linear mapping `M = [anger, disgust, fear, joy, sadness, surprise, neutral]` to be applied, if necessary.

### Multimodal detectors

We define a right-associative infix operator type (`:+:`) similar to built-in tuples (`,`) in order to create inputs with more than one channel.

```haskell
data (f :+: g) = f :+: g deriving (Show)
infixr 1 :+:
```

Also we define a (`:<:`) multi-parameter typeclass with an only method, `channel`, that allows us to inject a value from type (`:+:`) to another smaller type.

```haskell
class a :<: b where
    channel :: b -> a

instance {-# OVERLAPPING #-} a :<: a where
    channel = id

instance {-# OVERLAPPING #-} a :<: (a :+: b) where
    channel (x :+: _) = channel x

instance {-# OVERLAPPING #-} (a :<: b) => a :<: (c :+: b) where
    channel (_ :+: y) = channel y
```

Instead of defining a detector on a specific channel (such as `Face String`), we will define a detector on a generic channel `r` that contains the channel.

```haskell
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
```

### Examples

#### Functor

```haskell
isHappy  :: (Functor f, Emotion e) => f e -> f Bool
isHappy = fmap ((> 0.5) . joy . toEkman)

ghci> runDetectorT (isHappy (faceapp ("****","****"))) (Face "path/to/img")
True
```

#### Applicative

```haskell
ghci> let d1 = faceapp ("****","****")
ghci> let d2 = voiceapp ("****","****") (0,5)
ghci> let d3 = (&&) <$> isHappy d1 <*> isHappy d2
ghci> runDetectorT d3 (Face "..." :+: Voice "...")
True
```

#### Monad

```haskell
ghci> runDetectorT (filterM (isHappy . voiceapp "****") [(0,5), (5,10), (10,15)]) (Voice "happy.mp4")
[(0,5), (10,15)]
```

#### Monad transformer

```haskell
iod :: (Face String :<: r, Voice String :<: r) => DetectorT r IO PAD
iod = do api_key <- lift getLine
         api_secret <- lift getLine
         face <- faceapp (api_key,api_secret)
         api_key' <- lift getLine
         voice <- voiceapp api_key'
         let mean = (toPAD face + toPAD voice) / 2
         return mean
```

```haskell
dw :: (Face String :<: r, Voice String :<: r) => WriterT [String] (DetectorT r IO) PAD
dw = do face <- lift $ faceapp ("****","****")
        tell ["face: " ++ show face]
        voice <- lift $ voiceapp "****"
        tell ["voice: " ++ show voice]
        let mean = (toPAD face + toPAD voice) / 2
        tell ["mean: " ++ show mean]
        return mean
```