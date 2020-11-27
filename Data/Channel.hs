{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Channel(
    (:+:)(..),
    (:<:)(..)
) where

data (f :+: g) = f :+: g deriving (Show)
infixr 1 :+:

class a :<: b where
    channel :: b -> a

instance {-# OVERLAPPING #-} a :<: a where
    channel = id

instance {-# OVERLAPPING #-} a :<: (a :+: b) where
    channel (x :+: _) = channel x

instance {-# OVERLAPPING #-} (a :<: b) => a :<: (c :+: b) where
    channel (_ :+: y) = channel y