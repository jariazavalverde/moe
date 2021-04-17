{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Channel.Voice(
    Voice(..)
) where

newtype Voice a = Voice { getVoiceChannel :: a }
    deriving (Functor, Read, Show, Eq, Ord)