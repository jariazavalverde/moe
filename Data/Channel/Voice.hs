{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Channel.Voice(
    Voice(..)
) where

newtype Voice a = Voice { getMediaVoice :: a }
    deriving (Read, Show, Eq, Ord)