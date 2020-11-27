{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.Channel.Face(
    Face(..)
) where

newtype Face a = Face { getFaceChannel :: a }
    deriving (Functor, Read, Show, Eq, Ord)