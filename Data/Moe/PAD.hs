{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Moe.PAD(
    PAD,
    Padeable(..),
    normalize,
    aggregate
) where

type PAD = (Double, Double, Double)

class Padeable e where
    toPAD :: e -> PAD

instance Padeable String where
    toPAD = read

normalize :: Double -> Double -> PAD -> PAD
normalize min max (a,b,c) =  let [x,y,z] = map (\x -> (x-min)/(max-min)) [a,b,c] in (x,y,z)

aggregate :: Foldable t => t PAD -> PAD
aggregate xs = let (x,y,z) = foldl1 (\(as,bs,cs) (a,b,c) -> (as+a,bs+b,cs+c)) xs
                   len = fromIntegral $ length xs
               in (x/len, y/len, z/len)