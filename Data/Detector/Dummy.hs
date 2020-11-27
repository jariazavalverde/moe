{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Detector.Dummy(
    dummy
) where

import Data.Detector(DetectorT, mkDetectorT)
import Data.Channel((:<:)(..))

dummy :: () :<: r => DetectorT r IO ()
dummy = mkDetectorT (\_ -> return ())