{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Detector.Dummy(
    dummy
) where

import Data.Detector(DetectorT, mkDetectorT)
import Data.Channel((:<:)(..))

dummy :: (() :<: r, Monad m) => DetectorT r m ()
dummy = mkDetectorT (\_ -> return ())