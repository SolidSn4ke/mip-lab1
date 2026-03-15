{-# LANGUAGE DeriveGeneric #-}

module LightSource (LightSource (..)) where

import Data.Aeson
import GHC.Generics
import Intensity (Intensity)
import Point (Point)

data LightSource = LightSource
    { i0 :: Intensity,
      o :: Point,
      p :: Point
    }
    deriving (Show, Generic)

instance FromJSON LightSource
