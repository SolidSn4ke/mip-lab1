{-# LANGUAGE DeriveGeneric #-}

module LightSource (LightSource (..)) where

import Data.Aeson
import GHC.Generics
import Intensity (Intensity)
import Point (Point)

data LightSource = LightSource
    { i0 :: Intensity,
      p :: Point,
      o :: Point
    }
    deriving (Show, Generic)

instance FromJSON LightSource
