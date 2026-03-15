{-# LANGUAGE DeriveGeneric #-}

module Intensity (Intensity (..)) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)
import RGB

newtype Intensity = Intensity
    { rgb :: RGB
    }
    deriving (Eq, Generic)

instance Show Intensity where
    show (Intensity (RGB r1 g1 b1)) = show $ (,,) r1 g1 b1

instance FromJSON Intensity