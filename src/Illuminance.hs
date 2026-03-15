module Illuminance (Illuminance (..)) where

import RGB (RGB (..))

newtype Illuminance = Illuminance RGB deriving (Eq)

instance Show Illuminance where
    show (Illuminance (RGB r' g' b')) = show $ (,,) r' g' b'
