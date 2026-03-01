module Intensity (Intensity (..)) where

import RGB

newtype Intensity = Intensity RGB deriving (Eq)

instance Show Intensity where
    show (Intensity (RGB r g b)) = show $ (,,) r g b
