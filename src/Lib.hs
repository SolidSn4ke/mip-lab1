module Lib (
    colorIntensity,
) where

import Data.Function ((&))
import Intensity
import RGB

colorIntensity :: Intensity -> Double -> Intensity
colorIntensity (Intensity rgb) angle = Intensity $ RGB (r * cosT) (g * cosT) (b * cosT)
  where
    r = getR rgb
    g = getG rgb
    b = getB rgb
    cosT = cos angle & max 0
