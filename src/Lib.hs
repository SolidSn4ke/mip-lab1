module Lib (
    colorIntensity,
    colorIlluminance,
    localToGlobal,
    normalVec,
) where

import Data.Function ((&))
import Illuminance (Illuminance (..))
import Intensity
import Point (Point (..), mul, normalize)
import RGB

colorIntensity :: Intensity -> Double -> Intensity
colorIntensity (Intensity rgb) angle = Intensity $ RGB (r * cosT) (g * cosT) (b * cosT)
  where
    r = getR rgb
    g = getG rgb
    b = getB rgb
    cosT = cos angle & max 0

colorIlluminance :: Intensity -> Double -> Double -> Double -> Illuminance
colorIlluminance i0 theta alpha dist = Illuminance $ RGB r' g' b'
  where
    Intensity (RGB r g b) = colorIntensity i0 theta
    r' = r * cos alpha / dist ** 2
    g' = g * cos alpha / dist ** 2
    b' = b * cos alpha / dist ** 2

localToGlobal :: Double -> Double -> Point -> Point -> Point -> Point
localToGlobal x y p0 p1 p2 = p0 + (normalize (p1 - p0) `mul` x + normalize (p2 - p0) `mul` y)

normalVec :: Point -> Point -> Point -> Point
normalVec p0 p1 p2 = normalize $ (p2 - p0) * (p1 - p0)
