module Lib (
    colorIntensity,
    colorIlluminance,
    localToGlobal,
    normalVec,
) where

import Illuminance (Illuminance (..))
import Intensity
import LightSource (LightSource (..))
import Point (Point (..), dot, mul, norm, normalize)
import RGB

colorIntensity :: LightSource -> Point -> Intensity
colorIntensity (LightSource i0 pL axis) pT = Intensity $ RGB (r * cosT) (g * cosT) (b * cosT)
  where
    Intensity (RGB r g b) = i0
    s = pT - pL
    cosT = s `dot` axis / norm s

colorIlluminance :: LightSource -> Point -> Point -> Illuminance
colorIlluminance ls pT n = Illuminance $ RGB (r * cosA / rSqr) (g * cosA / rSqr) (b * cosA / rSqr)
  where
    LightSource _ pL _ = ls
    Intensity (RGB r g b) = colorIntensity ls pT
    s = pT - pL
    cosA = s `dot` n / norm s
    rSqr = norm s ** 2

localToGlobal :: Double -> Double -> Point -> Point -> Point -> Point
localToGlobal x y p0 p1 p2 = p0 + (normalize (p1 - p0) `mul` x + normalize (p2 - p0) `mul` y)

normalVec :: Point -> Point -> Point -> Point
normalVec p0 p1 p2 = normalize $ (p2 - p0) * (p1 - p0)
