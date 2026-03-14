module Lib (
    colorIntensity,
    colorIlluminance,
    localToGlobal,
    normalVec,
    middleVec,
    brdf,
    calcBrightness,
) where

import Brightness
import Illuminance
import Intensity
import LightSource
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
    s = pL - pT
    cosA = s `dot` n / norm s
    rSqr = norm s ** 2

localToGlobal :: Double -> Double -> Point -> Point -> Point -> Point
localToGlobal x y p0 p1 p2 = p0 + (normalize (p1 - p0) `Point.mul` x + normalize (p2 - p0) `Point.mul` y)

normalVec :: Point -> Point -> Point -> Point
normalVec p0 p1 p2 = normalize $ (p2 - p0) * (p1 - p0)

middleVec :: Point -> Point -> Point
middleVec v s = normalize $ v + s

brdf :: RGB -> Point -> Point -> Point -> Double -> Double -> Double -> RGB
brdf rgb n v s kD kS kE = mulVal rgb k
  where
    h = middleVec v s
    k = kD + kS * (h `dot` n) ** kE

calcBrightness :: [LightSource] -> Point -> Point -> Point -> RGB -> Double -> Double -> Double -> Brightness
calcBrightness ls pT n v kRGB kD kS kE = Brightness $ foldl helper (RGB 0 0 0) ls `mulVal` (1 / pi)
  where
    helper rgb ls' =
        let
            (LightSource _ pL _) = ls'
            s = pT - pL
            (Illuminance eRgb) = colorIlluminance ls' pT n
         in
            rgb `add` (eRgb `RGB.mul` brdf kRGB n v s kD kS kE)
