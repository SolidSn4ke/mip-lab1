module Lib (
    colorIntensity,
    colorIlluminance,
    localToGlobal,
    normalVec,
    middleVec,
    brdf,
    calcBrightness,
    calc,
) where

import Brightness
import Illuminance
import Intensity
import LightSource
import Point (Point (..), dot, mul, norm, normalize)
import RGB
import Scene

colorIntensity :: LightSource -> Point -> Intensity
colorIntensity (LightSource i0' pL axis) pT = Intensity $ mulVal rgb' cosT
  where
    Intensity rgb' = i0'
    s = pT - pL
    cosT = s `dot` axis / norm s

colorIlluminance :: LightSource -> Point -> Point -> Illuminance
colorIlluminance ls pT n = Illuminance $ mulVal rgb' $ cosA / rSqr
  where
    LightSource _ pL _ = ls
    Intensity rgb' = colorIntensity ls pT
    s = pL - pT
    cosA = s `dot` n / norm s
    rSqr = norm s ** 2

localToGlobal :: Double -> Double -> Point -> Point -> Point -> Point
localToGlobal x' y' p_0 p_1 p_2 = p_0 + (normalize (p_1 - p_0) `Point.mul` x' + normalize (p_2 - p_0) `Point.mul` y')

normalVec :: Point -> Point -> Point -> Point
normalVec p_0 p_1 p_2 = normalize $ (p_2 - p_0) * (p_1 - p_0)

middleVec :: Point -> Point -> Point
middleVec v s = normalize $ v + s

brdf :: RGB -> Point -> Point -> Point -> Double -> Double -> Double -> RGB
brdf rgb' n v s kD kS kE = mulVal rgb' k'
  where
    h = middleVec v s
    k' = kD + kS * (h `dot` n) ** kE

calcBrightness :: [LightSource] -> Point -> Point -> Point -> RGB -> Double -> Double -> Double -> Brightness
calcBrightness ls pT n v kRGB kD kS kE = Brightness $ foldl helper (RGB 0 0 0) ls `mulVal` (1 / pi)
  where
    helper rgb' ls' =
        let
            (LightSource _ pL _) = ls'
            s = pT - pL
            (Illuminance eRgb) = colorIlluminance ls' pT n
         in
            rgb' `add` (eRgb `RGB.mul` brdf kRGB n v s kD kS kE)

calc :: [LightSource] -> Triangle -> Point -> Surface -> (Double, Double) -> Brightness
calc ls t v s (x', y') = calcBrightness ls pT n v kRGB kD kS kE
  where
    Triangle p_0 p_1 p_2 = t
    Surface kRGB kD kS kE = s
    pT = localToGlobal x' y' p_0 p_1 p_2
    n = normalVec p_0 p_1 p_2