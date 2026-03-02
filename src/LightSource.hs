module LightSource (LightSource (..)) where

import Intensity (Intensity)
import Point (Point)

data LightSource = LightSource Intensity Point Point
