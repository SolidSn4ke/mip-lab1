module Point (Point (..), mul, norm, normalize) where

import Data.Function ((&))

data Point = Point Double Double Double deriving (Eq)

instance Show Point where
    show (Point x y z) = show $ (,,) x y z

instance Num Point where
    (+) (Point x1 y1 z1) (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)

    (-) p1 p2 = p1 + negate p2

    (*) (Point x1 y1 z1) (Point x2 y2 z2) = Point (y1 * z2 - z1 * y2) (x1 * z2 - z1 * x2) (x1 * y2 - y1 * x2)

    negate (Point x y z) = Point (negate x) (negate y) (negate z)

    abs = error ""

    signum = error ""

    fromInteger x = Point (fromIntegral x) (fromIntegral x) (fromIntegral x)

mul :: Point -> Double -> Point
mul (Point x y z) a = Point (x * a) (y * a) (z * a)

norm :: Point -> Double
norm (Point x y z) = sqrt $ x ** 2 + y ** 2 + z ** 2

normalize :: Point -> Point
normalize p = norm p ** (-1) & mul p
