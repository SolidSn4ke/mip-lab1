module RGB (RGB (..), add, mul, mulVal) where

data RGB = RGB Double Double Double deriving (Eq)

add :: RGB -> RGB -> RGB
add (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

mul :: RGB -> RGB -> RGB
mul (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)

mulVal :: RGB -> Double -> RGB
mulVal (RGB r g b) value = RGB (r * value) (g * value) (b * value)
