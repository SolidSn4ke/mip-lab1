module RGB (RGB (..), getR, getG, getB) where

data RGB = RGB Double Double Double deriving (Eq)

getR :: RGB -> Double
getR (RGB r _ _) = r

getG :: RGB -> Double
getG (RGB _ g _) = g

getB :: RGB -> Double
getB (RGB _ _ b) = b
