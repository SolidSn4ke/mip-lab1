{-# LANGUAGE DeriveGeneric #-}

module RGB (RGB (..), add, mul, mulVal) where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data RGB = RGB
    { r :: Double,
      g :: Double,
      b :: Double
    }
    deriving (Show, Eq, Generic)

instance FromJSON RGB

add :: RGB -> RGB -> RGB
add (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 + r2) (g1 + g2) (b1 + b2)

mul :: RGB -> RGB -> RGB
mul (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1 * r2) (g1 * g2) (b1 * b2)

mulVal :: RGB -> Double -> RGB
mulVal (RGB r1 g1 b1) value = RGB (r1 * value) (g1 * value) (b1 * value)
