{-# LANGUAGE DeriveGeneric #-}

module Scene (Scene (..), Triangle (..), Surface (..)) where

import Data.Aeson
import GHC.Generics
import LightSource
import Point
import RGB

data Scene = Scene
    { lights :: [LightSource],
      triangle :: Triangle,
      viewer :: Point,
      surface :: Surface,
      points :: [(Double, Double)]
    }
    deriving (Show, Generic)

instance FromJSON Scene

data Triangle = Triangle
    { p0 :: Point,
      p1 :: Point,
      p2 :: Point
    }
    deriving (Show, Generic)

instance FromJSON Triangle

data Surface = Surface
    { k :: RGB,
      kd :: Double,
      ks :: Double,
      ke :: Double
    }
    deriving (Show, Generic)

instance FromJSON Surface