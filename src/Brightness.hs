module Brightness (Brightness (..)) where

import RGB (RGB (..))

newtype Brightness = Brightness RGB

instance Show Brightness where
    show (Brightness (RGB r' g' b')) = show $ (,,) r' g' b'
