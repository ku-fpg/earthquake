{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Network.JavaScript.Widgets where

import Network.JavaScript.ElmArchitecture
import Network.Earthquake.Widget

newtype Slider = Slider Double
  deriving (Eq, Ord, Show)

instance Widget Slider where
  type Msg Slider = Slider
  view (Slider n) = Slider <$> view n

