{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Earthquake.Widget.Slider where

import Network.Earthquake.Remote
import Network.Earthquake.Widget
import Data.Text(Text)

data Slider = Slider
   { sliderMin :: Double
   , sliderMax :: Double
   , sliderValue :: Double
   , sliderStep  :: Double
   }

data SliderMsg 
  = SliderValue Double

--instance Widget Slider where
--  type Msg Slider = SliderMsg

