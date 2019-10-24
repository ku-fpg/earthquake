{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Earthquake.Widget.Button where

import Network.Earthquake.Remote
import Network.Earthquake.Widget
import Data.Text(Text)

data Button = Button Text Bool

data ButtonMsg 
  = NewText Text
  | Enable Bool
  | Click

instance Widget Button where
  type Msg Button = ButtonMsg
  view (Button txt en) = object
      [ ("type"   , tag "button")
      , ("text"   , send txt) 
      , ("enabled", send en) 
      , ("click"  , (\ () -> Click) <$> recv)
      ]  
  update (NewText txt) (Button _ en)  = pure $ Button txt en
  update (Enable en)   (Button txt _) = pure $ Button txt en
  update Click m = pure m
