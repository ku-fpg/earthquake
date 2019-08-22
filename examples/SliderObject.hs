{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty

import Network.JavaScript.ElmArchitecture
import Network.JavaScript.Widgets(Slider(..))

import Paths_earthquake

main :: IO ()
main = main_ 3000

data Sliders = Sliders
 { s1 :: Slider
 , s2 :: Slider
 , r  :: Double
 }
             
instance Widget Sliders Sliders where
  widget s@(Sliders s1 s2 r) = object
      [ "sliders" := update <$> widget [s1,s2]
      , "reset"   := wait (reset s)
      ]
    where update s@(Sliders (Slider s1) (Slider s2) _) = s { r = s1 + s2 }

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Sliders.html"
    middleware $ elmArchitecture $
      map Slider [0,10..50]
