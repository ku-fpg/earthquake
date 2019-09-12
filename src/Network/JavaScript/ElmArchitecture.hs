{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Network.JavaScript.ElmArchitecture (
  module Network.JavaScript.ElmArchitecture,
  module Network.JavaScript.Remote) where

import Control.Monad.Trans.State   (State,put,get,runState,evalState,execState)
import Control.Applicative         ((<|>))
import Data.Aeson                  (Value,ToJSON,toJSON,FromJSON(..),withObject,(.:), Result(..),fromJSON,(.=))
import Data.Maybe
import qualified Data.Aeson as A

import Control.Monad.Trans.Writer  (Writer,runWriter,tell, mapWriter)

import Network.JavaScript          (sendA, command, call, value, start, Application, listen)
import Data.Text(Text)

import Network.JavaScript.Remote

-- a widget is a combination of a model transformer, with a Remote (view) effect.
class Widget model where
  widget :: model -> Remote model

-- We provide the more general tag-based message, as well as the
-- more uniform model to model version.
widgetOneOf :: Widget model => [model] -> Remote (OneOf model)
widgetOneOf = arrayOf . map widget

instance Widget model => Widget [model] where
  widget ws = flip updateOneOf ws <$> widgetOneOf ws

updateOneOf :: OneOf model -> [model] -> [model]
updateOneOf (OneOf n w) ws = take n ws ++ [w] ++ drop (n+1) ws

-- An Applet take a model, and return a model, perhaps using
-- IO to generate a event that itself returns Applets.
-- newtype Applet model = Applet (Writer (IO (Event (model -> Applet model))) model)

data OneOf a = OneOf Int a
  deriving Show

arrayOf :: [Remote msg] -> Remote (OneOf msg)
arrayOf rs = array
  [ OneOf i <$> r
  | (r,i) <- rs `zip` [0..]
  ]




------------------------------------------------------------------------------


------------------------------------------------------------------------------
data RuntimeState model = RuntimeState
  { theModel :: model
  , theTick  :: Int
  }

elmArchitecture :: forall model .
                   (Show model, Widget model)
                => model
                -> Application -> Application
elmArchitecture  m = start $ \ e -> do
  print "elmArch"
  let render :: RuntimeState model
             -> IO ()
      render state@RuntimeState{..} = do
        print theModel
        let theView = widget @model theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print ("json",json)
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: RuntimeState model
           -> Remote model
	   -> IO ()
      wait state@RuntimeState{..} theView = do
        msg <- listen e
        print "waiting for event"
        print msg
        case fromJSON msg :: Result ResponseEvent of
          Error msg -> do
            print("Error fromJSON msg",msg)
            wait state theView
          Success msg' -> do
            print ("got msg",msg')
            case evalState (recvRemote theView msg') 0 of
              Nothing -> do
                print "no match found for event"
                wait state theView
              Just theModel' -> 
                render $ RuntimeState { theModel = theModel'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theTick = 0
                        }


------------------------------------------------------------------------------
-- Primitive widgets

tag :: Text -> Remote msg
tag txt = send txt

primitive :: (ToJSON m, ToResponse m) => Text -> m -> Remote m
primitive txt n = object 
      [ ("type"  , tag txt)  -- by convension
      , ("value" , send n)   -- the outgoing value
      , ("event" , recv)     -- the event reply
      ]  

instance Widget Double where
  widget = primitive "Double"

instance Widget Text where
  widget = primitive "Text"

instance Widget Bool where
  widget = primitive "Bool"  


