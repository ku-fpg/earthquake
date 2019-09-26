{-# OPTIONS_GHC -w #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
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
  module Network.Earthquake.Remote) where

import Control.Monad.Trans.State   (State,put,get,runState,evalState,execState)
import Control.Applicative         ((<|>))
import Data.Aeson                  (Value,ToJSON,toJSON,FromJSON(..),withObject,(.:), Result(..),fromJSON,(.=))
import Data.Maybe
import qualified Data.Aeson as A

import Control.Monad.Trans.Writer  (Writer,runWriter,tell, mapWriter)

import Network.JavaScript          (sendA, command, call, value, start, Application, listen)
import Data.Text(Text, pack)

import Network.Earthquake.Remote
import Network.Earthquake.Cmd
import Network.Earthquake.Widget


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
        let theView = view @model theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print ("json",json)
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: RuntimeState model
           -> Remote (Msg model)
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
              Just theMsg -> 
	        let (_,theModel') = update theMsg theModel in
                render $ RuntimeState { theModel = theModel'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theTick = 0
                        }


------------------------------------------------------------------------------
-- Primitive widgets


