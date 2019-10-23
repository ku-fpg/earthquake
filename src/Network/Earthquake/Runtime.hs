{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Earthquake.Runtime where

import Control.Monad.Trans.State   (runState,evalState)
import Data.Aeson                  (Result(..),fromJSON)

import Network.JavaScript          (sendA, command, call, value, start, Application, listen)


import Network.Earthquake.Cmd
import Network.Earthquake.Remote
import Network.Earthquake.Widget

data RuntimeState model = RuntimeState
  { theModel :: model
  , theTick  :: Int
  }

runtime :: forall model m .
           (Show model, Widget model, Update model m)
        => (forall a b . m a b -> (a,b))
        -> model
        -> Application -> Application
runtime run m = start $ \ e -> do
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
	        let (_,theModel') = run (update theMsg theModel) in
                render $ RuntimeState { theModel = theModel'
                                      , theTick = theTick + 1
                                      }
  render $ RuntimeState { theModel = m
                        , theTick = 0
                        }


------------------------------------------------------------------------------
-- Primitive widgets


