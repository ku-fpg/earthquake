{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Network.Earthquake.Runtime where

import Control.Concurrent.STM
import Control.Monad.Trans.State   (runState,evalState)
import Data.Aeson                  (Result(..),fromJSON)

import Network.JavaScript          (sendA, command, call, value, start, Application, listen, Engine)



import Network.Earthquake.Cmd
import Network.Earthquake.Remote
import Network.Earthquake.Widget

data RuntimeState model = RuntimeState
  { theModel :: !model
  , theTick  :: !Int
  , theResp  :: !(TChan (Msg model))
  }

runtime :: forall model m .
	   (Show model, Widget model, Update model m)
        => (forall a b . Monoid a => m a b -> (a,b))
	-> model
	-> Application -> Application
runtime run m = start $ jsbRuntime run m

jsbRuntime :: forall model m .
           (Show model, Widget model, Update model m)
        => (forall a b . Monoid a => m a b -> (a,b))
        -> model
        -> Engine -> IO ()
jsbRuntime run m e = do
  print "elmArch"
  let render :: Widget model 
      	     => RuntimeState model
             -> IO ()
      render state@RuntimeState{..} = do
        let theView = view theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print ("json",json)
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait state theView

      wait :: Widget model
      	   => RuntimeState model
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
              Just theMsg -> do
	        let (cmds,theModel') = run $ update theMsg theModel
		() <- spawnCmd theResp cmds
                render $ state { theModel = theModel'
                               , theTick = theTick + 1
                               }
  ch <- newTChanIO
  render $ RuntimeState { theModel = m
                        , theTick = 0
			, theResp = ch
                        }


------------------------------------------------------------------------------
-- Primitive widgets


