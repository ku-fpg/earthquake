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

runtime :: forall model m .
	   (Show model, Widget model, Update model m)
        => (forall a b . Monoid a => m a b -> (a,b))
	-> m (Cmd (Msg model)) model
	-> Application -> Application
runtime run m = start $ jsbRuntime run m

jsbRuntime :: forall model m .
           (Show model, Widget model, Update model m)
        => (forall a b . Monoid a => m a b -> (a,b))
	-> m (Cmd (Msg model)) model
        -> Engine -> IO ()
jsbRuntime run m e = do
  print "elmArch"
  theResp <- newTChanIO
  let render :: Widget model 
      	     => model
	     -> Int
             -> IO ()
      render theModel theTick = do
        let theView = view theModel
        let s0 = 0
        let (json,_) = runState (sendRemote theView) 0
        print ("json",json)
        sendA e $ command $ call "jsb.render" [value (0::Int),value json]
        wait theModel theTick theView

      wait :: Widget model
      	   => model
	   -> Int 
           -> Remote (Msg model)
	   -> IO ()
      wait theModel theTick theView = do 
        msg <- listen e
        print "waiting for event"
        print msg
        case fromJSON msg :: Result ResponseEvent of
          Error msg -> do
            print("Error fromJSON msg",msg)
	    wait theModel theTick theView
          Success msg' -> do
            print ("got msg",msg')
            case evalState (recvRemote theView msg') 0 of
              Nothing -> do
                print "no match found for event"
                wait theModel theTick theView
              Just theMsg -> act theTick $ update theMsg theModel

      act :: Widget model 
      	  => Int
	  -> m (Cmd (Msg model)) model
	  -> IO ()
      act theTick modelAct = do 
      	let (cmds,theModel) = run modelAct
	() <- spawnCmd theResp cmds
        render theModel $ theTick + 1

--  let (cmds,m') = run m
--  () <- spawnCmd ch cmds
  act 0 m 


------------------------------------------------------------------------------
-- Primitive widgets


