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
  module Network.JavaScript.Remote) where

import Control.Monad.Trans.State   (State,put,get,runState,evalState,execState)
import Control.Applicative         ((<|>))
import Data.Aeson                  (Value,ToJSON,toJSON,FromJSON(..),withObject,(.:), Result(..),fromJSON,(.=))
import Data.Maybe
import qualified Data.Aeson as A

import Control.Monad.Trans.Writer  (Writer,runWriter,tell, mapWriter)

import Network.JavaScript          (sendA, command, call, value, start, Application, listen)
import Data.Text(Text, pack)

import Network.JavaScript.Remote

data Cmd model where
  Promise :: IO (Msg model) -> Cmd model
  Ticker :: Int -> IO (Msg model) -> Cmd model
  MconcatCmd :: [Cmd model] -> Cmd model
  FmapCmd :: (a -> b) -> Cmd a -> Cmd b

instance Functor Cmd where 
  fmap = FmapCmd

instance Semigroup (Cmd msg) where 
  (<>) = mappend

instance Monoid (Cmd msg) where 
  mempty = mconcat []
  mappend a b = mconcat [a,b]
  mconcat = MconcatCmd

class Widget model where
  type Msg model
  view :: (msg ~ Msg model) => model -> Remote msg
  update :: (msg ~ Msg model) => msg -> model -> (Cmd msg,model)
--  update :: (msg ~ Msg model) => (Msg model) -> model -> (Cmd (Msg model),model)
  default update :: (msg ~ model, msg ~ Msg model) => msg -> model -> (Cmd msg,model)
  -- We default to the message being the new model.
  update msg _ = pure msg

-- can use pure for update, which is nice

instance Widget model => Widget [model] where
  type Msg [model] = OneOf (Msg model)
  view = arrayOf . map view
  update (OneOf n w) xs = (OneOf n <$> c, take n xs ++ [x] ++ drop (n+1) xs)
    where (c,x) = update w (xs !! n)

updateOneOf :: OneOf model -> [model] -> [model]
updateOneOf (OneOf n w) ws = take n ws ++ [w] ++ drop (n+1) ws

data OneOf a = OneOf Int a
  deriving Show

arrayOf :: [Remote msg] -> Remote (OneOf msg)
arrayOf rs = array
  [ OneOf i <$> r
  | (r,i) <- rs `zip` [0..]
  ]

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

tag :: String -> Remote msg
tag = send . pack

primitive :: (ToJSON m, ToResponse m) => String -> m -> Remote m
primitive txt n = object 
      [ ("type"  , tag txt)  -- type gives the type name
      , ("value" , send n)   -- the outgoing value
      , ("event" , recv)     -- the event reply
      ]  


-- Here are the base instances. 
-- We do not default view to this pattern, 
-- because it would complicate the API for Widget to save 4 lines of 
-- (internal) code.

instance Widget Double where
  type Msg Double = Double
  view = primitive $ show $ witness @Double

instance Widget Text where
  type Msg Text = Text
  view = primitive $ show $ witness @Text

instance Widget Bool where
  type Msg Bool = Bool
  view = primitive $ show $ witness @Bool

instance Widget () where
  type Msg () = ()
  view = primitive $ show $ witness @()

