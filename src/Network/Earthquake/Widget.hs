{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Earthquake.Widget where

import Data.Aeson                  (ToJSON)
import Data.Bifunctor
import Data.Text(Text, pack)
import GHC.Exts (Constraint)

import Network.Earthquake.Remote
import Network.Earthquake.Cmd

class Widget model where
  type Msg model
  
  type Update model (m :: * -> * -> *) :: Constraint
  type Update model m = Applicative (m (Cmd (Msg model)))
  
  view           :: model 
                 -> Remote (Msg model)
  update         :: Update model m
                 => Msg model 
                 -> model 
                 -> m (Cmd (Msg model)) model
  default update :: (model ~ Msg model, Applicative (m (Cmd (Msg model))))
                 => Msg model 
                 -> model 
                 -> m (Cmd (Msg model)) model

  -- We default to the message being the new model,
  -- for when the message and the model have the same type.
  update msg _ = pure msg

instance (Widget model) => Widget [model] where
  type Msg [model] = OneOf (Msg model)
  type Update [model] m = (Update model m, Bifunctor m)
  view = arrayOf . map view
  update (OneOf n w) xs = bimap
    (OneOf n <$>)
    (\ x -> take n xs ++ [x] ++ drop (n+1) xs)
    (update w (xs !! n))

updateOneOf :: OneOf model -> [model] -> [model]
updateOneOf (OneOf n w) ws = take n ws ++ [w] ++ drop (n+1) ws

data OneOf a = OneOf Int a
  deriving Show

arrayOf :: [Remote msg] -> Remote (OneOf msg)
arrayOf rs = array
  [ OneOf i <$> r
  | (r,i) <- rs `zip` [0..]
  ]

-- Here are the base instances. We do not default view to this pattern, 
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

tag :: String -> Remote msg
tag = send . pack

primitive :: (ToJSON m, ToResponse m) => String -> m -> Remote m
primitive txt n = object 
      [ ("type"  , tag txt)  -- type gives the type name
      , ("value" , send n)   -- the outgoing value
      , ("event" , recv)     -- the event reply
      ]  

