{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}

module Network.Earthquake.Widget where

import Data.Aeson                  (ToJSON)
import Data.Bifunctor
import Data.Biapplicative
import Data.Text(Text, pack)
import GHC.Exts (Constraint)

import Network.Earthquake.Remote
import Network.Earthquake.Cmd

class Widget model where
  type Msg model
  view :: model  -> Remote (Msg model)

self :: (Applicative f, Widget model, model ~ Msg model)  => Msg model -> model -> f model
self msg _ = pure msg

class Widget model => ApplicativeUpdate model where
  updateA :: Applicative f
  	  => Msg model 
          -> model 
          -> f model

class Widget model => BifunctorUpdate model where
  updateB :: Bifunctor f
  	  => Msg model 
          -> model
          -> f (Cmd (Msg model)) model
 
instance (Widget model) => Widget [model] where
  type Msg [model] = OneOf (Msg model)
  view = arrayOf . map view

updateOneOf :: OneOf model -> [model] -> [model]
updateOneOf (OneOf n w) ws = take n ws ++ [w] ++ drop (n+1) ws

data OneOf a = OneOf Int a
  deriving Show

arrayOf :: [Remote msg] -> Remote (OneOf msg)
arrayOf rs = array
  [ OneOf i <$> r
  | (r,i) <- rs `zip` [0..]
  ]

instance ApplicativeUpdate model => ApplicativeUpdate [model] where
  updateA (OneOf n w) xs = fmap
    (\ x -> updateOneOf (OneOf n x) xs)
    (updateA w (xs !! n))

instance BifunctorUpdate model => BifunctorUpdate [model] where
  updateB (OneOf n w) xs = bimap
    (OneOf n <$>)
    (\ x -> updateOneOf (OneOf n x) xs)
    (updateB w (xs !! n))

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

