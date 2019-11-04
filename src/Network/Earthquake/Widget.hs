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

class Widget model => BiapplicativeUpdate model where
  updateB :: Biapplicative f
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

instance BiapplicativeUpdate model => BiapplicativeUpdate [model] where
  updateB (OneOf n w) xs = bimap
    (OneOf n <$>)
    (\ x -> updateOneOf (OneOf n x) xs)
    (updateB w (xs !! n))

instance (Widget m1,Widget m2) => Widget (m1,m2) where
  type Msg (m1,m2) = Either (Msg m1) (Msg m2)
  -- Don't like the duplication beween view and update routing.
  -- could we use lenses to handle the plumbing? Surely?
  view (a,b) = array [ Left <$> view a, Right <$> view b]

instance (ApplicativeUpdate m1, ApplicativeUpdate m2) =>
    ApplicativeUpdate (m1,m2) where
  updateA (Left w) (a,b) = fmap
    (\ x -> (x,b))
    (updateA w a)
  updateA (Right w) (a,b) = fmap
    (\ x -> (a,x))
    (updateA w b)

instance (BiapplicativeUpdate m1, BiapplicativeUpdate m2) =>
    BiapplicativeUpdate (m1,m2) where
  updateB (Left w) (a,b) = bimap
    (Left <$>)
    (\ x -> (x,b))
    (updateB w a)
  updateB (Right w) (a,b) = bimap
    (Right <$>)
    (\ x -> (a,x))
    (updateB w b)

-- Here are the base instances. We do not default view to this pattern, 
-- because it would complicate the API for Widget to save 4 lines of 
-- (internal) code.

instance Widget Double where
  type Msg Double = Double
  view = primitive "double"

instance Widget Text where
  type Msg Text = Text
  view = primitive "text"

instance Widget Bool where
  type Msg Bool = Bool
  view = primitive "bool"

tag :: String -> Remote msg
tag = send . pack

primitive :: (ToJSON m, ToResponse m) => Text -> m -> Remote m
primitive txt n = object 
      [ ("type"  , send txt)  -- type gives the type name
      , ("value" , send n)   -- the outgoing value
      , ("event" , recv)     -- the event reply
      ]  

