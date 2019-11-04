{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Network.Earthquake.Remote
  ( Remote
  , Pair
  , Response
  , ToResponse
  , object
  , array
  , wait
  , send
  , recv
  , sendRemote
  , recvRemote
  , ResponseEvent
  , response
  ) where

import Control.Applicative         ((<|>))
import Control.Monad.Trans.State   (State,put,get)
import Data.Aeson                  (Value,ToJSON,toJSON,fromJSON,FromJSON(..),withObject,(.:), (.=), Result(..))
import qualified Data.Aeson as A
import Data.Maybe(isJust)
import Data.Text(Text, unpack)

import Prelude hiding (id)

-- | A 'Remote' functor that returns a message when evaluated.
data Remote msg where
  Send       :: ToJSON a => a -> Remote msg
  Response   :: FromJSON a => Text -> Remote a
  MapRemote  :: (a -> b) -> Remote a -> Remote b
  Object     :: [Pair msg] -> Remote msg
  Array      :: [Remote msg] -> Remote msg  

instance Functor Remote where
  fmap = MapRemote

type Pair msg = (Text, Remote msg)

-- | The types of response messages
data Response msg where
  ResponseValue :: FromJSON msg => Text -> Response msg

instance Show (Response msg) where
  show (ResponseValue t) = unpack t

class ToResponse msg where
  recv :: Remote msg

instance ToResponse () where
  recv = response "unit"

instance ToResponse Double where
  recv = response "double"

instance ToResponse Text where
  recv = response "text"

instance ToResponse Bool where
  recv = response "bool"

-- | A response event, which is id number, and value
data ResponseEvent where
    ResponseEvent :: Int -> Value -> ResponseEvent
  deriving Show

instance FromJSON ResponseEvent where
  parseJSON = withObject "ResponseEvent" $ \v -> ResponseEvent
  	       <$> (v .: "id") 
	       <*> (v .: "value")

------------------------------------------------------------------------------
-- Builders  

object :: [Pair msg] -> Remote msg
object = Object

array :: [Remote msg] -> Remote msg
array = Array

wait :: a -> Remote a
wait a = fmap (\ () -> a) recv

send :: ToJSON a => a -> Remote msg
send = Send

response :: FromJSON msg => Text -> Remote msg
response = Response

------------------------------------------------------------------------------
-- Sending

sendRemote :: Remote msg -> State Int Value
sendRemote (Send a) = pure $ toJSON a
sendRemote (Response r) = do
  id <- toJSON <$> alloc
  return $ A.object
    [ ("type" .= r)
    , ("id"   .= id)
    ]
sendRemote (MapRemote _ r) = sendRemote r
sendRemote (Object pairs) = A.object <$> sequenceA
  [ (lbl .=) <$> sendRemote r
  | (lbl, r) <- pairs
  ]
sendRemote (Array rs) = toJSON <$> sequenceA (sendRemote <$> rs)

------------------------------------------------------------------------------
-- Response

recvRemote :: Remote msg -> ResponseEvent -> State Int (Maybe msg)
recvRemote (Send {}) _ = pure Nothing
recvRemote (Response _) (ResponseEvent i v) = do
  i' <- alloc
  if i == i'
  then case fromJSON v of
    Error{} -> return Nothing -- internal (type) error
    Success a -> return $ Just a
  else pure Nothing
recvRemote (Object pairs) we = f <$> sequenceA
    [ recvRemote r we
    | (_, r) <- pairs
    ]
  where
    f xs = head $ filter isJust xs ++ [Nothing]
recvRemote (Array rs) we = f <$> sequenceA
    [ recvRemote r we
    | r <- rs
    ]
  where
    f xs = head $ filter isJust xs ++ [Nothing]
recvRemote (MapRemote f r) ev = fmap f <$> recvRemote r ev

------------------------------------------------------------------------------
-- Utils

alloc :: Enum a => State a a
alloc = do
  s <- get
  put (succ s)
  return s
