{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Earthquake.Remote
  ( Remote
  , Pair
  , ResponseType
  , ToResponse
  , object
  , array
  , wait
  , send
  , recv
  , sendRemote
  , metaRemote
  , recvRemote
  , ResponseEvent
  , response
  ) where

import Control.Applicative         ((<|>))
import Control.Monad.Trans.State   (State,put,get,evalState)
import Data.Aeson                  (Value,ToJSON(..),toJSON,fromJSON,FromJSON(..),withObject,(.:), (.=), Result(..))
import qualified Data.Aeson as A
import Data.Maybe(isJust)
import Data.Text(Text, unpack, pack)

import Prelude hiding (id)

-- | A 'Remote' functor that returns a message when evaluated.
data Remote msg where
  Send       :: ToJSON a => a -> Remote msg
  Response   :: FromJSON a => ResponseType -> Remote a
  MapRemote  :: (a -> b) -> Remote a -> Remote b
  Object     :: [Pair msg] -> Remote msg
  Array      :: [Remote msg] -> Remote msg  

instance Functor Remote where
  fmap = MapRemote

type Pair msg = (Text, Remote msg)

data ResponseType
  = ResponseUnit
  | ResponseDouble
  | ResponseText
  | ResponseBool
  deriving (Eq, Ord, Show)

instance ToJSON ResponseType where
  toJSON ResponseUnit = "unit"
  toJSON ResponseDouble = "double"
  toJSON ResponseText = "text"
  toJSON ResponseBool = "bool"

data ResponsePath = ResponsePath [Text] ResponseType
  deriving (Eq, Ord, Show)

addToResponsePath :: Text -> ResponsePath -> ResponsePath
addToResponsePath p (ResponsePath ps ty) = ResponsePath (p:ps) ty

instance ToJSON ResponsePath where
  toJSON (ResponsePath path ty) = A.object
    [ "path" .= path
    , "type" .= ty
    ]


class ToResponse msg where
  recv :: Remote msg

instance ToResponse () where
  recv = response ResponseUnit

instance ToResponse Double where
  recv = response ResponseDouble

instance ToResponse Text where
  recv = response ResponseText

instance ToResponse Bool where
  recv = response ResponseBool

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

response :: FromJSON msg => ResponseType -> Remote msg
response = Response

------------------------------------------------------------------------------
-- Sending

-- JSON-like structure for representing sending.
-- We want to keep hold of the types of the RecvValue,
-- otherwise we would use Value here.

data SendJSON
 = SendValue Value              -- a value to send
 | RecvValue Int ResponseType   -- an id and type for a recv
 | SendObject [(Text,SendJSON)]
 | SendArray [SendJSON]
 deriving (Show)


instance ToJSON SendJSON where
  toJSON (SendValue v) = v
  toJSON (RecvValue i t) = toJSON i
  toJSON (SendObject pairs) = A.object
    [ (lbl .= r) | (lbl,r) <- pairs ]
  toJSON (SendArray rs) = toJSON (toJSON <$> rs)
  
sendRemote :: Remote msg -> SendJSON
sendRemote = flip evalState 0 . sendRemote'

sendRemote' :: Remote msg -> State Int SendJSON
sendRemote' (Send a) = pure $ SendValue $ toJSON a
sendRemote' (Response r) = do
  i <- alloc
  return $ RecvValue i r
sendRemote' (MapRemote _ r) = sendRemote' r
sendRemote' (Object pairs) = SendObject <$> sequenceA
  [ (\ r' -> (lbl,r')) <$> sendRemote' r 
  | (lbl, r) <- pairs
  ]
sendRemote' (Array rs) = SendArray <$> sequenceA
  (sendRemote' <$> rs)

-- meta information about types of responses
-- result starts at id #0, and is consecutive.
metaRemote :: SendJSON -> [ResponsePath]
metaRemote (SendValue v) = []
metaRemote (RecvValue i t) = [ResponsePath [] t]
metaRemote (SendObject pairs) = concat
  [ addToResponsePath p <$> metaRemote v
  | (p,v) <- pairs 
  ]
metaRemote (SendArray rs) = concat
  [ addToResponsePath (pack $ show i) <$> metaRemote v
  | (i::Int,v) <- [0..] `zip` rs
  ]

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
