{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Network.JavaScript.Remote
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
  ) where

import Control.Applicative         ((<|>))
import Control.Monad.Trans.State   (State,put,get)
import Data.Aeson                  (Value,ToJSON,toJSON,FromJSON(..),withObject,(.:), (.=))
import qualified Data.Aeson as A
import Data.Maybe(isJust)
import Data.Text(Text)

-- | A 'Remote' functor that returns a message when evaluated.
data Remote msg where
  Send       :: ToJSON a => a -> Remote msg
  Response   :: Response a -> Remote a
  MapRemote  :: (a -> b) -> Remote a -> Remote b
  Object     :: [Pair msg] -> Remote msg
  Array      :: [Remote msg] -> Remote msg  

instance Functor Remote where
  fmap = MapRemote

type Pair msg = (Text, Remote msg)

-- | The types of response messages
data Response msg where
  ResponseUnit   :: Response ()
  ResponseDouble :: Response Double
  ResponseText   :: Response Text
  ResponseBool   :: Response Bool

class ToResponse msg where
  witness :: Response msg

instance ToResponse () where
  witness = ResponseUnit

instance ToResponse Double where
  witness = ResponseDouble

instance ToResponse Text where
  witness = ResponseText

instance ToResponse Bool where
  witness = ResponseBool

-- | A response event, which is type evidence, id number, and value
data ResponseEvent where
  ResponseEvent :: Response a -> Int -> a -> ResponseEvent

instance Show ResponseEvent where
  show (ResponseEvent ResponseUnit   i a) = show (i,a)
  show (ResponseEvent ResponseDouble i a) = show (i,a)
  show (ResponseEvent ResponseText   i a) = show (i,a)
  show (ResponseEvent ResponseBool   i a) = show (i,a)

instance FromJSON ResponseEvent where
  parseJSON = withObject "ResponseEvent" $ \v ->
    let response :: FromJSON a => Response a -> _ ResponseEvent
        response r = ResponseEvent r <$> (v .: "id") <*> (v .: "value")
    in response ResponseUnit   <|>
       response ResponseDouble <|>
       response ResponseText   <|>
       response ResponseBool

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

recv :: ToResponse msg => Remote msg
recv = Response witness

------------------------------------------------------------------------------
-- Sending

sendRemote :: Remote msg -> State Int Value
sendRemote (Send a) = pure $ toJSON a
sendRemote (Response _) = toJSON <$> alloc 
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
recvRemote (Response r') (ResponseEvent r i e) = do
  i' <- alloc
  if i == i'
  then case (r',r,e) of
         (ResponseUnit,ResponseUnit,()) -> return $ Just ()
         (ResponseDouble,ResponseDouble,d) -> return $ Just d
         (ResponseText,ResponseText,t) -> return $ Just t
         (ResponseBool,ResponseBool,b) -> return $ Just b
         _ -> return Nothing -- internal error!
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
