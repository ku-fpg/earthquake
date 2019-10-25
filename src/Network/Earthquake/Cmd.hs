{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Earthquake.Cmd where

import Control.Concurrent
import Control.Concurrent.STM

data Cmd msg where
  Futures :: (TChan msg -> IO ()) -> Cmd msg
  MconcatCmd :: [Cmd msg] -> Cmd msg
  FmapCmd :: (a -> b) -> Cmd a -> Cmd b

instance Functor Cmd where 
  fmap = FmapCmd

instance Semigroup (Cmd msg) where 
  (<>) = mappend

instance Monoid (Cmd msg) where 
  mempty = mconcat []
  mappend a b = mconcat [a,b]
  mconcat = MconcatCmd

cmd :: IO msg -> Cmd msg
cmd io = futures $ \ ch -> io >>= atomically . writeTChan ch

futures :: (TChan msg -> IO ()) -> Cmd msg
futures = Futures

spawnCmd :: TChan msg -> Cmd msg -> IO ()
spawnCmd ch (Futures io) = do
  _ <- forkIO $ io ch
  return ()
spawnCmd ch (MconcatCmd cmds) = sequence_ (spawnCmd ch <$> cmds)
spawnCmd ch (FmapCmd f cmds) = do
  ch' <- newTChanIO
  spawnCmd ch' cmds
  let loop = do
        atomically $ readTChan ch' >>= writeTChan ch . f
        loop
  _ <- forkIO $ loop
  return ()
