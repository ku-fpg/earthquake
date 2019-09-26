{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Earthquake.Cmd where

import Control.Concurrent
import Control.Concurrent.STM

data Cmd msg where
  Promise :: IO msg -> Cmd msg
--  Ticker :: Int -> IO msg -> Cmd msg
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
cmd = Promise

spawnCmd :: TChan msg -> Cmd msg -> IO ()
spawnCmd ch (Promise io) = do
  _ <- forkIO $ io >>= atomically . writeTChan ch
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
