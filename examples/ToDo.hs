{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

-- classic ToDo example. Based on the Elm version.

module Main where

import Web.Scotty
import Data.Text as T
import Data.Maybe
import Text.Read (readMaybe)

import Network.Earthquake.Widget
import Network.Earthquake.Runtime
import Network.Earthquake.Remote

import qualified Task
import           Task(Task)
--import Paths_earthquake

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
--  dataDir <- getDataDir
  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Sliders.html"
    middleware $ runtime (pure $ Task.newTask "X" 99) updateA

data ToDo = ToDo
    { tasks      :: [Task]      -- List of the TODO tasks
    , field      :: String      -- ??
    , uid        :: Int         -- name supply for id
    , visibility :: Visibility  --
    }
  
data Visibility = All | Completed | Active
  deriving (Show, Read, Eq, Ord)

data Msg
    = ChangeVisibility Visibility

{-
instance Widget ToDo ToDo where
  widget todo@ToDo{..} = update <$> view
    where
      update :: Msg -> ToDo
      update (ChangeVisibility v) = todo { visibility = v }
      view :: Remote Msg
      view = object
          [ "type"        := tag "Todo"
          , "tasks"       := (error "tasks" :: OneOf Task -> Msg) <$> widget tasks
          , "field"       := send field
          , "uid"         := send uid
          , "visibility"  := ChangeVisibility <$> widget visibility
          ]

instance Widget Visibility Visibility where
  widget = option [All,Completed,Active]

-- an option choice, with default
option :: (Show a, Read a) => [a] -> a -> Remote a
option xs x = object
   [ "type"    := tag "option"
   , "options" := send $ show <$> xs
   , "option"  := send $ show x
   , "choice" := (fromMaybe x . readMaybe . unpack) <$> recv
   ]

sendShow :: Show a => a -> Remote w
sendShow = send . pack . show
  
-}
