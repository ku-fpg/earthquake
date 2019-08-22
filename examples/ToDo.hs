{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- classic ToDo example. Based on the Elm version.

module Main where

import Web.Scotty
import Data.Text
import Data.Maybe
import Text.Read (readMaybe)

import Network.JavaScript.ElmArchitecture
import Network.JavaScript.Widgets(Slider(..))

import Paths_earthquake

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
--  dataDir <- getDataDir
  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Sliders.html"
    middleware $ elmArchitecture $ Task "Hello" False Nothing 99

data Task = Task
    { description :: Text
    , completed   :: Bool
    , edits       :: Maybe Text
    , id          :: Int
    } | NoTask
  deriving Show

instance Widget Task Task where
  widget task@Task{..} = object
      [ "type"        := tag "Task"
      , "description" := send description
      , "completed"   := send completed
      , "edits"       := send edits
      , "id"          := send id
      , "focus"       := wait $ task { edits = Just description }
      , "edit"        := (\ msg -> task { edits = Just msg })
                       <$> recv
      , "cancel"      := wait $ task { edits = Nothing }
      , "completed"   := (\ bool -> task { completed = bool })
                       <$> recv
      ]
  widget NoTask = object ["type" := tag "NoTask"]

  
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
  
      
