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

import Paths_earthquake

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
--  dataDir <- getDataDir
  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/Sliders.html"
    middleware $ runtime (pure $ newTask "X" 99) updateA


data Task = Task
    { description :: Text
    , completed   :: Bool
    , edits       :: Maybe Text
    , id          :: Int
    } | NoTask
  deriving Show

newTask :: Text -> Int -> Task
newTask desc id = Task
    { description = desc
    , completed = False
    , edits = Nothing
    , id = id
    }

data TaskMsg
  = Focus
  | Edit Text
  | Cancel
  | Commit
  | Completed Bool
  | Delete

instance Widget Task where
  type Msg Task = TaskMsg
  view task@Task{..} = object
      [ ("type"        , tag "Task" )
      , ("description" , send description )
      , ("complete"    , send completed )
      , ("edits"       , send edits )
      , ("id"          , send id )
      , ("focus"       , wait Focus )
      , ("edit"        , Edit <$> recv )
      , ("cancel"      , wait Cancel )
      , ("completed"   , Completed <$> recv )
      , ("delete"      , wait Delete )
      ]
  view NoTask = object [("type" , tag "NoTask")]

instance ApplicativeUpdate Task where
  updateA Focus m@Task{description}  = pure $ m { edits = Just description }
  updateA (Edit description) m       = pure $ m { edits = Just description }
  updateA Cancel m                   = pure $ m { edits = Nothing }
  updateA Commit m@Task{edits,description} = case edits of
    Nothing -> pure m
    Just raw | T.null raw -> pure NoTask
             | otherwise -> pure $ m { edits = Nothing
	     	       		     , description = description 
				     }
  updateA (Completed bool) m = pure $ m { completed = bool }
  updateA Delete _ = pure NoTask

{-
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
  
-}
