{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}

module Task where

import Data.Text as T

import Network.Earthquake.Widget
import Network.Earthquake.Runtime
import Network.Earthquake.Remote

data Task = Task
    { description :: Text
    , completed   :: Bool
    , edits       :: Maybe Text
    , id          :: Int
    } | NoTask
        -- TODO: should be possible to abstract 
  deriving (Show, Eq, Ord)

newTask :: Text -> Int -> Task
newTask desc id = Task
    { description = desc
    , completed = False
    , edits = Nothing
    , id = id
    }

isComplete :: Task -> Bool
isComplete Task{..} = completed

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
        -- inputs
      , ("focus"       , wait Focus )
      , ("edit"        , Edit <$> recv )
      , ("cancel"      , wait Cancel )
      , ("commit"      , wait Commit )
      , ("completed"   , Completed <$> recv )
      , ("delete"      , wait Delete )
      ]
  view NoTask = object [("type" , tag "NoTask")]

updateTask :: TaskMsg -> Task -> Maybe Task
-- Focus means that you are actively editing
updateTask Focus m@Task{description}  = pure $ m { edits = Just description }
-- An edit message updates the internal edit field.
updateTask (Edit description) m       = pure $ m { edits = Just description }
-- cancel leaves the edit state (aka unfocus?)
updateTask Cancel m                   = pure $ m { edits = Nothing }
-- can commit changes to this entry
updateTask Commit m@Task{edits} = case edits of
  Nothing -> pure m
  Just raw | T.null raw -> pure NoTask
           | otherwise -> pure $ m { edits = Nothing
    	       		     , description = raw
		     }
updateTask (Completed bool) m = pure $ m { completed = bool }
updateTask Delete m = Nothing


