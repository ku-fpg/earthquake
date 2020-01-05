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
    }
  deriving (Show, Eq, Ord)

newTask :: Text -> Int -> Task
newTask desc id = Task
    { description = desc
    , completed = False
    }

isComplete :: Task -> Bool
isComplete Task{..} = completed

data TaskMsg
  = Edit Text
  | Completed Bool
  | Delete

instance Widget Task where
  type Msg Task = TaskMsg
  view task@Task{..} = object
      [ ("type"        , tag "Task" )
      , ("description" , send description )
        -- input/output
      , ("completed"   , Completed <$> view completed )
        -- inputs
      , ("edit"        , Edit <$> recv )
      , ("delete"      , wait Delete )
      ]

updateTask :: TaskMsg -> Task -> Maybe Task
updateTask (Edit raw) m
  | T.null raw = pure m
  | otherwise = pure $ m { description = raw }
updateTask (Completed bool) m = pure $ m { completed = bool }
updateTask Delete m = Nothing


