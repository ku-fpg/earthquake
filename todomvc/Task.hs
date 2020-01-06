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

newTask :: Text -> Task
newTask desc = Task
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
        -- outputs
      , ("description" , send description )
        -- inputs
      , ("edit"        , Edit <$> recv )
      , ("delete"      , wait Delete )
        -- input/output
      , ("completed"   , Completed <$> view completed )
      ]

updateTask :: TaskMsg -> Task -> Maybe Task
updateTask (Edit raw) m
    | T.null desc = Nothing
    | otherwise = pure $ m { description = desc }
  where desc = T.strip raw
updateTask (Completed bool) m = pure $ m { completed = bool }
updateTask Delete m = Nothing


