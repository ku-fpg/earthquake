{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

-- classic ToDo example. Based on the Elm and backbone.js versions.

module Main where

import Web.Scotty
import Data.Text as T
import Data.Maybe
import Data.String
import Text.Read (readMaybe)
import Data.Char (isSpace)

import Network.Wai.Middleware.RequestLogger

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
   
    get "/" $ file $ dataDir ++ "/todomvc/index.html"
    get "/earthquake.html" $ file $ dataDir ++ "/todomvc/earthquake.html"

    sequence_
      [ get (fromString f) $ file $ dataDir ++ f
      | f <- [ "/todomvc/node_modules/todomvc-common/base.css"
             , "/todomvc/node_modules/todomvc-app-css/index.css"
             , "/todomvc/css/app.css"
             , "/todomvc/node_modules/todomvc-common/base.js"
             , "/todomvc/js/app.js"
             ]
      ]

    middleware $ logStdoutDev
      
    let startA = pure $ ToDo
          { tasks = []
          , visibility = All
          , checkAll = False
          }

    middleware $ runtime startA updateA


data ToDo = ToDo
    { tasks      :: [Task]      -- List of the TODO tasks
    , visibility :: Visibility  --
    , checkAll   :: Bool
    } deriving (Show, Eq, Ord)
    
  
data Visibility = All | Completed | Active
  deriving (Show, Read, Eq, Ord)

data TodoMsg
    = CreateOnEnter Text
    | TaskMsg (OneOf Task.TaskMsg)
    | DeleteComplete
    | ChangeVisibility Visibility -- The router uses this
    | CheckAll Bool

instance Widget ToDo where
  type Msg ToDo = TodoMsg
  
  view :: ToDo -> Remote TodoMsg
  view todo@ToDo{..} = object
    [ ( "type"          , tag "ToDo" )
      -- The output
    , ( "visibility"    , send $ show $ visibility )
      -- The input
    , ( "createOnEnter" , CreateOnEnter <$> recv )
    , ( "deletecomplete", wait DeleteComplete )
    , ( "router"        , (ChangeVisibility . read . unpack) <$> recv)
     -- The input/output
    , ( "checkAll"      , CheckAll <$> view checkAll)
    , ( "tasks"         , TaskMsg <$> view tasks )
    ]

instance ApplicativeUpdate ToDo where
  updateA :: Applicative f => TodoMsg -> ToDo -> f ToDo
  updateA (TaskMsg (OneOf n w)) todo@ToDo{..} = pure $ todo
    { tasks = updateOrDeleteOneOf (OneOf n $ Task.updateTask w (tasks !! n)) tasks
    }
  updateA (CreateOnEnter field_) todo@ToDo{..} = newModel
    where description = T.strip field_
          newModel
            | T.all isSpace description = pure todo
            | otherwise = pure $ todo
               { tasks = tasks ++ [ Task.newTask description ]
               }
  updateA DeleteComplete todo@ToDo{..} = pure $ todo
    { tasks = Prelude.filter (not . Task.isComplete) tasks
    }
  updateA (ChangeVisibility v) todo = pure $ todo { visibility = v }
  updateA (CheckAll t) todo@ToDo{..} = pure $ todo
    { checkAll = t
    , tasks = [ task { Task.completed = t } | task <- tasks ]
    }  
