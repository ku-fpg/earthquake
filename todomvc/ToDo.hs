{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE InstanceSigs #-}

-- classic ToDo example. Based on the Elm version.

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
      
{-
    let startA = pure $ ToDo
          { tasks = []
          , field = ""
          , uid = 100
          , visibility = All
          }

    middleware $ runtime startA updateA
-}
    return ()

data ToDo = ToDo
    { tasks      :: [Task]      -- List of the TODO tasks
    , field      :: Text        -- The top entry textbox
    , uid        :: Int         -- name supply for id
    , visibility :: Visibility  --
    } deriving (Show, Eq, Ord)
    
  
data Visibility = All | Completed | Active
  deriving (Show, Read, Eq, Ord)

{-
type Msg
    = NoOp
    | UpdateField String
    | Add
    | UpdateTask ( Int, Todo.Task.Msg )
    | DeleteComplete
    | CheckAll Bool
    | ChangeVisibility String
-}

data TodoMsg
    = UpdateField Text
    | Add
    | TaskMsg (OneOf Task.TaskMsg)
    | DeleteComplete
    | ChangeVisibility Visibility

instance Widget ToDo where
  type Msg ToDo = TodoMsg
  
  view :: ToDo -> Remote TodoMsg
  view todo@ToDo{..} = object
    [ ( "type"        , tag "ToDo" )
    , ( "tasks"       , TaskMsg <$> view tasks )
    , ( "field"       , send field )
    , ( "uid"         , send uid )
    , ( "visibility"  , send $ show $ visibility )
      -- The input
    , ( "add"         , wait Add )
    , ( "updatefield" , UpdateField <$> recv )
    , ( "deletecomplete", wait DeleteComplete )
    ]

instance ApplicativeUpdate ToDo where
  updateA :: Applicative f => TodoMsg -> ToDo -> f ToDo
  updateA (TaskMsg (OneOf n w)) todo@ToDo{..} = pure $ todo
    { tasks = updateOrDeleteOneOf (OneOf n $ Task.updateTask w (tasks !! n)) tasks
    }
  updateA (UpdateField txt) todo@ToDo{..} = pure $ todo
    { field = txt
    }
  updateA Add todo@ToDo{..} = newModel
    where description = T.strip field
          newModel
            | T.all isSpace description = pure todo
            | otherwise = pure $ todo
               { uid = succ uid
               , field = ""
               , tasks = tasks ++ [ Task.newTask description uid ]
               }
  updateA DeleteComplete todo@ToDo{..} = pure $ todo
    { tasks = Prelude.filter (not . Task.isComplete) tasks
    }
            
    
{-  
  view todo@ToDo{..} = update <$> view
    where
      update :: Msg -> ToDo
      update (ChangeVisibility v) = todo { visibility = v }

-}

{-
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
