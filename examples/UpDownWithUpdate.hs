{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}


module Main where

-- import Data.Aeson             
import Web.Scotty

import Network.JavaScript.ElmArchitecture

import Paths_earthquake

-- The Up/Down example of the elm architecture
newtype Counter = Counter Int
  deriving (Eq, Ord, Enum, Num, Show)

data Update = Up | Down

update :: Update -> Counter -> Counter
update Up   (Counter n) = Counter (n + 1)
update Down (Counter n) = Counter (n - 1)

instance Widget Counter Update where
  widget (Counter n) = object 
        [ "down" := wait $ Down
        , "text" := send (show n)
        , "up"   := wait $ Up
        ]

instance Widget Counter Counter where
  widget c = fmap (flip update c) $ widget c

main :: IO ()
main = main_ 3000

main_ :: Int -> IO ()
main_ i = do
  dataDir <- getDataDir
--  dataDir <- return "."
  scotty i $ do
    get "/" $ file $ dataDir ++ "/examples/UpDown.html"
    middleware $ elmArchitecture $ Counter 0
            
