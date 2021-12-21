module Actions
  ( list
  , preview
  , add
  , update
  , remove
  , retry
  ) where

import Db
    ( Connection
    , getAll
    , getById
    , save
    , toggle
    , delete
    , validateOutput
    , validateInput)
import Ui (showAll, askForContent, askForId, invalidInput)

list :: Connection -> IO ()
list conn = getAll conn >>= validateOutput >>= showAll

preview :: Connection -> IO ()
preview conn = askForId >>= getById conn >>= validateOutput >>= showAll

add :: Connection -> IO ()
add conn = askForContent >>= validateInput >>= save conn >>= validateOutput >>= showAll

update :: Connection -> IO ()
update conn = askForId >>= toggle conn >>= validateOutput >>= showAll

remove :: Connection -> IO ()
remove conn = askForId >>= delete conn >>= validateOutput >>= showAll

retry :: IO ()
retry = invalidInput