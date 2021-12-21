{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Db
    ( getById
    , getAll
    , getConnection
    , toggle
    , save
    , delete
    , todoId
    , todoTitle
    , todoMessage
    , todoComplete
    , validateOutput
    , validateInput
    , Todo
    , Connection
    ) where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import qualified System.Environment as ENV
import Control.Exception (try)

data Todo = Todo
          { todoId :: Int
          , todoTitle :: String
          , todoMessage :: String
          , todoComplete :: Bool
          } deriving (Show)

type DbResponse = Maybe (Either SqlError [Todo])

instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field <*> field

getEnv :: IO (String, String, String, String)
getEnv = do
  host <- ENV.getEnv "PG_HOST"
  db <- ENV.getEnv "PG_DB"
  user <- ENV.getEnv "PG_USER"
  pass <- ENV.getEnv "PG_PASS"
  return (host, db, user, pass)

getDbConfig :: (String, String, String, String) -> IO ConnectInfo
getDbConfig (host, db, user, pass) = do
  return defaultConnectInfo
        { connectHost = host
        , connectDatabase = db
        , connectUser = user
        , connectPassword = pass
        }

getConnection :: IO Connection
getConnection = getEnv >>= getDbConfig >>= connect

getAll :: Connection -> IO DbResponse
getAll conn = try dbQuery >>= toIOMaybe
  where dbQuery = query_ conn "SELECT id, title, message, complete FROM todos ORDER BY complete ASC, id DESC;"

getById :: Connection -> Int -> IO DbResponse
getById conn id = try dbQuery >>= toIOMaybe
  where dbQuery = query conn "SELECT id, title, message, complete FROM todos WHERE id = ?" (Only id)

save :: Connection -> Maybe (String, String) -> IO DbResponse
save conn input = do
  case input of
    Just (title, message) -> try dbQuery >>= toIOMaybe
      where dbQuery = query conn "INSERT INTO todos (title, message) VALUES (?, ?) RETURNING id, title, message, complete" [title, message]
    Nothing -> return Nothing

toggle :: Connection -> Int -> IO DbResponse
toggle conn id = try dbQuery >>= toIOMaybe
  where dbQuery = query conn "UPDATE todos SET complete = NOT complete WHERE id = ? RETURNING id, title, message, complete" [id]

delete :: Connection -> Int -> IO DbResponse
delete conn id = try dbQuery >>= toIOMaybe
  where dbQuery = query conn "DELETE FROM todos WHERE id = ? RETURNING id, title, message, complete" [id]

toIOMaybe :: a -> IO (Maybe a)
toIOMaybe a = do
  return $ Just a

validateOutput :: DbResponse -> IO (Maybe [Todo])
validateOutput = \case
  Just input -> do
    case input of
      Right todos -> return $ Just todos
      Left err -> return Nothing
  Nothing -> return Nothing

validateInput :: (String, String) -> IO (Maybe (String, String))
validateInput (title, message) = if title /= "" && message /= ""
                                  then return $ Just (title, message)
                                  else return Nothing