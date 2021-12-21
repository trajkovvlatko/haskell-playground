{-# LANGUAGE LambdaCase #-}

module Ui
  ( showOne
  , showAll
  , showMenu
  , invalidInput
  , askForContent
  , askForId
) where

import Db (Todo, todoId, todoTitle, todoMessage, todoComplete)
import Prelude hiding (id)
import Database.PostgreSQL.Simple (SqlError)

showOne :: Todo -> IO ()
showOne todo = do
  putStrLn "-------------------------------"
  putStrLn $ "ID: " ++ show (todoId todo)
  putStrLn $ "Title: " ++ todoTitle todo
  putStrLn $ "Message: " ++ todoMessage todo
  putStrLn $ if todoComplete todo then "Completed" else "Not completed"

showAll :: Maybe [Todo] -> IO ()
showAll = \case
  Just todos -> mapM_ showOne todos
  Nothing -> putStrLn "An error occured."

showMenu :: IO ()
showMenu = do
  putStrLn ""
  putStrLn "-------------------------------"
  putStrLn ""
  putStrLn "1: List all todos"
  putStrLn "2: Show a todo"
  putStrLn "3: Add a new todo"
  putStrLn "4: Toggle complete state"
  putStrLn "5: Delete a todo"
  putStrLn "Pick action:"

invalidInput :: IO ()
invalidInput = putStrLn "Invalid action selected. Try again."

askForContent :: IO (String, String)
askForContent = do
  putStrLn "Add title:"
  title <- getLine
  putStrLn "Add message:"
  message <- getLine
  return (title, message)

askForId :: IO Int
askForId = putStrLn "Pick id: " >> read <$> getLine