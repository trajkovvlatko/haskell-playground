module Lib (run) where

import Configuration.Dotenv (defaultConfig, loadFile)
import Db (Connection, getConnection)
import Ui (showMenu)
import Actions (list, preview, add, update, remove, retry)

menuSelection :: Connection -> IO ()
menuSelection conn = do
  showMenu
  action <- getLine
  case action of
    "1" -> list conn
    "2" -> preview conn
    "3" -> add conn
    "4" -> update conn
    "5" -> remove conn
    _   -> retry
  menuSelection conn

loadEnv :: IO [(String, String)]
loadEnv = loadFile defaultConfig

run :: IO ()
run = loadEnv >> getConnection >>= menuSelection