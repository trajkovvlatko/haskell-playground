{-# LANGUAGE DeriveGeneric #-}

module Lib
    ( run
    ) where

import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson ( FromJSON, eitherDecode )
import Configuration.Dotenv (loadFile, defaultConfig)
import qualified System.Environment as ENV

data MainInfo = MainInfo { temp           :: Float
                         , feels_like     :: Float
                         , humidity       :: Float
                         } deriving (Show, Generic)

newtype Weather = Weather { main :: MainInfo } deriving (Show, Generic)

instance FromJSON Weather
instance FromJSON MainInfo

getApiKey :: IO String
getApiKey = ENV.getEnv "APIKEY"

endpoint :: String -> String -> String
endpoint city apiKey =
    let host = "http://api.openweathermap.org"
        path = "/data/2.5/weather"
        query = "?q=" ++ city ++ "&units=metric&appid=" ++ apiKey
        in host ++ path ++ query

getJSON :: String -> String  -> IO B.ByteString
getJSON city apiKey = simpleHttp $ endpoint city apiKey

showWeather :: MainInfo -> IO ()
showWeather mainInfo = do
    putStrLn $ "Current temperature : " ++ show (temp mainInfo) ++ "C"
    putStrLn $ "Feels like          : " ++ show (feels_like mainInfo) ++ "C"
    putStrLn $ "Humidity            : " ++ show (humidity mainInfo) ++ "%"

run :: IO ()
run = do
    loadFile defaultConfig

    putStrLn "Search city: "
    city <- getLine
    apiKey <- getApiKey

    response <- (eitherDecode <$> getJSON city apiKey) :: IO (Either String Weather)
    case response of
        Left error -> print error
        Right weather -> showWeather $ main weather