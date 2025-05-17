{-# LANGUAGE OverloadedStrings #-}

module Config
    ( ConnectInfo(..)
    , initDbPool
    , getDbConfig
    ) where

import Database.PostgreSQL.Simple
import Data.Pool
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Get environment variable with a default value
getEnvWithDefault :: String -> String -> IO String
getEnvWithDefault key defaultValue = do
    value <- lookupEnv key
    return $ fromMaybe defaultValue value

-- Get integer environment variable with a default value
getIntEnvWithDefault :: String -> Int -> IO Int
getIntEnvWithDefault key defaultValue = do
    value <- lookupEnv key
    return $ fromMaybe defaultValue (value >>= readMaybe)

-- Get database configuration from environment variables
getDbConfig :: IO ConnectInfo
getDbConfig = do
    host <- getEnvWithDefault "DB_HOST" "localhost"
    port <- getIntEnvWithDefault "DB_PORT" 5432
    user <- getEnvWithDefault "DB_USER" "postgres"
    password <- getEnvWithDefault "DB_PASSWORD" "postgres"
    database <- getEnvWithDefault "DB_NAME" "haskell_api"
    return ConnectInfo
        { connectHost = host
        , connectPort = fromIntegral port
        , connectUser = user
        , connectPassword = password
        , connectDatabase = database
        }

-- Initialize the connection pool
initDbPool :: IO (Pool Connection)
initDbPool = do
    dbConfig <- getDbConfig
    newPool $ defaultPoolConfig
        (connect dbConfig)  -- create
        close              -- destroy
        10                -- seconds to keep idle connections
        10                -- max connections 