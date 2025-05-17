{-# LANGUAGE OverloadedStrings #-}

module Config
    ( ConnectInfo(..)
    , initDbPool
    , getDbConfig
    ) where

import Database.PostgreSQL.Simple
import Data.Pool (Pool, PoolConfig(..), newPool, defaultPoolConfig)
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
    -- Try Railway's PGHOST first, then fall back to DB_HOST, then default
    host <- getEnvWithDefault "PGHOST" =<< getEnvWithDefault "DB_HOST" "localhost"
    -- Same for other variables
    port <- getIntEnvWithDefault "PGPORT" =<< getIntEnvWithDefault "DB_PORT" 5432
    user <- getEnvWithDefault "PGUSER" =<< getEnvWithDefault "DB_USER" "postgres"
    password <- getEnvWithDefault "PGPASSWORD" =<< getEnvWithDefault "DB_PASSWORD" "postgres"
    database <- getEnvWithDefault "PGDATABASE" =<< getEnvWithDefault "DB_NAME" "haskell_api"
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
        (connect dbConfig)  -- create connection
        close              -- destroy connection
        0.5               -- keep unused resource for 0.5 seconds
        10                -- maximum 10 resources 