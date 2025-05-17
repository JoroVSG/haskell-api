{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import qualified Web.Scotty as Scotty
import Config (initDbPool, getDbConfig)
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), Value)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM)
import Network.HTTP.Types.Status (status201, status400, status404, status500)
import qualified Models as M
import Data.Pool (withResource)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- Data types
data HealthResponse = HealthResponse 
    { status :: String
    } deriving (Show, Generic)

instance ToJSON HealthResponse

-- API Documentation
apiDocs :: Value
apiDocs = object
    [ "info" .= object
        [ "title" .= ("Haskell API" :: String)
        , "version" .= ("1.0" :: String)
        , "description" .= ("A simple REST API built with Scotty" :: String)
        ]
    , "paths" .= object
        [ "/health" .= object
            [ "get" .= object
                [ "summary" .= ("Health Check" :: String)
                , "description" .= ("Returns the health status of the API" :: String)
                , "produces" .= [("application/json" :: String)]
                , "responses" .= object
                    [ "200" .= object
                        [ "description" .= ("Successful health check" :: String)
                        ]
                    ]
                ]
            ]
        , "/users" .= object
            [ "get" .= object
                [ "summary" .= ("List Users" :: String)
                , "description" .= ("Returns a list of all users" :: String)
                , "produces" .= [("application/json" :: String)]
                , "responses" .= object
                    [ "200" .= object
                        [ "description" .= ("List of users" :: String)
                        ]
                    ]
                ]
            , "post" .= object
                [ "summary" .= ("Create User" :: String)
                , "description" .= ("Creates a new user" :: String)
                , "consumes" .= [("application/json" :: String)]
                , "produces" .= [("application/json" :: String)]
                , "parameters" .= [ object
                    [ "name" .= ("body" :: String)
                    , "in" .= ("body" :: String)
                    , "required" .= True
                    , "schema" .= object
                        [ "type" .= ("object" :: String)
                        , "properties" .= object
                            [ "userName" .= object
                                [ "type" .= ("string" :: String)
                                ]
                            , "userEmail" .= object
                                [ "type" .= ("string" :: String)
                                ]
                            ]
                        ]
                    ]
                ]
                , "responses" .= object
                    [ "201" .= object
                        [ "description" .= ("User created successfully" :: String)
                        ]
                    , "400" .= object
                        [ "description" .= ("Invalid request" :: String)
                        ]
                    ]
                ]
            ]
        , "/users/{id}" .= object
            [ "get" .= object
                [ "summary" .= ("Get User by ID" :: String)
                , "description" .= ("Returns a single user by ID" :: String)
                , "parameters" .= [ object
                    [ "name" .= ("id" :: String)
                    , "in" .= ("path" :: String)
                    , "required" .= True
                    , "type" .= ("integer" :: String)
                    ]
                ]
                , "produces" .= [("application/json" :: String)]
                , "responses" .= object
                    [ "200" .= object
                        [ "description" .= ("User found" :: String)
                        ]
                    , "404" .= object
                        [ "description" .= ("User not found" :: String)
                        ]
                    ]
                ]
            ]
        ]
    ]

-- Error handling
jsonError :: String -> ActionM ()
jsonError message = do
    Scotty.status status400
    Scotty.json $ object ["error" .= message]

-- API implementation
app :: Pool Connection -> Scotty.ScottyM ()
app pool = do
    -- Health check endpoint
    Scotty.get "/health" $ do
        Scotty.json $ HealthResponse "OK"

    -- List users endpoint
    Scotty.get "/users" $ do
        users <- liftIO $ withResource pool M.getUsers
        Scotty.json users

    -- Get user by ID endpoint
    Scotty.get "/users/:id" $ do
        uid <- Scotty.param "id"
        users <- liftIO $ withResource pool (`M.getUserById` uid)
        case users of
            [] -> do
                Scotty.status status404
                Scotty.json $ object ["error" .= ("User not found" :: String)]
            (user:_) -> Scotty.json user

    -- Create user endpoint
    Scotty.post "/users" $ do
        user <- Scotty.jsonData :: ActionM M.User
        result <- liftIO $ withResource pool (`M.createUser` user)
        case result of
            (newUser:_) -> do
                Scotty.status status201
                Scotty.json newUser
            [] -> do
                Scotty.status status500
                Scotty.json $ object ["error" .= ("Failed to create user" :: String)]

    -- API documentation endpoint
    Scotty.get "/swagger.json" $ do
        Scotty.json apiDocs

    -- Not found handler
    Scotty.notFound $ do
        Scotty.status status404
        Scotty.json $ object ["error" .= ("Route not found" :: String)]

-- Get port from environment variable
getPort :: IO Int
getPort = do
    portStr <- lookupEnv "PORT"
    return $ fromMaybe 3000 (portStr >>= readMaybe)

main :: IO ()
main = do
    port <- getPort
    putStrLn $ "Starting server on port " ++ show port ++ "..."
    pool <- initDbPool
    Scotty.scotty port (app pool)
