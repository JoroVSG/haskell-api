{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main (main) where

import qualified Web.Scotty as Scotty
import Config (initDbPool, getDbConfig)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection, execute_, Only(..), query_, ConnectInfo(..))
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), Value)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import GHC.Generics
import Control.Monad.IO.Class (liftIO)
import Web.Scotty (ActionM)
import Network.HTTP.Types.Status (status201, status400, status404, status500, status503)
import Network.Wai (pathInfo, Response, Request, responseLBS)
import qualified Models as M
import Data.Pool (withResource)
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Exception (SomeException, catch)

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
    -- List users endpoint
    Scotty.get "/users" $ do
        users <- liftIO $ withResource pool M.getUsers
        Scotty.json users

    -- Get user by ID endpoint
    Scotty.get "/users/:id" $ do
        uid <- Scotty.pathParam "id"
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

-- Run database migrations
runMigrations :: Connection -> IO ()
runMigrations conn = do
    putStrLn "Running database migrations..."
    
    -- Create users table if it doesn't exist
    putStrLn "Checking users table..."
    execute_ conn "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL)"
    
    -- Check if the constraint exists before trying to add it
    putStrLn "Checking email uniqueness constraint..."
    constraintExists <- query_ conn "SELECT COUNT(*) FROM pg_constraint WHERE conname = 'users_email_unique'" :: IO [Only Int]
    case constraintExists of
        [Only 0] -> do
            putStrLn "Adding email uniqueness constraint..."
            _ <- execute_ conn "ALTER TABLE users ADD CONSTRAINT users_email_unique UNIQUE (email)"
            putStrLn "Constraint added successfully"
        _ -> putStrLn "Email uniqueness constraint already exists, skipping..."
    
    putStrLn "Migrations completed successfully"

-- Get port from environment variable
getPort :: IO Int
getPort = do
    portStr <- lookupEnv "PORT"
    return $ fromMaybe 3000 (portStr >>= readMaybe)

main :: IO ()
main = do
    putStrLn "Starting application initialization..."
    port <- getPort
    putStrLn $ "Port configuration: " ++ show port

    putStrLn "Starting database initialization..."
    putStrLn "Getting database configuration..."
    dbConfig <- getDbConfig
    putStrLn $ "Database config obtained: " ++ show (connectHost dbConfig) ++ ":" ++ show (connectPort dbConfig)
    
    putStrLn "Attempting to create database pool..."
    pool <- initDbPool `catch` \e -> do
        putStrLn $ "Database connection error: " ++ show (e :: SomeException)
        error "Failed to initialize database pool"
    putStrLn "Database pool created successfully"
    
    putStrLn $ "Starting Scotty server on port " ++ show port
    Scotty.scotty port $ do
        Scotty.middleware $ \app req respond -> do
            liftIO $ putStrLn $ "Received request to: " ++ show (pathInfo req)
            app req respond

        -- Health check endpoint
        Scotty.get "/health" $ do
            liftIO $ putStrLn "Health check endpoint accessed"
            Scotty.json $ object ["status" .= ("OK" :: String)]
            
        -- Swagger documentation endpoint
        Scotty.get "/swagger.json" $ do
            liftIO $ putStrLn "Swagger documentation endpoint accessed"
            Scotty.json apiDocs
            
        -- User routes
        Scotty.get "/users" $ do
            liftIO $ putStrLn "Getting all users"
            users <- liftIO $ withResource pool M.getUsers
            Scotty.json users

        Scotty.get "/users/:id" $ do
            uid <- Scotty.param "id"
            liftIO $ putStrLn $ "Getting user by id: " ++ show uid
            users <- liftIO $ withResource pool (`M.getUserById` uid)
            case users of
                [] -> do
                    Scotty.status status404
                    Scotty.json $ object ["error" .= ("User not found" :: String)]
                (user:_) -> Scotty.json user

        Scotty.post "/users" $ do
            user <- Scotty.jsonData :: ActionM M.User
            liftIO $ putStrLn $ "Creating new user: " ++ show user
            result <- liftIO $ withResource pool (`M.createUser` user)
            case result of
                (newUser:_) -> do
                    Scotty.status status201
                    Scotty.json newUser
                [] -> do
                    Scotty.status status500
                    Scotty.json $ object ["error" .= ("Failed to create user" :: String)]

    putStrLn "Application fully initialized and running..."
