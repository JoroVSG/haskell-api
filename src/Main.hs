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
import qualified Api
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Exception (SomeException, catch)
import Network.Wai.Middleware.Cors (cors, simpleCors, simpleCorsResourcePolicy, CorsResourcePolicy(..), simpleHeaders)

-- Data types
newtype HealthResponse = HealthResponse 
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
                , "produces" .= ["application/json" :: String]
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
                , "produces" .= ["application/json" :: String]
                , "responses" .= object
                    [ "200" .= object
                        [ "description" .= ("List of users" :: String)
                        ]
                    ]
                ]
            , "post" .= object
                [ "summary" .= ("Create User" :: String)
                , "description" .= ("Creates a new user" :: String)
                , "consumes" .= ["application/json" :: String]
                , "produces" .= ["application/json" :: String]
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
                , "produces" .= ["application/json" :: String]
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
        , "/employees" .= object
            [ "get" .= object
                [ "summary" .= ("List Employees" :: String)
                , "description" .= ("Returns a list of all employees" :: String)
                , "produces" .= ["application/json" :: String]
                , "responses" .= object
                    [ "200" .= object
                        [ "description" .= ("List of employees" :: String)
                        ]
                    ]
                ]
            , "post" .= object
                [ "summary" .= ("Create Employee" :: String)
                , "description" .= ("Creates a new employee" :: String)
                , "consumes" .= ["application/json" :: String]
                , "produces" .= ["application/json" :: String]
                , "parameters" .= [ object
                    [ "name" .= ("body" :: String)
                    , "in" .= ("body" :: String)
                    , "required" .= True
                    , "schema" .= object
                        [ "type" .= ("object" :: String)
                        , "properties" .= object
                            [ "employeeFirstName" .= object [ "type" .= ("string" :: String) ]
                            , "employeeMiddleName" .= object [ "type" .= ("string" :: String) ]
                            , "employeeLastName" .= object [ "type" .= ("string" :: String) ]
                            , "employeeDisplayName" .= object [ "type" .= ("string" :: String) ]
                            , "employeeEmail" .= object [ "type" .= ("string" :: String) ]
                            , "employeePosition" .= object [ "type" .= ("string" :: String) ]
                            , "employeeAddress" .= object [ "type" .= ("string" :: String) ]
                            , "employeeSite" .= object [ "type" .= ("string" :: String) ]
                            , "employeeManagerId" .= object [ "type" .= ("integer" :: String) ]
                            , "employeeContract" .= object [ "type" .= ("string" :: String), "enum" .= (["FULL_TIME", "PART_TIME"] :: [String]) ]
                            , "employeeStartDate" .= object [ "type" .= ("string" :: String), "format" .= ("date" :: String) ]
                            , "employeeEndDate" .= object [ "type" .= ("string" :: String), "format" .= ("date" :: String) ]
                            , "employeeDepartment" .= object [ "type" .= ("string" :: String) ]
                            , "employeePictureUrl" .= object [ "type" .= ("string" :: String) ]
                            ]
                        ]
                    ]
                ]
                , "responses" .= object
                    [ "201" .= object [ "description" .= ("Employee created successfully" :: String) ]
                    , "400" .= object [ "description" .= ("Invalid request" :: String) ]
                    ]
                ]
            ]
        , "/employees/{id}" .= object
            [ "get" .= object
                [ "summary" .= ("Get Employee by ID" :: String)
                , "description" .= ("Returns a single employee by ID" :: String)
                , "parameters" .= [ object
                    [ "name" .= ("id" :: String)
                    , "in" .= ("path" :: String)
                    , "required" .= True
                    , "type" .= ("integer" :: String)
                    ]
                ]
                , "produces" .= ["application/json" :: String]
                , "responses" .= object
                    [ "200" .= object [ "description" .= ("Employee found" :: String) ]
                    , "404" .= object [ "description" .= ("Employee not found" :: String) ]
                    ]
                ]
            , "put" .= object
                [ "summary" .= ("Update Employee" :: String)
                , "description" .= ("Updates an existing employee" :: String)
                , "parameters" .= [ object
                    [ "name" .= ("id" :: String)
                    , "in" .= ("path" :: String)
                    , "required" .= True
                    , "type" .= ("integer" :: String)
                    ]
                ]
                , "produces" .= ["application/json" :: String]
                , "responses" .= object
                    [ "200" .= object [ "description" .= ("Employee updated" :: String) ]
                    , "404" .= object [ "description" .= ("Employee not found" :: String) ]
                    ]
                ]
            , "delete" .= object
                [ "summary" .= ("Delete Employee" :: String)
                , "description" .= ("Deletes an employee" :: String)
                , "parameters" .= [ object
                    [ "name" .= ("id" :: String)
                    , "in" .= ("path" :: String)
                    , "required" .= True
                    , "type" .= ("integer" :: String)
                    ]
                ]
                , "produces" .= ["application/json" :: String]
                , "responses" .= object
                    [ "204" .= object [ "description" .= ("Employee deleted" :: String) ]
                    , "404" .= object [ "description" .= ("Employee not found" :: String) ]
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

-- Run database migrations
runMigrations :: Connection -> IO ()
runMigrations conn = do
    putStrLn "Running database migrations..."
    
    -- Check if users table exists
    tableExists <- query_ conn "SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'users')" :: IO [Only Bool]
    case tableExists of
        [Only True] -> putStrLn "Users table already exists, skipping creation"
        _ -> do
            putStrLn "Creating users table..."
            execute_ conn "CREATE TABLE IF NOT EXISTS users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, email TEXT NOT NULL)"
            putStrLn "Users table created successfully"
    
    -- Check if email uniqueness constraint exists
    putStrLn "Checking email uniqueness constraint..."
    constraintExists <- query_ conn "SELECT COUNT(*) FROM pg_constraint WHERE conname = 'users_email_unique'" :: IO [Only Int]
    case constraintExists of
        [Only 0] -> do
            putStrLn "Adding email uniqueness constraint..."
            _ <- execute_ conn "ALTER TABLE users ADD CONSTRAINT users_email_unique UNIQUE (email)"
            putStrLn "Constraint added successfully"
        _ -> putStrLn "Email uniqueness constraint already exists, skipping..."

    -- Check if employees table exists
    employeesExists <- query_ conn "SELECT EXISTS (SELECT FROM pg_tables WHERE schemaname = 'public' AND tablename = 'employees')" :: IO [Only Bool]
    case employeesExists of
        [Only True] -> putStrLn "Employees table already exists, skipping creation"
        _ -> do
            putStrLn "Creating employees table..."
            execute_ conn "CREATE TABLE IF NOT EXISTS employees (\
                \id SERIAL PRIMARY KEY, \
                \first_name TEXT NOT NULL, \
                \middle_name TEXT, \
                \last_name TEXT NOT NULL, \
                \display_name TEXT NOT NULL, \
                \email TEXT NOT NULL UNIQUE, \
                \position TEXT NOT NULL, \
                \address TEXT NOT NULL, \
                \site TEXT NOT NULL, \
                \manager_id INTEGER REFERENCES employees(id), \
                \employment_contract TEXT NOT NULL CHECK (employment_contract IN ('FULL_TIME', 'PART_TIME')), \
                \start_date DATE NOT NULL, \
                \end_date DATE, \
                \department TEXT NOT NULL, \
                \picture_url TEXT, \
                \created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP\
                \)"
            putStrLn "Employees table created successfully"
    
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
    
    -- Run database migrations
    putStrLn "Running database migrations..."
    withResource pool runMigrations
    
    putStrLn $ "Starting Scotty server on port " ++ show port
    Scotty.scotty port $ do
        -- CORS middleware
        Scotty.middleware $ cors $ const $ Just simpleCorsResourcePolicy
            { corsRequestHeaders = simpleHeaders
            , corsOrigins = Just (["http://localhost:5173"], True)
            , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
            }

        -- Logging middleware
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
            
        -- Use the Api module for all routes
        Api.startApp pool

    putStrLn "Application fully initialized and running..."
