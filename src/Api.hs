{-# LANGUAGE OverloadedStrings #-}

module Api
    ( startApp
    ) where

import Web.Scotty
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import Data.Aeson (ToJSON, object, (.=))
import Control.Monad.IO.Class (liftIO)
import qualified Models as M
import qualified Data.Text.Lazy as TL

type DbPool = Pool Connection

-- Start the application
startApp :: DbPool -> IO ()
startApp pool = scotty 3000 $ do
    -- User endpoints
    get "/users" $ do
        users <- liftIO $ withResource pool M.getUsers
        json users

    get "/users/:id" $ do
        uid <- param "id"
        users <- liftIO $ withResource pool (`M.getUserById` uid)
        case users of
            [] -> status status404
            (user:_) -> json user

    post "/users" $ do
        user <- jsonData :: ActionM M.User
        result <- liftIO $ withResource pool (`M.createUser` user)
        case result of
            (newUser:_) -> json newUser
            [] -> status status500

    -- Employee endpoints
    get "/employees" $ do
        employees <- liftIO $ withResource pool M.getEmployees
        json employees

    get "/employees/:id" $ do
        eid <- param "id"
        employees <- liftIO $ withResource pool (`M.getEmployeeById` eid)
        case employees of
            [] -> do
                status status404
                json $ object ["error" .= ("Employee not found" :: String)]
            (employee:_) -> json employee

    post "/employees" $ do
        employee <- jsonData :: ActionM M.Employee
        result <- liftIO $ withResource pool (`M.createEmployee` employee)
        case result of
            (newEmployee:_) -> json newEmployee
            [] -> do
                status status500
                json $ object ["error" .= ("Failed to create employee" :: String)]

    put "/employees/:id" $ do
        eid <- param "id"
        employee <- jsonData :: ActionM M.Employee
        result <- liftIO $ withResource pool (\conn -> M.updateEmployee conn eid employee)
        case result of
            [] -> do
                status status404
                json $ object ["error" .= ("Employee not found" :: String)]
            (updatedEmployee:_) -> json updatedEmployee

    delete "/employees/:id" $ do
        eid <- param "id"
        success <- liftIO $ withResource pool (`M.deleteEmployee` eid)
        if success
            then status status204
            else do
                status status404
                json $ object ["error" .= ("Employee not found" :: String)]

    -- Basic error handling
    notFound $ do
        status status404
        json $ object ["error" .= ("Route not found" :: String)] 