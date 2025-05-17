{-# LANGUAGE OverloadedStrings #-}

module Api
    ( startApp
    ) where

import Web.Scotty
import Data.Pool (Pool)
import Database.PostgreSQL.Simple (Connection)
import Data.Aeson (ToJSON)
import Control.Monad.IO.Class (liftIO)
import qualified Models as M
import qualified Data.Text.Lazy as TL

type DbPool = Pool Connection

-- Start the application
startApp :: DbPool -> IO ()
startApp pool = scotty 3000 $ do
    -- GET /users
    get "/users" $ do
        users <- liftIO $ withResource pool M.getUsers
        json users

    -- GET /users/:id
    get "/users/:id" $ do
        uid <- param "id"
        users <- liftIO $ withResource pool (`M.getUserById` uid)
        case users of
            [] -> status status404
            (user:_) -> json user

    -- POST /users
    post "/users" $ do
        user <- jsonData :: ActionM M.User
        result <- liftIO $ withResource pool (`M.createUser` user)
        case result of
            (newUser:_) -> json newUser
            [] -> status status500

    -- Basic error handling
    notFound $ do
        status status404
        json $ object ["error" .= ("Route not found" :: String)] 