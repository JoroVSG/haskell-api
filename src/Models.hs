{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Models
    ( User(..)
    , createUser
    , getUsers
    , getUserById
    ) where

import GHC.Generics
import Data.Aeson
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Data.Text (Text)

data User = User
    { userId :: Maybe Int
    , userName :: Text
    , userEmail :: Text
    } deriving (Show, Generic)

instance ToJSON User
instance FromJSON User
instance FromRow User

-- Database queries
createUser :: Connection -> User -> IO [User]
createUser conn user = query conn
    "INSERT INTO users (name, email) VALUES (?, ?) RETURNING id, name, email"
    (userName user, userEmail user)

getUsers :: Connection -> IO [User]
getUsers conn = query_ conn "SELECT id, name, email FROM users"

getUserById :: Connection -> Int -> IO [User]
getUserById conn uid = query conn
    "SELECT id, name, email FROM users WHERE id = ?"
    (Only uid) 