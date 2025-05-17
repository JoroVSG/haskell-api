module Main where

import qualified Api
import qualified Config
import Database.PostgreSQL.Simple

main :: IO ()
main = do
    -- Initialize the database pool
    pool <- Config.initDbPool
    
    -- Create the users table if it doesn't exist
    conn <- connect Config.defaultConnInfo
    _ <- execute_ conn createTableQuery
    close conn
    
    -- Start the web server
    putStrLn "Starting server on http://localhost:3000"
    Api.startApp pool
  where
    createTableQuery = "CREATE TABLE IF NOT EXISTS users (\
        \id SERIAL PRIMARY KEY, \
        \name TEXT NOT NULL, \
        \email TEXT NOT NULL UNIQUE)" 