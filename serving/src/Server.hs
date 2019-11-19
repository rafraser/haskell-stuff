{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors

-- Data type for the Users
data User = User {
    name :: String,
    age :: Int,
    email :: String
} deriving (Eq, Show, Generic)
instance ToJSON User

-- List of two users
users_list :: [User]
users_list = [ User "Robert Fraser" 19 "test@example.com"
             ,User "Not Robert" 91 "test2@example.com" ]
     
-- Create the API types     
type MainAPI = UserAPI :<|> StaticAPI
type StaticAPI = Raw
type UserAPI = "users" :> Get '[JSON] [User]

-- Register the static server
staticServer :: Server StaticAPI
staticServer = serveDirectoryWebApp "public/"

-- Register the users server
userServer :: Server UserAPI
userServer = return users_list

-- ?
api :: Proxy MainAPI
api = Proxy

-- Setup an application which will serve our API with the servers
app :: Application
app = simpleCors (serve api $ userServer :<|> staticServer)

-- Run the application on port 8080
main :: IO()
main = run 8080 app