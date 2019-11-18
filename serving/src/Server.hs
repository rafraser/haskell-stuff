{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Server where

import Servant
import Data.Aeson
import Data.Aeson.Types
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp

data User = User {
    name :: String,
    age :: Int,
    email :: String
} deriving (Eq, Show, Generic)
instance ToJSON User

users_list :: [User]
users_list = [ User "Robert Fraser" 19 "test@example.com"
             ,User "Not Robert" 91 "test2@example.com" ]

type UserAPI1 = "users" :> Get '[JSON] [User]

server1 :: Server UserAPI1
server1 = return users_list

userAPI :: Proxy UserAPI1
userAPI = Proxy

app :: Application
app = serve userAPI server1

main :: IO()
main = run 8080 app