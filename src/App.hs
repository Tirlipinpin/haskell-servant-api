{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

module App where

import           Control.Monad.Trans.Except
import           Data.Aeson
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.IO

-- * api

type UserApi = "users" :> Get '[JSON] [User]
  :<|> "users" :> Capture "userId" Integer :> Get '[JSON] User
  :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Request_result

userApi :: Proxy UserApi
userApi = Proxy

-- * app

run :: IO ()
run = do
  let port = 3000
      settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings
  runSettings settings =<< mkApp

mkApp :: IO Application
mkApp = return $ serve userApi server

server :: Server UserApi
server = getUsers
  :<|> getUserById
  :<|> createUser

getUsers :: Handler [User]
getUsers = return [exampleUser, exampleUser2, exampleUser3]

getUserById :: Integer -> Handler User
getUserById = \ case
  0 -> return exampleUser
  _ -> throwE err404

createUser :: User -> Handler Request_result
createUser x = return Request_result { msg="", status=200 }

examplesUsers :: [User]
examplesUsers = [exampleUser, exampleUser2, exampleUser3]

exampleUser :: User
exampleUser = User {userId=0, userName="Henri", userEmail="hgaudeaux@hotmail.fr" }

exampleUser2 :: User
exampleUser2 = User {userId=1, userName="John", userEmail="john@gmail.com" }

exampleUser3 :: User
exampleUser3 = User {userId=2, userName="Alexis", userEmail="alexdu98@laposte.net" }

-- * user

data User = User {
  userId :: Integer,
  userName :: String,
  userEmail :: String
} deriving (Eq, Show, Generic)

data Request_result = Request_result {
  msg :: String,
  status :: Integer
} deriving (Eq, Show, Generic)

instance ToJSON User
instance FromJSON User

instance ToJSON Request_result
instance FromJSON Request_result

data a + b = Foo a b

type X = Int + Bool
