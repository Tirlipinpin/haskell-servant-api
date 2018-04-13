
module AppSpec where

import           Control.Exception (throwIO)
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

import           App hiding (getUsers)

getUsers :: ClientM [User]
getUser :: Integer -> ClientM User
setUser :: User -> ClientM Request_result
getUsers :<|> getUser :<|> setUser = client userApi

spec :: Spec
spec = do
  describe "/user" $ do
    withClient mkApp $ do
      it "lists an example user" $ \ env -> do
        try env getUsers `shouldReturn` [User {userId = 0, userName = "Henri", userEmail = "hgaudeaux@hotmail.fr"},User {userId = 1, userName = "John", userEmail = "john@gmail.com"},User {userId = 2, userName = "Alexis", userEmail = "alexdu98@laposte.net"}]

      it "allows to show users by id" $ \ env -> do
        try env (getUser 0) `shouldReturn` User {userId=0, userName="Henri", userEmail="hgaudeaux@hotmail.fr" }

      it "throws a 404 for missing users" $ \ env -> do
        try env (getUser 42) `shouldThrow` (\ e -> responseStatus e == notFound404)

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
  beforeAll (newManager defaultManagerSettings) $ do
    flip aroundWith innerSpec $ \ action -> \ manager -> do
      testWithApplication x $ \ port -> do
        let baseUrl = BaseUrl Http "localhost" port ""
        action (ClientEnv manager baseUrl)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<<
  runClientM action clientEnv
