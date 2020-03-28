{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Aeson
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API
import Servant.API.Generic ((:-))
import Servant.Client
import qualified Servant.Client.Streaming as S
import qualified Servant.Server
import qualified Servant.Server.Generic
import Servant.Types.SourceT (foreach)

data Position
  = Position
      { xCoord :: Int,
        yCoord :: Int
      }
  deriving (Show, Generic)

instance FromJSON Position

newtype HelloMessage = HelloMessage {msg :: String}
  deriving (Show, Generic)

instance FromJSON HelloMessage

data ClientInfo
  = ClientInfo
      { clientName :: String,
        clientEmail :: String,
        clientAge :: Int,
        clientInterestedIn :: [String]
      }
  deriving (Generic)

instance ToJSON ClientInfo

data Email
  = Email
      { from :: String,
        to :: String,
        subject :: String,
        body :: String
      }
  deriving (Show, Generic)

instance FromJSON Email

type API =
  "position" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Position
    :<|> "hello" :> QueryParam "name" String :> Get '[JSON] HelloMessage
    :<|> "marketing" :> ReqBody '[JSON] ClientInfo :> Post '[JSON] Email

position ::
  -- | value for "x"
  Int ->
  -- | value for "y"
  Int ->
  ClientM Position

hello ::
  -- | an optional value for "name"
  Maybe String ->
  ClientM HelloMessage

marketing ::
  -- | value for the request body
  ClientInfo ->
  ClientM Email

api :: Proxy API
api = Proxy

position :<|> hello :<|> marketing = client api

type API' = API :<|> EmptyAPI

api' :: Proxy API'
api' = Proxy

(position' :<|> hello' :<|> marketing') :<|> EmptyClient = client api'

queries :: ClientM (Position, HelloMessage, Email)
queries = do
  pos <- position 10 10
  message <- hello (Just "servant")
  em <- marketing (ClientInfo "Alp" "alp@foo.com" 26 ["haskell", "mathematics"])
  return (pos, message, em)

run :: IO ()
run = do
  manager' <- newManager defaultManagerSettings
  res <- runClientM queries (mkClientEnv manager' (BaseUrl Http "localhost" 8081 ""))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right (pos, message, em) -> do
      print pos
      print message
      print em

type HoistClientAPI = Get '[JSON] Int :<|> Capture "n" Int :> Post '[JSON] Int

hoistClientAPI :: Proxy HoistClientAPI
hoistClientAPI = Proxy

getIntClientM :: ClientM Int

postIntClientM :: Int -> ClientM Int
getIntClientM :<|> postIntClientM = client hoistClientAPI

-- our conversion function has type: forall a. ClientM a -> IO a
-- the result has type:
-- Client IO HoistClientAPI = IO Int :<|> (Int -> IO Int)
getClients :: ClientEnv -> Client IO HoistClientAPI
getClients clientEnv =
  hoistClient
    hoistClientAPI
    ( fmap (either (error . show) id)
        . flip runClientM clientEnv
    )
    (client hoistClientAPI)

type StreamAPI = "positionStream" :> StreamGet NewlineFraming JSON (SourceIO Position)

streamAPI :: Proxy StreamAPI
streamAPI = Proxy

posStream :: S.ClientM (SourceIO Position)
posStream = S.client streamAPI

printSourceIO :: Show a => ClientEnv -> S.ClientM (SourceIO a) -> IO ()
printSourceIO env c = S.withClientM c env $ \e -> case e of
  Left err -> putStrLn $ "Error: " ++ show err
  Right rs -> foreach fail print rs

data APIG route
  = APIG {__ping :: route :- "api" :> "ping" :> Get '[PlainText] String}
  deriving (Generic)

serverG :: APIG (Servant.Server.Generic.AsServerT Servant.Server.Handler)
serverG = APIG
  { __ping = ping
  }

ping :: Servant.Server.Handler String
ping = return "pong"

main :: IO ()
main = do
  putStrLn "hello world"
