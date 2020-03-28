{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Reader
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

data Env = Env {}

type AppM = ReaderT Env IO

data APIG route
  = APIG {__ping :: route :- "api" :> "ping" :> Get '[PlainText] String}
  deriving (Generic)

serverG :: APIG (Servant.Server.Generic.AsServerT AppM)
serverG = APIG
  { __ping = ping
  }

ping :: AppM String
ping = return "pong"

main :: IO ()
main = do
  putStrLn "hello world"
