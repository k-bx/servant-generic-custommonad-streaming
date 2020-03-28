{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
import GHC.Generics
import qualified Network.HTTP.Client
import Network.HTTP.Media ((//))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API.Generic ((:-))
import qualified Servant.API.Generic
import Servant.Client
import qualified Servant.Client.Generic
import qualified Servant.Server
import qualified Servant.Server.Generic
import qualified Servant.Types.SourceT
import System.Environment (getArgs)
import UnliftIO

data Env = Env {}

type AppM = ReaderT Env IO

data GZip

instance Accept GZip where
  contentType _ = "application" // "gzip"

instance MimeRender GZip BL.ByteString where
  mimeRender _ val = val

instance MimeUnrender GZip BL.ByteString where
  mimeUnrenderWithType _ "application/gzip" bs = Right bs
  mimeUnrenderWithType _ mt _bs = Left $ "Mime must be application/gzip, but it's: " ++ show mt

data APIG route
  = APIG
      { __ping :: route :- "api" :> "ping" :> Get '[PlainText] String,
        __downloadFile :: route :- "api" :> "download-file" :> Post '[GZip] BL.ByteString,
        __downloadFile2 ::
          route :- "api" :> "download-file2"
            :> StreamPost NoFraming OctetStream (SourceIO B.ByteString)
      }
  deriving (Generic)

serverG :: APIG (Servant.Server.Generic.AsServerT AppM)
serverG = APIG
  { __ping = ping,
    __downloadFile = downloadFile,
    __downloadFile2 = downloadFile2
  }

ping :: AppM String
ping = return "pong"

downloadFile :: AppM BL.ByteString
downloadFile = pure "BYTESTRINGCONTENTS"

downloadFile2 :: AppM (SourceIO B.ByteString)
downloadFile2 = liftIO $ return (Servant.Types.SourceT.source ["BYTESTRINGCONTENTS2"])

nt :: Env -> AppM a -> Servant.Server.Handler a
nt s x = Servant.Server.Handler $ ExceptT $ UnliftIO.try $ runReaderT x s

cliRoutes :: ClientEnv -> APIG (Servant.Client.Generic.AsClientT AppM)
cliRoutes _env =
  undefined -- ?
  -- Servant.Client.Generic.genericClientHoist
  --   (\x -> liftIO (runClientM x env >>= either throwIO return))

cliPing :: ClientEnv -> AppM String
cliPing env = __ping (cliRoutes env)

cliDownloadFile :: ClientEnv -> AppM BL.ByteString
cliDownloadFile env = __downloadFile (cliRoutes env)

cliDownloadFile2 :: ClientEnv -> AppM (SourceIO B.ByteString)
cliDownloadFile2 env = __downloadFile2 (cliRoutes env)

main :: IO ()
main = do
  args <- getArgs
  case args of
    ("server" : _) -> do
      let env = Env
      let waiApp :: Wai.Application
          waiApp =
            Servant.Server.serveWithContext
              (Proxy :: Proxy (Servant.API.Generic.ToServantApi APIG))
              (EmptyContext)
              ( hoistServerWithContext
                  (Proxy :: Proxy (Servant.API.Generic.ToServantApi APIG))
                  (Proxy :: Proxy '[])
                  (nt env)
                  (Servant.Server.Generic.genericServerT serverG)
              )
      Warp.run 6669 waiApp
    ("client" : _) -> do
      let baseUrl' = BaseUrl
            { baseUrlScheme = Http,
              baseUrlHost = "localhost",
              baseUrlPort = 6669,
              baseUrlPath = ""
            }
      mgr <- Network.HTTP.Client.newManager (Network.HTTP.Client.defaultManagerSettings {Network.HTTP.Client.managerResponseTimeout = Network.HTTP.Client.responseTimeoutNone})
      let cliEnv = (mkClientEnv mgr baseUrl')
      let env = Env
      bs <- flip runReaderT env $ cliDownloadFile cliEnv
      putStrLn $ "> testDownloadAndFilter got bs: " <> show (BL.length bs)
    _ -> error "not implemented"
