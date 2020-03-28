{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import Data.Proxy
import GHC.Generics
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.Media ((//))
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import Servant
import Servant.API
import Servant.API.Generic ((:-))
import qualified Servant.API.Generic
import Servant.Client
import qualified Servant.Client.Streaming as S
import qualified Servant.Server
import Servant.Server.Experimental.Auth (AuthHandler)
import qualified Servant.Server.Generic
import Servant.Types.SourceT (foreach)
import System.Environment (getArgs)
import qualified UnliftIO

data Env = Env {}

type AppM = ReaderT Env IO

data GZip

instance Accept GZip where
  contentType _ = "application" // "gzip"

instance MimeRender GZip BL.ByteString where
  mimeRender _ val = val

instance MimeUnrender GZip BL.ByteString where
  mimeUnrenderWithType _ "application/gzip" _bs = Right ""
  mimeUnrenderWithType _ mt _bs = Left $ "Mime must be application/gzip, but it's: " ++ show mt

data APIG route
  = APIG
      { __ping :: route :- "api" :> "ping" :> Get '[PlainText] String,
        __downloadFile :: route :- "api" :> "download-file" :> Post '[GZip] BL.ByteString,
        __downloadFile2 :: route :- "api" :> "download-file" :> Post '[GZip] BL.ByteString
      }
  deriving (Generic)

serverG :: APIG (Servant.Server.Generic.AsServerT AppM)
serverG = APIG
  { __ping = ping,
    __downloadFile = downloadFile,
    __downloadFile2 = undefined
  }

ping :: AppM String
ping = return "pong"

downloadFile :: AppM BL.ByteString
downloadFile = pure "BYTESTRINGCONTENTS"

nt :: Env -> AppM a -> Servant.Server.Handler a
nt s x = Servant.Server.Handler $ ExceptT $ UnliftIO.try $ runReaderT x s

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
      undefined
    _ -> error "not implemented"
