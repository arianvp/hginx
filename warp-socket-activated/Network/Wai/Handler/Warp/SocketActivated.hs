module Network.Wai.Handler.Warp.SocketActivated 
( module WarpTLS
, module Warp
, runSocketActivated
, runSocketActivatedTLS
) where
import Debug.Trace
import Data.Maybe
import Control.Monad
import Control.Monad.Extra

import System.Systemd.Daemon
import System.Posix.Types
import Network.Socket
import Network.Wai
import Network.Wai.Handler.WarpTLS as WarpTLS
import Network.Wai.Handler.Warp as Warp
import Network.Wai.Handler.Warp.Internal as Internal


runSocketActivated :: Warp.Settings
    -> Application
    -> IO ()
runSocketActivated settings app =
  run'
    settings
    (Warp.runSettings settings app)
    (\socket -> Warp.runSettingsSocket settings socket app)

run' :: Warp.Settings
     -> IO ()
     -> (Socket -> IO ())
     -> IO ()
run' settings f g = do
  fds <- getActivatedSockets
  let port = fromIntegral (Warp.getPort settings)
  case fds of
    Nothing -> f
    Just fds -> do
      fd <- findM (\s -> (==) <$> socketPort s <*> pure (fromIntegral port)) fds
      case fd of
        Just fd -> g fd
        Nothing -> f

runSocketActivatedTLS :: WarpTLS.TLSSettings
       -> Warp.Settings
       -> Application
       -> IO ()
runSocketActivatedTLS tlsSettings settings app =
  run'
    settings
    (WarpTLS.runTLS tlsSettings settings app)
    (\socket -> WarpTLS.runTLSSocket tlsSettings settings socket app)
      
      

