{-#LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types.Status
import Control.Applicative
import Control.Monad
import Network.Socket
import System.Posix.Process
import System.Posix.Env
import System.Posix.Types
import Foreign.C.Types(CInt(..))

import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class

fdStart :: CInt
fdStart = 3

-- | Return a list of activated sockets, if the program was started with
-- socket activation.  The sockets are in the same order as in
-- the associated @.socket@ file.  The sockets will have their family, type,
-- and status set appropriately.  Returns @Nothing@ in systems without socket activation (or
-- when the program was not socket activated).
getActivatedSockets :: IO (Maybe [Socket])
getActivatedSockets = runMaybeT $ do
    listenPid <- read <$> MaybeT (getEnv "LISTEN_PID")
    listenFDs <- read <$> MaybeT (getEnv "LISTEN_FDS")
    myPid     <- lift getProcessID
    guard $ listenPid == myPid
    mapM makeSocket [fdStart .. fdStart + listenFDs - 1]
  where makeSocket :: CInt -> MaybeT IO Socket
        makeSocket fd = do
          fam  <- socketFamily fd
          typ  <- socketType fd
          stat <- socketStatus fd
          lift $ mkSocket fd fam typ defaultProtocol stat

socketFamily :: CInt -> MaybeT IO Family
socketFamily fd = do
    familyInt <- lift $ c_socket_family fd
    guard $ familyInt >= 0
    return $ unpackFamily familyInt

socketType :: CInt -> MaybeT IO SocketType
socketType fd = do
    typeInt <- lift $ c_socket_type fd
    case typeInt of
        0 -> return NoSocketType
        1 -> return Stream
        2 -> return Datagram
        3 -> return Raw
        4 -> return RDM
        5 -> return SeqPacket
        _ -> mzero

socketStatus :: CInt -> MaybeT IO SocketStatus
socketStatus fd = do
    listeningInt <- lift $ c_socket_listening fd
    case listeningInt of
      0 -> return Bound
      1 -> return Listening
      _ -> mzero

foreign import ccall unsafe "socket_family"
  c_socket_family :: CInt -> IO CInt

foreign import ccall unsafe "socket_type"
  c_socket_type :: CInt -> IO CInt

foreign import ccall unsafe "socket_listening"
  c_socket_listening :: CInt -> IO CInt

settings :: Settings
settings = setPort 3000 defaultSettings

app :: Application
app req respond =
  respond (responseLBS status200 [] "Hello World")

main :: IO ()
main = withSocketsDo $ do
  socks <- getActivatedSockets

  case socks of
    Nothing -> runSettings settings app
    Just socks ->
      runSettingsSocket settings (head socks) app
