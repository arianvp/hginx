{-#LANGUAGE OverloadedStrings #-}
module Main where
import Network.Wai.Handler.Warp.SocketActivated
import Network.Wai
import Network.HTTP.Types.Status


settings :: Settings
settings = setPort 3000 defaultSettings

app :: Application
app req respond =
  respond (responseLBS status200 [] "Hello World")

main :: IO ()
main = do
  putStrLn "Starting up"
  runSocketActivated settings app 
