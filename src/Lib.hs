{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

getMeExample =
    do manager <- newManager tlsManagerSettings
       res <- getMe token manager
       case res of
         Left e ->
             do putStrLn "Request failed"
                print e
         Right GetMeResponse { user_result = u } ->
             do putStrLn "Request succeeded"
                print $ user_first_name u
    where token = Token $ "bot" <> token

