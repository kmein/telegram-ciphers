{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Data.Monoid
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

token = Token $ "bot" <> "252445649:AAH3NQZeVQRvZD-b860REZgvBJ8Jo9M_wPM"

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

pollMessages :: Token -> Manager -> IO (Maybe UpdatesResponse)
pollMessages token manager =
    eitherToMaybe <$> getUpdates token Nothing Nothing Nothing manager

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = const Nothing `either` Just

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe = flip (flip . maybe)
