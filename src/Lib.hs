{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Text.Cipher
import Text.Cipher.Interactive

import Control.Monad
import Data.List (sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Text as T
import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Web.Telegram.API.Bot

someFunc :: IO ()
someFunc = putStrLn "someFunc"

token = Token $ "bot" <> "252445649:AAH3NQZeVQRvZD-b860REZgvBJ8Jo9M_wPM"
chatId = 18980945

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

run =
    do manager <- newManager tlsManagerSettings
       result <- pollMessages token manager
       putStrLn "polled message"
       handleMaybe result (return ()) $ \response ->
           do let messages = getMessages response
                  latest = head messages
                  encrypt = fromMaybe [] . playfair "crybaby"
                  ciphered = processGrouping (Just 4) encrypt latest
                  request = sendMessageRequest (T.pack $ show chatId) (T.pack ciphered)
              print latest
              () <$ sendMessage token request manager
              putStrLn "sent"

pollMessages :: Token -> Manager -> IO (Maybe UpdatesResponse)
pollMessages token manager =
    eitherToMaybe <$> getUpdates token Nothing Nothing Nothing manager

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = const Nothing `either` Just

getMessages :: UpdatesResponse -> [String]
getMessages = map T.unpack . mapMaybe text . sortBy (comparing $ negate . date) . mapMaybe message . update_result

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe = flip (flip . maybe)
