{-# LANGUAGE OverloadedStrings #-}
module Lib where

import Text.Cipher
import Text.Cipher.Interactive

import Control.Concurrent (threadDelay)
import Control.Monad
import Data.List (isPrefixOf, sortBy, stripPrefix)
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

run key previousId =
    do manager <- newManager tlsManagerSettings
       result <- pollMessages token manager
       putStrLn "polled"
       (newKey, handledMessage) <- handleMaybe result (return (key, Nothing)) $ \response ->
           do let messages = mapMaybe message $ update_result response
                  latest = head messages
                  latestId = message_id latest
                  latestText = T.unpack $ fromMaybe T.empty $ text latest
                  ciphered = processGrouping (Just 4) encrypt latestText
                  request =
                      SendMessageRequest
                      { message_chat_id = T.pack (show chatId)
                      , message_text = T.pack ciphered
                      , message_reply_to_message_id = Nothing
                      -- Just latestId
                      , message_parse_mode = Nothing
                      , message_disable_web_page_preview = Nothing
                      , message_disable_notification = Nothing
                      , message_reply_markup = Nothing
                      }
                      -- sendMessageRequest (T.pack $ show chatId) (T.pack ciphered)
              if "/setkey" `isPrefixOf` latestText
              then let newKey = stripPrefix "/setkey " latestText
                   in do putStrLn $ "Set new key: " ++ show newKey
                         return (fromMaybe key newKey, Just latestId)
              else do unless (previousId == Just latestId) $
                          do void (sendMessage token request manager)
                             putStrLn $ latestText <> " -> " <> ciphered
                      return (key, Just latestId)
       run newKey handledMessage
    where encrypt = fromMaybe "Invalid input, sorry." . playfair key


pollMessages :: Token -> Manager -> IO (Maybe UpdatesResponse)
pollMessages token manager =
    eitherToMaybe <$> getUpdates token (Just (-1)) (Just 1) Nothing manager
    -- Just (-1) :: get latest update
    -- Just 1 :: get only one update

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = const Nothing `either` Just

handleMaybe :: Maybe a -> b -> (a -> b) -> b
handleMaybe = flip (flip . maybe)
