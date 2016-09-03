{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib (run) where

import Text.Cipher.Interactive

import Control.Monad
import Control.Monad.Reader

import Data.Char (toLower)
import Data.List (nub, sortBy, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import qualified Data.Text as T

import Network.HTTP.Client (newManager, Manager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import Safe (readMay)

import System.Log.Logger

import Web.Telegram.API.Bot

logDebug :: (MonadIO m) => String -> m ()
logDebug = liftIO . debugM "telegram-ciphers"

getChatIds :: Token -> IO [Int]
getChatIds tok =
    do man <- newManager tlsManagerSettings
       res <- getUpdates tok Nothing Nothing Nothing man
       case res of
         Left e -> fail $ show e
         Right updates ->
             return . nub . map (chat_id . chat) $
             mapMaybe message (update_result updates)

initialOptions :: CiphersOptions
initialOptions = CiphersOptions (Playfair "crybaby") Encrypt (Just 4)

run :: T.Text -> IO ()
run tokenText =
    do let token = Token $ "bot" <> tokenText
       manager <- newManager tlsManagerSettings
       updateGlobalLogger "telegram-ciphers" $ setLevel DEBUG
       mainLoop initialOptions Nothing `runReaderT` (manager, token)

mainLoop :: CiphersOptions -> Maybe Int -> ReaderT (Manager, Token) IO ()
mainLoop opts previousId =
    do (manager, token) <- ask
       response <- liftIO $ getUpdates token (Just (-1)) (Just 1) Nothing manager
       case response of
         Left e ->
             do logDebug "polling failed"
                mainLoop opts previousId
         Right response ->
             let messages = mapMaybe message $ update_result response
                 latest = head messages
                 latestId = message_id latest
                 latestText = T.unpack $ fromMaybe T.empty $ text latest
             in do logDebug "polled (called getUpdates)"
                   handleCommand latestText "/setcipher" $ \case
                       ["atbash"] ->
                           do logDebug "changed cipher to atbash"
                              mainLoop opts { cipher = Atbash } Nothing
                       ["autokey", key] ->
                           do logDebug "changed cipher to autokey"
                              mainLoop opts { cipher = Autokey key } Nothing
                       ["caesar", shift] | Just n <- readMay shift ->
                           do logDebug "changed cipher to caesar"
                              mainLoop opts { cipher = Caesar n } Nothing
                       ["polybius"] ->
                           do logDebug "changed cipher to polybius"
                              mainLoop opts { cipher = Polybius } Nothing
                       ["playfair", key] ->
                           do logDebug "changed cipher to playfair"
                              mainLoop opts { cipher = Playfair key } Nothing
                       ["scytale", shift] | Just n <- readMay shift ->
                           do logDebug "changed cipher to scytale"
                              mainLoop opts { cipher = Scytale n } Nothing
                       ["substitution", key] ->
                           do logDebug "changed cipher to substitution"
                              mainLoop opts { cipher = Substitution key } Nothing
                       ["vigenere", key] ->
                           do logDebug "changed cipher to vigenere"
                              mainLoop opts { cipher = Vigenere key } Nothing
                       _ -> mainLoop opts previousId
                   handleCommand latestText "/setdirection" $ \case
                       ["encrypt"] ->
                           do logDebug "changed direction to encrypt"
                              mainLoop opts { direction = Encrypt } Nothing
                       ["decrypt"] ->
                           do logDebug "changed direction to decrypt"
                              mainLoop opts { direction = Decrypt } Nothing
                       _ -> mainLoop opts previousId
                   handleCommand latestText "/setgrouping" $ \case
                       [] ->
                           do logDebug "disabled grouping"
                              mainLoop opts { grouped = Nothing } Nothing
                       [g] | Just n <- readMay g ->
                           do logDebug $ "set grouping to " <> g
                              mainLoop opts { grouped = Just n } Nothing
                       _ -> mainLoop opts $ Just latestId
                   unless (previousId == Just latestId) $
                       do let ciphered = processGrouping (grouped opts) encrypt latestText
                              request =
                                  emptyMessage
                                  { message_chat_id = T.pack (show $ chat_id $ chat latest)
                                  , message_text = T.pack ciphered
                                  , message_reply_to_message_id = Just latestId
                                  }
                          void $ liftIO $ sendMessage token request manager
                          logDebug $ latestText <> " -> " <> ciphered
                   mainLoop opts (Just latestId)
    where encrypt = toFunction opts

handleCommand :: (Monad m) => String -> String -> ([String] -> m a) -> m ()
handleCommand text cmd argf = mapM_ argf arg
    where arg = (words . map toLower) <$> stripPrefix cmd text

emptyMessage :: SendMessageRequest
emptyMessage =
    SendMessageRequest
    { message_chat_id = T.empty
    , message_text = T.empty
    , message_reply_to_message_id = Nothing
    , message_parse_mode = Nothing
    , message_disable_web_page_preview = Nothing
    , message_disable_notification = Nothing
    , message_reply_markup = Nothing
    }
