-- |
-- Module      : Network.SMTP.Client
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Client
    ( -- * Main functions
      smtpOpenConnection
    , smtpSendCommand
    , smtpCloseConnection
      -- * Helper
    , smtpSendEmail
    ) where

import Data.Maild.Email
import qualified Data.Map.Strict as Map (empty)

import Network.SMTP.Types
import Network.SMTP.Parser
import Network.SMTP.Connection

import Control.Concurrent.STM (atomically, newTChan)

import Network (PortID, connectTo)

import System.FilePath  (FilePath, (</>))
import System.Log.Logger (Priority(..))

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC

------------------------------------------------------------------------------
--                               SMTPClient                                 --
------------------------------------------------------------------------------

type ClientConnection = SMTPConnection ()

-- | Open a connection to the given Domain at the given port
smtpOpenConnection :: Domain -- ^ the server domain
                   -> PortID -- ^ the server port
                   -> Domain -- ^ the client domain
                   -> IO (Maybe ClientConnection)
smtpOpenConnection d port dom = do
    h <- connectTo d port
    con <- handleToSMTPConnection h d "client" =<< atomically newTChan 
    res <- smtpInitConnection con dom
    if res
        then return $ Just con
        else do logMessage con WARNING $ "can't connect to domain " ++ d
                hcClose con
                return Nothing
  where
    -- After opening a socket, here is the way to know if the server is
    -- providing a SMTP service (still need to parse the return value)
    smtpInitConnection :: ClientConnection -- the connection
                       -> Domain            -- the client domain (for the HELO command)
                       -> IO Bool
    smtpInitConnection con domain = do
        -- when client connects to server, server immediately returns a code.
        respl <- smtpReadResponses con
        if not $ checkResponses respl RC220ServiceReady
            then return False
            else smtpTryCommand con (HELO domain) RC250Ok

-- | Close the given SMTP Connection
smtpCloseConnection :: ClientConnection
                    -> IO ()
smtpCloseConnection con = do
    smtpSendCommand con QUIT
    logMessage con DEBUG "close connection"
    hcClose con

-- | Send an email (will send all the needed command in order to send the
-- message)
smtpSendEmail :: ClientConnection -- ^ the connection
              -> ReversePath       -- ^ the sender
              -> [ForwardPath]     -- ^ recipients
              -> FilePath          -- ^ the file's content
              -> IO Bool
smtpSendEmail con from to filepath = do
    smtpTryCommand con (MAIL from Map.empty) RC250Ok
    mapM (\t -> smtpTryCommand con (RCPT t Map.empty) RC250Ok) to
    smtpTryCommand con DATA RC354StartMailInput
    sendTheData con
    where
        sendTheData :: ClientConnection -> IO Bool
        sendTheData con = do
            content <- BC.readFile filepath
            hcPut con content
            hcPut con $ BC.pack "\r\n.\r\n"
            l <- smtpReadResponses con
            return $ case l of
                [Right (Response (Right RC250Ok) _ _)] -> True
                _                                      -> False

-- Try to send a command, if the server's response does not correspond to the
-- expected ResponseCode: return False
smtpTryCommand :: ClientConnection -- the connection
               -> Command           -- the command
               -> ResponseCode      -- the expected return code
               -> IO Bool
smtpTryCommand con cmd expected = do
    respList <- smtpSendCommand con cmd
    return $ checkResponses respList expected

-- Check a list of response and verify the return value is the one expected
checkResponses :: [Either String Response]
               -> ResponseCode
               -> Bool
checkResponses [] _ = error "No response... MUST not fall here"
checkResponses l  c =
  case l of
    [resp]    -> checkResponse c resp -- the only or last response
    (resp:rs) -> if checkResponse c resp then checkResponses rs c else False
  where
    checkResponse :: ResponseCode -> Either String Response -> Bool
    checkResponse _        (Left _)  = False
    checkResponse expected (Right r) =
      case (code r) of
          Left  _ -> False
          Right c -> c == expected

-- | Send a command to the given connection
--
-- Miss: AUTH and STARTTLS
smtpSendCommand :: ClientConnection
                -> Command
                -> IO [Either String Response]
smtpSendCommand con cmd =
  case cmd of
      (HELO domain) -> smtpSendString $ "HELO " ++ domain
      (EHLO domain) -> smtpSendString $ "EHLO " ++ domain
      (MAIL from _) -> smtpSendString $ "MAIL FROM:" ++ (maybe "<>" showPath from)
      (RCPT to   _) -> smtpSendString $ "RCPT TO:" ++ (showPath to)
      DATA          -> smtpSendString "DATA"
      RSET          -> smtpSendString "RSET"
      QUIT          -> smtpSendString "QUIT"
      (NOOP s)      -> smtpSendString $ "NOOP" ++ (maybe [] (\ss -> " " ++ ss) s)
      (HELP s)      -> smtpSendString $ "HELP" ++ (maybe [] (\ss -> " " ++ ss) s)
      (EXPN arg)    -> smtpSendString $ "EXPN " ++ arg
      (VRFY arg)    -> smtpSendString $ "VRFY " ++ arg
      _             -> return $ [Left $ "not supported yet: " ++ (show cmd)]
  where
    smtpSendString :: String
                   -> IO [Either String Response]
    smtpSendString s = do
        let msg = s ++ "\r\n"
        hcPut con $ BC.pack msg
        logMessage con DEBUG $ show msg
        smtpReadResponses con

-- Method to read and parse a response
smtpReadResponses :: ClientConnection
                  -> IO [Either String Response]
smtpReadResponses con = do
    -- mbuff <- timeout (5 * 60 * 1000000) $ hcGetSome con 2048
    mbuff <- hcGetSome con 2048 >>= \s -> return $ Just s
    case mbuff of
        Nothing   -> do logMessage con DEBUG "connection timenout"
                        return [Left "server didn't respond in less than 5min"]
        Just buff -> do
            logMessage con DEBUG $ "read: " ++ (show buff)
            case parseResponseByteString buff of
                Left err -> return $ [Left err]
                Right r  -> do
                    if endOfResponse r
                        then return [Right r]
                        else smtpReadResponses con >>= \rs -> return $ (Right r):rs
