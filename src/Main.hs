-- |
-- Module      : Main
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# Language OverloadedStrings #-}
import Network
import Network.SMTP
import Network.SMTP.Types
import Network.SMTP.Auth

import System.Environment (getArgs, getProgName)
import System.FilePath  (FilePath, (</>))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Data.MailStorage
import Data.Configurator
import Data.Configurator.Types

getSMTPConfigFrom :: Config -> IO SMTPConfig
getSMTPConfigFrom conf = do
    d <- require conf "smtp.domain"
    p <- require conf "smtp.port"
    c <- require conf "smtp.connections"
    dir <- require conf "mailstorage.path"
    mMailStorage <- getMailStorage dir
    mailStorage <- case mMailStorage of
                        Nothing -> initMailStorageDir dir
                        Just ms -> return ms
    return $ SMTPConfig p d c mailStorage

main = do
    args <- getArgs
    name <- getProgName
    case args of
        [configFile] -> do conf <- load [Required configFile]
                           config <- getSMTPConfigFrom conf
                           startSMTPServer config
        _           -> putStrLn $ "usage: " ++ name ++ " <configuration file>"

startSMTPServer config = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop config smtpChan
    runServerOnPort config smtpChan

recvLoop config smtpChan = do
    email <- getNextEmail smtpChan
    -- We received a new email. So, move it from *incoming* directory to
    -- *fordelivery* directory
    fromIncomingToFordelivery (storageDir config) email
    -- now we need to notify the mail manager to require him to deliver the
    -- message to the corresponding mail box or! to forward it to an other
    -- postfix server
    -- TODO: deliverEmail mailManagerChan
    recvLoop config smtpChan

------------------------------------------------------------------------------
--                          SMTP-Server: MainLoop                           --
------------------------------------------------------------------------------

runServerOnPort :: SMTPConfig -> SMTPChan -> IO ()
runServerOnPort config chan = withSocketsDo $ do
    let port = fromIntegral $ smtpPort config
    socket <- listenOn $ PortNumber port
    connections <- newMVar 0
    acceptConnection config connections socket chan

acceptConnection :: SMTPConfig -> MVar Int -> Socket -> SMTPChan -> IO ()
acceptConnection config connections sock chan = do
    (handle, _, _) <- accept sock
    nConnections <- readMVar connections
    if nConnections < smtpMaxClients config
        then do modifyMVar_ connections $ \c -> return $ c + 1
                forkIO $ do
                    acceptClient config handle chan
                    modifyMVar_ connections $ \c -> return $ c - 1
        else do forkIO $ rejectClient config handle
    acceptConnection config connections sock chan
