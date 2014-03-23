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

import DeliveryManager

import System.Environment (getArgs, getProgName)
import System.FilePath  (FilePath, (</>))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Data.MailStorage
import Data.Configurator
import Data.Configurator.Types

getMailStorageConfig :: Config -> IO MailStorage
getMailStorageConfig conf = do
    dir <- require conf "mailstorage.path"
    mMailStorage <- getMailStorage dir
    case mMailStorage of
         Nothing -> initMailStorageDir dir
         Just ms -> return ms

getSMTPConfigFrom :: Config -> MailStorage -> IO SMTPConfig
getSMTPConfigFrom conf ms = do
    d <- require conf "smtp.domain"
    p <- require conf "smtp.port"
    c <- require conf "smtp.connections"
    return $ SMTPConfig p d c ms

getDeliveryManagerConfig :: Config -> MailStorage -> IO DeliveryManager
getDeliveryManagerConfig _ ms = return $ DeliveryManager ms

main = do
    args <- getArgs
    name <- getProgName
    case args of
        [configFile] -> do conf <- load [Required configFile]
                           ms <- getMailStorageConfig conf
                           config <- getSMTPConfigFrom conf ms
                           dmConfig <- getDeliveryManagerConfig conf ms
                           dmChan <- newDeliveryChan
                           forkIO $ runDeliveryManager dmConfig dmChan
                           startSMTPServer config dmChan
        _           -> putStrLn $ "usage: " ++ name ++ " <configuration file>"

startSMTPServer config dmChan = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop config dmChan smtpChan
    runServerOnPort config smtpChan

recvLoop config dmChan smtpChan = do
    email <- getNextEmail smtpChan
    -- We received a new email. So, move it from *incoming* directory to
    -- *fordelivery* directory
    fromIncomingToFordelivery (storageDir config) email
    -- now we need to notify the mail manager to require him to deliver the
    -- message to the corresponding mail box or! to forward it to an other
    -- postfix server
    deliverEmail dmChan email
    recvLoop config dmChan smtpChan

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

------------------------------------------------------------------------------
--                      DeliveryManager: MainLoop                           --
------------------------------------------------------------------------------

runDeliveryManager dmc dmChan = do
    email <- getNextEmailToDeliver dmChan
    -- New email to deliver:
    -- -1- deliver to local users:
    email' <- deliverEmailToLocalRCPT dmc email
    -- -2- forward to distant users:
    case mailTo email' of
        [] -> deleteDataFromDeliveryDir (mailStorageDir dmc) email'
        l  -> putStrLn "TODO: forward it to distant users"
    runDeliveryManager dmc dmChan
