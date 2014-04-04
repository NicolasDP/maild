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
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Data.MailStorage
import Data.Maild.Email
import Data.Configurator as C
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
getDeliveryManagerConfig _ ms = do
    
    return $ DeliveryManager ms

configureDefaultLoggingSystem :: IO ()
configureDefaultLoggingSystem = do
    -- set the default log level to WARNING for every component
    updateGlobalLogger rootLoggerName (setLevel WARNING)

logMessage = logM "maild"

logDebug     = logMessage DEBUG
logInfo      = logMessage INFO
logNotice    = logMessage NOTICE
logWarning   = logMessage WARNING
logError     = logMessage ERROR
logCritical  = logMessage CRITICAL
logAlert     = logMessage ALERT
logEmergency = logMessage EMERGENCY

configureLoggingSystem :: Config -> IO ()
configureLoggingSystem conf = do
    lvl <- lookupDefault "WARNING" conf "log.level"
    updateGlobalLogger rootLoggerName (setLevel $ read lvl)
    mfile <- C.lookup conf "log.file"
    filelvl <- lookupDefault "INFO" conf "log.file-level"
    case mfile of
        Nothing   -> return ()
        Just file -> do
            h <- fileHandler file (read filelvl) >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "[$time : $loggername : $prio] $msg")
            updateGlobalLogger rootLoggerName (addHandler h)

defaultMain :: FilePath -> IO ()
defaultMain configFile = do
    -- parse the configuration files
    logNotice $ "load configuration file: " ++ (show configFile)
    conf <- load [Required configFile]
    configureLoggingSystem conf
    getMap conf >>= \m -> logDebug $ "configuration: " ++ (show m)
    -- load the Mail Storage configuration
    ms <- getMailStorageConfig conf
    logDebug $ "MailStorageConfig: " ++ (show ms)
    -- load the SMTP configuration
    config <- getSMTPConfigFrom conf ms
    logDebug $ "SMTPConfig: " ++ (show config)
    -- load the delivery manager configuration
    dmConfig <- getDeliveryManagerConfig conf ms
    logDebug $ "DeliveryManagerConfig: " ++ (show dmConfig)

    -- create a DeliveryChan
    dmChan <- newDeliveryChan
    -- launch the DeliveryManager
    logInfo "start delivery manager"
    forkIO $ runDeliveryManager dmConfig dmChan
    -- Now launch the SMTP Server
    startSMTPServer config dmChan

main :: IO ()
main = do
    configureDefaultLoggingSystem
    args <- getArgs
    name <- getProgName
    case args of
        [configFile] -> defaultMain configFile
        _            -> putStrLn $ "usage: " ++ name ++ " <configuration file>"

startSMTPServer config dmChan = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop config dmChan smtpChan
    runServerOnPort config smtpChan

recvLoop config dmChan smtpChan = do
    email <- getNextEmail smtpChan
    -- We received a new email. So, move it from *incoming* directory to
    -- *fordelivery* directory
    fromIncomingToFordelivery (storageDir config) email
    -- forward the email to the delivery manager
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
    logInfo $ "start SMTP server on port: " ++ (show $ smtpPort config)
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
    noticeM "DeliveryManager" $ "received new email: " ++ (show email)
    -- New email to deliver:
    -- -1- deliver to local users:
    email' <- deliverEmailToLocalRCPT dmc email
    -- -2- forward to distant users:
    case mailTo email' of
        [] -> deleteDataFromDeliveryDir (mailStorageDir dmc) email'
        l  -> errorM "DeliveryManager" $ "cannot forward email to: " ++ (show l)
    runDeliveryManager dmc dmChan
