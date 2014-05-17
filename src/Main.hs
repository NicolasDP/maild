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
import qualified Network.DNS as DNS

import System.Environment (getArgs, getProgName)
import System.FilePath  (FilePath, (</>))
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter
import System.IO (stdout)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad (when)
import GHC.IO.Handle (hClose)

import Data.List
import Data.Function (on)
import Data.MailStorage
import Data.Maild.Email
import Data.Configurator as C
import Data.Configurator.Types
import Data.DeliveryManager
import qualified Data.ByteString.Char8 as BC

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

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
    l <- require conf "smtp.blacklist"
    return $ SMTPConfig p d c l ms

getDeliveryManagerConfig :: Config -> MailStorage -> IO DeliveryManager
getDeliveryManagerConfig conf ms = do
    d <- require conf "smtp.domain"
    return $ DeliveryManager ms d

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
    updateGlobalLogger rootLoggerName (removeHandler)
    lvl <- lookupDefault "WARNING" conf "log.level"
    sh <- streamHandler stdout (read lvl) >>= \sh -> return $
            setFormatter sh (simpleLogFormatter "[$loggername] $msg")
    updateGlobalLogger rootLoggerName (setLevel (read lvl). addHandler sh)
    mfile <- C.lookup conf "log.file"
    filelvl <- lookupDefault "INFO" conf "log.file-level"
    case mfile of
        Nothing   -> return ()
        Just file -> do
            h <- fileHandler file (read filelvl) >>= \lh -> return $
                    setFormatter lh (simpleLogFormatter "[$loggername][$time] $msg")
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

type ConnectionMap = Map String Int

runServerOnPort :: SMTPConfig -> SMTPChan -> IO ()
runServerOnPort config chan = withSocketsDo $ do
    let port = fromIntegral $ smtpPort config
    socket <- listenOn $ PortNumber port
    connections <- newMVar $ Map.empty
    logInfo $ "start SMTP server on port: " ++ (show $ smtpPort config)
    acceptConnection config connections socket chan

acceptConnection :: SMTPConfig -> MVar ConnectionMap -> Socket -> SMTPChan -> IO ()
acceptConnection config connections sock chan = do
    (handle, peeraddr, _) <- accept sock
    numberOfConnections <- withMVar connections (\c -> return $ Map.lookup peeraddr c)
    size <- withMVar connections (\c -> return $ Map.size c)
    isBlackListed <- checkInBlackList config peeraddr
    if size > smtpMaxClients config || isBlackListed || maybe False (\x -> x > 1) numberOfConnections
        then forkIO $ rejectClient config handle peeraddr
        else do modifyMVar_ connections $ \c -> return $ Map.insertWith (\_ o -> o + 1) peeraddr 0 c
                forkIO $ do
                    acceptClient config handle peeraddr chan
                    modifyMVar_ connections $ \c -> return $ Map.update (\x -> if x == 1 then Nothing else Just (x-1)) peeraddr c
    acceptConnection config connections sock chan

checkInBlackList :: SMTPConfig -> String -> IO Bool
checkInBlackList config addr = do
    list <- (readFile $ smtpBlackList config) >>= \l -> return $ lines l
    return $ isInList list
    where
        isInList :: [String] -> Bool
        isInList []     = False
        isInList (x:xs) = if isSuffixOf x addr then True else isInList xs

------------------------------------------------------------------------------
--                      DeliveryManager: MainLoop                           --
------------------------------------------------------------------------------

-- find the MX server which corresponds to this domain
findMXofDom :: Domain -> IO [(Domain, Int)]
findMXofDom domain = do
    rs <- DNS.makeResolvSeed DNS.defaultResolvConf
    list <- DNS.withResolver rs $ \resolver ->
                DNS.lookupMX resolver (BC.pack domain)
    return $ case list of
        Left  _ -> []
        Right l -> map (\(d, p) -> (BC.unpack d, p)) l

forwardEmail :: DeliveryManager -- ^ configuration
             -> ReversePath     -- ^ send mail From
             -> [[ForwardPath]] -- ^ list of list of recipients (grouped per domain)
             -> FilePath        -- ^ filepath where the data is
             -> IO [[ForwardPath]]
forwardEmail _   _    []       _        = return []
forwardEmail dmc from (to:tos) filepath = do
    -- send the queue:
    ret <- forwardEmail dmc from tos filepath

    -- find the list of MX sorted by priority to use
    lMx <- (findMXofDom (domainpart $ address $ head to))
           >>= \l -> return $ sortBy (compare `on` snd) l
    -- try the forward
    sended <- tryToForwardMail lMx from to
    return $ if sended then ret else (to:ret)
    where
        tryToForwardMail :: [(Domain, Int)] -- list of MX sorted by priority
                         -> ReversePath     -- send the mail from
                         -> [ForwardPath]   -- the list of recipients
                         -> IO Bool         -- does it work ?
        tryToForwardMail []     _   _  = return $ False
        tryToForwardMail (d:ds) fp rps = do
            infoM "DeliveryManager" $ "try to send to domain: " ++ (show d)
            mcon <- smtpOpenConnection (fst d) (PortNumber 25) (currentDomain dmc)
            case mcon of
                Nothing  -> do noticeM "DeliveryManager" $ "can't send to " ++ (show d)
                               tryToForwardMail ds fp rps
                Just con -> do
                    b <- smtpSendEmail con fp rps filepath
                    smtpCloseConnection con
                    return b

tryToSendMail :: DeliveryManager  -- ^ configuration
              -> ReversePath      -- ^ send mail From
              -> [ForwardPath]    -- ^ recipients
              -> FilePath         -- ^ filepath where the data is
              -> IO [ForwardPath] -- ^ unable to send to the returned recipients
tryToSendMail dmc from tos filepath = do
    let ltos = sortPerDomain tos []
    rtos <- forwardEmail dmc from ltos filepath
    return $ concat rtos
    where
        sortPerDomain :: [ForwardPath] -> [[ForwardPath]] -> [[ForwardPath]]
        sortPerDomain []  res = res
        sortPerDomain fps res =
            let (lfps, rfps) = foldr getCommonForwardPath ([], []) fps
            in sortPerDomain rfps (lfps:res)

        getCommonForwardPath :: ForwardPath -> ([ForwardPath], [ForwardPath]) -> ([ForwardPath], [ForwardPath])
        getCommonForwardPath fp ([], l)     = ([fp], l)
        getCommonForwardPath fp (rp:rps, l) =
            if (domainpart $ address fp) == (domainpart $ address rp)
                then (fp:rp:rps,    l)
                else (   rp:rps, fp:l)

tryToForward :: DeliveryManager -> Email -> IO (Maybe Email)
tryToForward dmc email =
    case mailFrom email of
        Nothing   -> return Nothing -- should not fall here
        Just from -> do
            tos <- tryToSendMail dmc from (mailTo email) ((forDeliveryDir $ mailStorageDir dmc) </> (mailData email))
            return $ case tos of
                [] -> Nothing
                _  -> Just $ email { mailTo = tos }

runDeliveryManager :: DeliveryManager -> DeliveryChan -> IO ()
runDeliveryManager dmc dmChan = do
    email <- getNextEmailToDeliver dmChan
    noticeM "DeliveryManager" $ "received new email: " ++ (show $ mailData email)
    -- New email to deliver:
    -- -1- deliver to local users:
    email' <- deliverEmailToLocalRCPT dmc email
    -- -2- forward to distant recipients:
    when (not $ null $ mailTo email') $ do
        remains <- tryToForward dmc email'
        case remains of
           Nothing      -> noticeM "DeliveryManager" "the email is gone"
           Just email'' -> warningM "DeliveryManager" $ "can't send to: " ++ (show $ mailTo email'')
    deleteDataFromDeliveryDir (mailStorageDir dmc) email'
    runDeliveryManager dmc dmChan
