-- |
-- Module      : Main
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
import Network
import Network.SMTP
import Network.SMTP.Types
import Network.SMTP.Auth
import Data.MailStorage

import System.Environment (getArgs)
import System.FilePath  (FilePath, (</>))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

defaultSMTPServerConfig :: SMTPConfig
defaultSMTPServerConfig =
    SMTPConfig
        25          -- listen on SMTP port
        "localhost" -- domain name of this service
        10          -- number of simultaneous connections authorized
        "/tmp"
        (\_ _ _ -> return True)  -- Accept all the connections
        undefined   -- Mandatory: define it!

identifySMTPUser :: SMTPConfig
                 -> String
                 -> Maybe String
                 -> IO Bool
identifySMTPUser config login passwd = undefined
{-
    muser <- getUser (smtpMailDir config) login
    case muser of
        Nothing   -> return False
        Just user -> checkUserPassword user
    where
        checkUserPassword :: User -> IO Bool
        checkUserPassword _ = return True -- TODO: check the User's digest file
-}

verifySMTPUser :: SMTPConfig -> String -> IO [String]
verifySMTPUser config name = undefined
{-
    getUser (smtpMailDir config) name >>= \muser ->
        case muser of
            Just user -> return [(userLogin user) ++ "@" ++ (smtpDomainName config)]
            Nothing   -> return []
-}

main = do
    args <- getArgs
    case args of
        ["port", p, "domain", d, "connections", c, "dir", dir] -> do
                       isMD <- isMailStorageDir dir
                       if not isMD then initMailStorageDir dir else return ()
                       startSMTPServer $ SMTPConfig
                                            (read p)
                                            d
                                            (read c)
                                            dir
                                            identifySMTPUser
                                            verifySMTPUser
        ["default"] -> startSMTPServer defaultSMTPServerConfig
        _           -> putStrLn "usage: [port <port> domain <domain> connections <connections> maildir <dir>] | default"

startSMTPServer config = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop (mailStorageDir config) smtpChan
    runServerOnPort config smtpChan

recvLoop dir smtpChan = do
    email <- getNextEmail smtpChan
    -- We received a new email. So, move it from *incoming* directory to
    -- *fordelivery* directory
    fromIncomingToFordelivery dir $ mailData email
    -- now we need to notify the mail manager to require him to deliver the
    -- message to the corresponding mail box or! to forward it to an other
    -- postfix server
    -- TODO: deliverEmail mailManagerChan
    recvLoop dir smtpChan

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
