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

verifySMTPUser :: SMTPConfig -> String -> IO [MailUser]
verifySMTPUser config name = return $ findMailUsers name $ storageDir config

expandSMTPUser :: SMTPConfig -> String -> IO [MailUser]
expandSMTPUser config name = undefined

main = do
    args <- getArgs
    case args of
        ["port", p, "domain", d, "connections", c, "dir", dir] -> do
                       mMailStorage <- getMailStorage dir
                       mailStorage <- case mMailStorage of
                                         Nothing -> initMailStorageDir dir
                                         Just ms -> return ms
                       startSMTPServer $ SMTPConfig
                                            (read p)
                                            d
                                            (read c)
                                            mailStorage
                                            verifySMTPUser
                                            expandSMTPUser
        _           -> putStrLn "usage: [port <port> domain <domain> connections <connections> maildir <dir>] | default"

startSMTPServer config = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop (storageDir config) smtpChan
    runServerOnPort config smtpChan

recvLoop storageConfig smtpChan = do
    email <- getNextEmail smtpChan
    -- We received a new email. So, move it from *incoming* directory to
    -- *fordelivery* directory
    fromIncomingToFordelivery storageConfig $ mailData email
    -- now we need to notify the mail manager to require him to deliver the
    -- message to the corresponding mail box or! to forward it to an other
    -- postfix server
    -- TODO: deliverEmail mailManagerChan
    recvLoop storageConfig smtpChan

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
