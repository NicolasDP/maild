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

import System.Environment (getArgs)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

defaultSMTPServerConfig :: SMTPConfig
defaultSMTPServerConfig =
    SMTPConfig
        25          -- listen on SMTP port
        "localhost" -- domain name of this service
        10          -- number of simultaneous connections authorized
        "/tmp"      -- maildir

main = do
    args <- getArgs
    case args of
        ["port", p, "domain", d, "connections", c, "dir", dir] -> startSMTPServer $ SMTPConfig (read p) d (read c) dir
        ["default"] -> startSMTPServer defaultSMTPServerConfig
        _           -> putStrLn "usage: [port <port> domain <domain> connections <connections>] | default"

startSMTPServer config = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop (smtpMailDir config) smtpChan
    runServerOnPort config smtpChan

recvLoop dir smtpChan = do
    email <- getNextEmail smtpChan
    putStrLn "############################# New Email"
    putStrLn $ show email
    putStrLn "#######################################"
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
