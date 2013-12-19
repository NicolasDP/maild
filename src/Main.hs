-- |
-- Module      : Main
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--

import Network.SMTP
import Network.SMTP.Types
import Network.SMTP.Auth

import Control.Concurrent (forkIO)

main = do
    smtpChan <- newSMTPChan 
    forkIO $ recvLoop smtpChan
    runServerOnPort (SMTPConfig 8080 "localhost" 1) smtpChan

recvLoop smtpChan = do
    email <- getNextEmail smtpChan
    putStrLn "############################# New Email"
    putStrLn $ show email
    putStrLn "#######################################"
    recvLoop smtpChan
