-- |
-- Module      : Network.SMTP
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP
    ( -- * SMTP Server
      -- ** Notifications
      SMTPChan
    , newSMTPChan
    , getNextEmail
      -- ** Configuration
    , SMTPConfig(..)
      -- ** Main method
    , acceptClient
    , rejectClient
      -- ** Helpers
    , respond
    ) where

import Network.SMTP.Types
import Network.SMTP.Auth

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.State

import qualified Data.ByteString.Char8     as BC

import System.IO

data SMTPConfig = SMTPConfig
    { smtpPort       :: Int
    , smtpDomainName :: String
    , smtpMaxClients :: Int
    } deriving (Show, Read, Eq)

------------------------------------------------------------------------------
--                               SMTPChan                                   --
------------------------------------------------------------------------------

-- | A channel used to be notified each time Email has been received
type SMTPChan = TChan Email

-- | a helper to create a SMTPChan
newSMTPChan :: IO (SMTPChan)
newSMTPChan = atomically (newTChan)

-- | internal command: publish the received email
publishEmail :: SMTPChan -> Email -> IO ()
publishEmail chan = atomically . writeTChan chan

-- | a helper to wait and then get an Email.
getNextEmail :: SMTPChan -> IO Email
getNextEmail = atomically . readTChan

------------------------------------------------------------------------------
--                               Handling Clients                           --
------------------------------------------------------------------------------

-- | Accept a client, and communicate with him to get an email.
-- -1- send code 221 ;
-- -2- read/answer any commands
acceptClient :: SMTPConfig -> Handle -> SMTPChan -> IO ()
acceptClient config h chan = do
    respond220 h (smtpDomainName config)
    email <- execStateT (commandProcessor config h) (Email "" "" "" BC.empty)
    publishEmail chan email

-- | Reject a client:
-- as described in RFC5321:
-- -1- send code 554 ;
-- -2- loop until client quit but always answer him code
--     503 for any other command/string/input received from the handler.
rejectClient :: SMTPConfig -> Handle -> IO ()
rejectClient config h = do
    respond554 h
    loopRejectClient h
    where
        loopRejectClient h = do
            buff <- liftIO $ BC.hGetLine h
            case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
                Right QUIT -> closeHandle config h
                _          -> respond503 h >> loopRejectClient h

------------------------------------------------------------------------------
--                            Responding to Client                          --
------------------------------------------------------------------------------

respond :: Handle -> Int -> String -> IO ()
respond h code msg = BC.hPutStrLn h $ BC.pack $ (show code) ++ " " ++ msg

-- Standard answer messages:
respond500 h        = respond h 500 "Syntax error, command not recognized"
respond501 h        = respond h 501 "Syntax error in parameters"
respond502 h        = respond h 502 "Command not implemented"
respond503 h        = respond h 503 "Bad sequence of commands"
respond504 h        = respond h 504 "Command parameter not implemented"

respond211 h status = respond h 211 $ "System status: " ++ status
respond214 h help   = respond h 214 $ "Help: " ++ help

respond220 h domain = respond h 220 $ domain ++ " Service ready"
respond221 h domain = respond h 221 $ domain ++ " Service closing transmission channel" 
respond421 h domain = respond h 421 $ domain ++ " Service not available, closing transmission channel"

respond250 h        = respond h 250 "Action completed"
respond251 h fwd    = respond h 251 $ "User not local, will forward to: <" ++ fwd ++ ">"
respond252 h        = respond h 252 "Cannot VRFY user, but will accept and attempt delivery"

respond455 h        = respond h 455 "Server unable to accomodate parameters"
respond555 h        = respond h 555 "MAIL FROM/RCPT TO not recognized or not implemented"
respond450 h        = respond h 450 "Requested mail action not taken: mailbox unavailable"
respond550 h        = respond h 550 "Requested action not taken: mailbox unavailable"
respond451 h        = respond h 451 "Requested action aborted: error in processing"
respond551 h fwd    = respond h 551 $ "User not local: please try <" ++ fwd ++ ">"
respond452 h        = respond h 452 "Requested action not taken: insufficient system storage"
respond552 h        = respond h 552 "Requested mail action not aborted: exceeded storage allocation"
respond553 h        = respond h 553 "Requested action not taken: mailbox name not allowed"

respond354 h        = respond h 354 "Start mail input; end with <CRLF>.<CRLF>"
respond554 h        = respond h 554 "Transaction failed or no SMTP Service here"

closeHandle config h = respond221 h (smtpDomainName config) >> hClose h


type EmailS a = StateT Email IO a

commandHandleHELO h client = do
    modify (\s -> s { mailClient = client })
    liftIO $ respond250 h

commandHandleMAIL h from = do
    modify (\s -> s { mailFrom = from })
    liftIO $ respond250 h

commandHandleRCPT h to = do
    -- Check the destination and the relay
    modify (\s -> s { mailTo = to })
    liftIO $ respond250 h

commandHandleDATA h = do
    liftIO $ respond354 h -- say: go on! Don't be shy, give me your data
    d <- liftIO $ readMailData h BC.empty
    modify (\s -> s { mailData = d})
    liftIO $ respond250 h
    where
        readMailData h t = do
            buff <- liftIO $ BC.hGetLine h
            if buff == BC.pack (".\r")
                then return t
                else readMailData h $ BC.concat [t, buff, BC.pack "\n"]

commandProcessor :: SMTPConfig -> Handle -> EmailS ()
commandProcessor config h = do
    buff <- liftIO $ BC.hGetLine h
    case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
        Right (HELO client) -> commandHandleHELO h client >> commandProcessor config h
        Right (MAIL from)   -> commandHandleMAIL h from   >> commandProcessor config h
        Right (RCPT to)     -> commandHandleRCPT h to     >> commandProcessor config h
        Right DATA          -> commandHandleDATA h        >> commandProcessor config h
        Right QUIT          -> liftIO $ closeHandle config h
        Right (INVALCMD _)  -> (liftIO $ respond500 h)    >> commandProcessor config h
        Right _             -> (liftIO $ respond502 h)    >> commandProcessor config h
        Left _              -> (liftIO $ respond500 h)    >> (liftIO $ closeHandle config h)
