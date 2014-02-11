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
      -- * SMTP Client
    , SMTPConnection
    , smtpInitConnection
    , smtpSendCommand
    , smtpSendString
    , smtpReadResponses
    ) where

import Network.SMTP.Types
import Network.SMTP.Auth

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.State

import qualified Data.ByteString.Char8     as BC

import System.IO
import System.FilePath  (FilePath, (</>))

import Data.MailStorage
import Data.List (find)

data SMTPConfig = SMTPConfig
    { smtpPort       :: Int
    , smtpDomainName :: String
    , smtpMaxClients :: Int
    , mailStorageDir :: FilePath
    , userIdentify   :: SMTPConfig -> String -> Maybe String -> IO Bool
    , userVerify     :: SMTPConfig -> String -> IO [String]
    }

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

-- | Accept a client, and communicate with him to get emails.
-- -1- send code 221 ;
-- -2- read/answer any commands
acceptClient :: SMTPConfig -> Handle -> SMTPChan -> IO ()
acceptClient config h chan = do
    respond220 h (smtpDomainName config)
    clientLoop [] [] [] ""
    where
        clientLoop :: String -> String -> [String] -> FilePath -> IO ()
        clientLoop client from to fpath = do
            (err, email) <- runStateT (commandProcessor config h) (Email client from to fpath)
            case err of
                CPQUIT   -> closeHandle config h
                CPRESET  -> clientLoop [] [] [] "" -- clear the information
                -- RFC5321 (section DATA: 4.1.1.4): process the storage of an email after DATA command:
                -- and clear the buffers (so restart a loop without any information)
                -- TODO: respond should be send only if the email storage has been processed properly
                CPEMAIL  -> publishEmail chan email >> clientLoop [] [] [] ""
                CPAGAIN  -> clientLoop (mailClient email) (mailFrom email) (mailTo email) (mailData email)
                CPVRFY u -> do mails <- (userVerify config) config u
                               case mails of
                                    [] -> respond252 h
                                    l  -> respond' h 250 l
                               clientLoop (mailClient email) (mailFrom email) (mailTo email) (mailData email)

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
respond h code msg = BC.hPutStrLn h $ BC.pack $ (show code) ++ " " ++ msg ++ "\r"

respond' h code []     =    BC.hPutStrLn h $ BC.pack $ (show code) ++ "\r"
respond' h code [s]    =    respond h code s
respond' h code (s:xs) = do BC.hPutStrLn h $ BC.pack $ (show code) ++ "-" ++ s ++ "\r"
                            respond' h code xs

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
    modify (\s -> s { mailTo = (to:mailTo s) })
    liftIO $ respond250 h

commandHandleDATA h config = do
    clientDomain <- gets (\s -> mailClient s)
    fromEmail    <- gets (\s -> mailFrom   s)
    filename     <- liftIO $ generateUniqueFilename clientDomain fromEmail
    modify (\s -> s { mailData = filename })
    liftIO $ do
        respond354 h -- say: go on! Don't be shy, give me your data
        BC.writeFile (incomingDir </> filename) BC.empty
        readMailData h $ incomingDir </> filename
        respond250 h
    where
        mailDir     = mailStorageDir config
        incomingDir = mailDir </> getIncomingDir
        -- TODO: insert a timestamp at the TOP of the Data content
        readMailData h p = do
            buff <- liftIO $ BC.hGetLine h
            if buff == BC.pack (".\r")
                then return ()
                else do BC.appendFile p $ BC.concat [buff, BC.pack "\n"]
                        readMailData h p

commandHandleRSET h = liftIO $ respond250 h

data CommandProcessorERROR
    = CPQUIT
    | CPRESET
    | CPEMAIL
    | CPAGAIN
    | CPVRFY String

commandProcessor :: SMTPConfig -> Handle -> EmailS (CommandProcessorERROR)
commandProcessor config h = do
    buff <- liftIO $ BC.hGetLine h
    case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
        Right (HELO client) -> commandHandleHELO h client >> commandProcessor config h
        Right (MAIL from)   -> commandHandleMAIL h from   >> commandProcessor config h
        Right (RCPT to)     -> commandHandleRCPT h to     >> commandProcessor config h
        Right (VRFY user)   -> return $ CPVRFY user
        Right DATA          -> commandHandleDATA h config >> return CPEMAIL
        Right QUIT          -> return CPQUIT
        Right RSET          -> commandHandleRSET h        >> return CPRESET
        Right (NOOP _)      -> (liftIO $ respond250 h)    >> commandProcessor config h
        Right (INVALCMD _)  -> (liftIO $ respond500 h)    >> commandProcessor config h
        Right _             -> (liftIO $ respond502 h)    >> commandProcessor config h
        Left _              -> (liftIO $ respond500 h)    >> return CPAGAIN
        
------------------------------------------------------------------------------
--                               SMTPClient                                 --
------------------------------------------------------------------------------

data SMTPConnection = SMTPConnection
    { handle :: Handle
    } deriving (Eq)

-- | Close the given SMTP Connection and return the handle
-- the server has certainly closed the connection, but let user manages it
smtpCloseConnection :: SMTPConnection
                    -> IO Handle
smtpCloseConnection (SMTPConnection h) =
    smtpSendCommand QUIT (SMTPConnection h) >> return h

-- | After opening a socket, here is the way to know if the server is
-- providing an SMTP service (you still need to parse the return value)
smtpInitConnection :: Handle
                   -> IO (Either Response SMTPConnection)
smtpInitConnection h = do
    -- when client connects to server, server immediately returns a code.
    -- ok (220) or ko (554)
    respl <- smtpReadResponses $ SMTPConnection h
    return $ checkResponseList (head respl) [RC220ServiceReady]
    where
        checkResponseList :: Either String Response
                          -> [ResponseCode] -- ^ expected one
                          -> Either Response SMTPConnection
        checkResponseList r expected =
            case r of
                Left err   -> error err
                Right resp ->
                    case find (checkResponseCode $ code resp) expected of
                        Nothing -> Left resp
                        Just r' -> Right $ SMTPConnection h

        checkResponseCode :: Either Int ResponseCode
                          -> ResponseCode
                          -> Bool
        checkResponseCode (Left  _) _ = False
        checkResponseCode (Right c) e = c == e


smtpSendCommand :: Command
                -> SMTPConnection
                -> IO [Either String Response]
smtpSendCommand (HELO domain) h = smtpSendString h $ "HELO " ++ domain
smtpSendCommand (EHLO domain) h = smtpSendString h $ "EHLO " ++ domain
smtpSendCommand (MAIL from)   h = smtpSendString h $ "MAIL FROM:<" ++ from ++ ">"
smtpSendCommand (RCPT to)     h = smtpSendString h $ "RCPT TO:<" ++ to ++ ">"
smtpSendCommand DATA          h = smtpSendString h "DATA"
smtpSendCommand RSET          h = smtpSendString h "RSET"
smtpSendCommand QUIT          h = smtpSendString h "QUIT"
smtpSendCommand (NOOP s)      h = smtpSendString h $ "NOOP" ++ (maybe [] (\ss -> " " ++ ss) s)
smtpSendCommand (HELP s)      h = smtpSendString h $ "HELP" ++ (maybe [] (\ss -> " " ++ ss) s)
smtpSendCommand (EXPN arg)    h = smtpSendString h $ "EXPN " ++ arg
smtpSendCommand (VRFY arg)    h = smtpSendString h $ "VRFY " ++ arg
-- miss: AUTH
smtpSendCommand cmd           _ = return $ [Left $ "not supported yet: " ++ (show cmd)]

-- | Send a specific string to the server
-- no need to add the command line terminal chars (CRLF) they will be append
smtpSendString :: SMTPConnection -- ^ the connected socket
               -> String         -- ^ the command to send
               -> IO [Either String Response]
smtpSendString (SMTPConnection h) msg = do
    BC.hPutStrLn h $ BC.pack $ msg ++ "\r"
    smtpReadResponses $ SMTPConnection h

smtpReadResponses :: SMTPConnection
                  -> IO [Either String Response]
smtpReadResponses (SMTPConnection h) = do
    buff <- BC.hGetLine h 
    case parseResponseByteString buff of
        Left err -> return $ [Left err]
        Right r  -> do
            if endOfResponse r
                then return [Right r]
                else smtpReadResponses (SMTPConnection h) >>= \rs -> return $ (Right r):rs
