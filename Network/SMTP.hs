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
import Network.SMTP.Parser

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.State

import qualified Data.ByteString.Char8     as BC

import System.IO
import System.Timeout
import System.Hourglass
import System.FilePath  (FilePath, (</>))
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

import Data.MailStorage
import Data.Maild.Email
import Data.List (find)

data SMTPConfig = SMTPConfig
    { smtpPort       :: Int
    , smtpMxDomain   :: Domain
    , smtpMaxClients :: Int
    , storageDir     :: MailStorage
    } deriving (Show)

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

emptyEmail :: Email
emptyEmail = Email
                { mailFrom   = Nothing
                , mailTo     = []
                , mailData   = []
                }

addMailFrom :: Email -> ReversePath -> Email
addMailFrom email from
    = Email
        { mailFrom = Just from
        , mailTo   = mailTo email
        , mailData = mailData email
        }

addMailTo :: Email -> ForwardPath -> Email
addMailTo email to
    = Email
        { mailFrom = mailFrom email
        , mailTo   = to:(mailTo email)
        , mailData = mailData email
        }

addMailData :: Email -> FilePath -> Email
addMailData email datapath
    = Email
        { mailFrom = mailFrom email
        , mailTo   = mailTo email
        , mailData = datapath
        }

newClientConnectionState :: IO ClientConnectionState
newClientConnectionState = do
    t <- timeCurrent
    return $ ClientConnectionState
        { timestamp    = t
        , domainClient = Nothing
        , smtpType     = Nothing
        , identified   = Nothing
        , smtpMail     = emptyEmail
        }

-- | Accept a client, and communicate with him to get emails.
-- -1- send code 221 ;
-- -2- read/answer any commands
acceptClient :: SMTPConfig -> Handle -> SMTPChan -> IO ()
acceptClient config h chan = do
    listDoms <- listDomains $ storageDir config :: IO [String]
    respond' h 220 $ ("service ready"):listDoms
    newClientConnectionState >>= clientLoop
    where
        clientLoop :: ClientConnectionState -> IO ()
        clientLoop ccs = do
            (err, ccs') <- runStateT (commandProcessor config h) ccs
            case err of
                CPQUIT   -> closeHandle h
                CPEMAIL  -> publishEmail chan (smtpMail ccs')
                              >> runStateT doClearBuffer ccs'
                              >>= \(_, ccs'') -> clientLoop ccs''
                CPAGAIN  -> clientLoop ccs'
                CPVRFY u -> do mails <- findMailStorageUsers (storageDir config) u
                               case mails of
                                    [] -> respond252 h
                                    l  -> respond' h 250 $ userMailToVRFY l
                               clientLoop ccs'

        userMailToVRFY :: [MailStorageUser] -> [String]
        userMailToVRFY []        = []
        userMailToVRFY (user:xs) = (userMailsString (firstName user) (lastName user) (emails user)) ++ (userMailToVRFY xs)

        userMailsString :: String -> String -> [EmailAddress] -> [String]
        userMailsString _ _ [] = []
        userMailsString fname lname (addr:xs) =
            (fname ++ " " ++ lname ++ "<" ++ (show addr) ++ ">"):(userMailsString fname lname xs)

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
                Right QUIT -> closeHandle h
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
respond221 h        = respond h 221 $ "Service closing transmission channel"
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
respond552 h        = respond h 552 "Requested mail action aborted: exceeded storage allocation"
respond553 h        = respond h 553 "Requested action not taken: mailbox name not allowed"

respond334 h        = respond h 334 "Start auth input"
respond354 h        = respond h 354 "Start mail input; end with <CRLF>.<CRLF>"
respond554 h        = respond h 554 "Transaction failed or no SMTP Service here"

closeHandle h = respond221 h >> hClose h

type CCStateS a = StateT ClientConnectionState IO a

commandHandleHELO' h client = do
    mdc <- gets (\s -> domainClient s)
    maybe (return ()) (\_ -> doClearBuffer) mdc
    modify $ \s -> s { domainClient = Just client }

commandHandleHELO h client = do
    commandHandleHELO' h client
    modify $ \s -> s { smtpType = Just SMTP }
    liftIO $ respond250 h 

commandHandleEHLO h client = do
    commandHandleHELO' h client
    modify $ \s -> s { smtpType = Just ESMTP }
    liftIO $
        respond'
            h
            250
            [ "Service ready"
            , "AUTH PLAIN" -- "LOGIN CRAM-MD5"
            ]

commandHandleMAIL :: Handle -> ReversePath -> CCStateS ()
commandHandleMAIL h from = do
    modify (\s -> s { smtpMail = addMailFrom (smtpMail s) from })
    liftIO $ respond250 h

commandHandleRCPT :: Handle -> ForwardPath -> SMTPConfig -> CCStateS ()
commandHandleRCPT h to config = do
    let Path _ addr = to :: Path
    isAuth <- gets (\s -> identified s)
    size <- gets (\s -> length $  mailTo $ smtpMail s)
    isAllowed <- liftIO $ isAllowedRCPT addr isAuth
    if isAllowed
        then do if size < 101
                    then do modify (\s -> s { smtpMail = addMailTo (smtpMail s) to })
                            liftIO $ respond250 h
                    else liftIO $ respond h 452 "too many recipients"
        else let (EmailAddress _ dom) = addr
             in  liftIO $ respond551 h $ dom
    where
        isAllowedRCPT :: EmailAddress -> Maybe a -> IO Bool
        isAllowedRCPT _    (Just _) = return True
        isAllowedRCPT addr Nothing  = isLocalAddress (storageDir config) addr

authentifyPlain :: Handle -> SMTPConfig -> BC.ByteString -> CCStateS ()
authentifyPlain h config buff =
    case serverAuthPlain buff of
        Left err         -> liftIO $ respond h 501 "5.5.2: cannot decode. Is it base64?"
        Right (user, pw) -> do
            muser <- liftIO $ getMailStorageUser (storageDir config) $ BC.unpack user
            case muser of
                Nothing -> liftIO $ respond h 500 "cannot authenticate"
                Just u  ->
                    if userDigest u == BC.unpack pw
                        then do modify (\s -> s { identified = Just u })
                                liftIO $ respond h 235 "2.7.0 Authentication granted"
                        else liftIO $ respond h 535 "Authentication failed"

commandHandleAUTH :: Handle -> SMTPConfig -> AuthType -> Maybe String -> CCStateS ()
commandHandleAUTH h config PLAIN mTxt =
    case mTxt of
        Nothing -> do liftIO $ respond334 h
                      buff <- liftIO $ BC.hGetLine h
                      authentifyPlain h config buff
        Just t  -> authentifyPlain h config $ BC.pack t
commandHandleAUTH h config authType mTxt =
    liftIO $ respond504 h

commandHandleDATA :: Handle -> SMTPConfig -> CCStateS ()
commandHandleDATA h config = do
    mdc          <- gets (\s -> domainClient s)
    mfromEmail   <- gets (\s -> mailFrom $ smtpMail s)
    rcptEmails   <- gets (\s -> not.null $ mailTo $ smtpMail s)
    case (mdc, mfromEmail, rcptEmails) of
        (Nothing, _        , _    ) -> liftIO $ respond h 503 "use command EHLO first"
        (_      , Nothing  , _    ) -> liftIO $ respond h 503 "use command MAIL first"
        (_      , _        , False) -> liftIO $ respond h 503 "use command RCPT first"
        (Just dc, Just from, _    ) -> do
		    filename <- liftIO $ generateUniqueFilename dc (show from)
		    modify (\s -> s { smtpMail = addMailData (smtpMail s) filename })
		    email <- gets (\s -> smtpMail s)
		    mtype <- gets (\s -> smtpType s)
		    liftIO $ do
		        let filepath = (incomingDir $ storageDir config) </> filename
		        respond354 h -- say: go on! Don't be shy, give me your data
		        createIncomingDataFile (storageDir config) (smtpMxDomain config) dc mtype email
		        readMailData h filepath
		        respond250 h
    where
        -- TODO: insert a timestamp at the TOP of the Data content
        readMailData h p = do
            buff <- liftIO $ BC.hGetLine h
            if buff == BC.pack (".\r")
                then return ()
                else do BC.appendFile p $ BC.concat [buff, BC.pack "\n"]
                        readMailData h p

commandHandleRSET h = do
    doClearBuffer
    liftIO $ respond250 h
    return $ CPAGAIN

doClearBuffer :: CCStateS ()
doClearBuffer = modify clearBuffers
    where
        clearBuffers s = s { smtpMail = emptyEmail }

data CommandProcessorERROR
    = CPQUIT
    | CPEMAIL
    | CPAGAIN
    | CPVRFY String

commandProcessor :: SMTPConfig -> Handle -> CCStateS (CommandProcessorERROR)
commandProcessor config h = do
    mbuff <- liftIO $ timeout (5 * 60 * 1000000) $ BC.hGetSome h 2048
    case mbuff of
        Nothing -> return CPQUIT
        Just buff ->
		    case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
		        Right (HELO client) -> commandHandleHELO h client    >> commandProcessor config h
		        Right (EHLO client) -> commandHandleEHLO h client    >> commandProcessor config h
		        Right (MAIL from)   -> commandHandleMAIL h from      >> commandProcessor config h
		        Right (RCPT to)     -> commandHandleRCPT h to config >> commandProcessor config h
		        Right (VRFY user)   -> return $ CPVRFY user
		        Right DATA          -> commandHandleDATA h config    >> return CPEMAIL
		        Right QUIT          -> return CPQUIT
		        Right RSET          -> commandHandleRSET h
		        Right (AUTH t msg)  -> commandHandleAUTH h config t msg >> commandProcessor config h
		        Right (NOOP _)      -> (liftIO $ respond250 h)       >> commandProcessor config h
		        Right (INVALCMD _)  -> (liftIO $ respond500 h)       >> commandProcessor config h
		        Right _             -> (liftIO $ respond502 h)       >> commandProcessor config h
		        Left  _             -> (liftIO $ respond500 h)       >> return CPAGAIN

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
smtpSendCommand (MAIL from)   h = smtpSendString h $ "MAIL FROM:" ++ (showPath from)
smtpSendCommand (RCPT to)     h = smtpSendString h $ "RCPT TO:" ++ (showPath to)
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
