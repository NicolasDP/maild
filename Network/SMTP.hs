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
    , smtpOpenConnection
    , smtpInitConnection
    , smtpCloseConnection
    , smtpSendEmail
    , smtpSendCommand
    , smtpSendString
    , smtpReadResponses
    ) where

import Network
import Network.SMTP.Types
import Network.SMTP.Auth
import Network.SMTP.Parser
import Network.SMTP.Connection

import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Control.Monad.State

import qualified Data.ByteString.Char8     as BC (ByteString, pack, unpack, concat, appendFile)

import System.IO
import System.Timeout
import System.FilePath  (FilePath, (</>))
import System.Log.Logger (Priority(..))

import Data.MailStorage
import Data.Maild.Email
import Data.List (find, null)

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

newClientConnectionState :: ClientConnectionState
newClientConnectionState = do
    ClientConnectionState
        { domainClient = Nothing
        , smtpType     = Nothing
        , identified   = Nothing
        , smtpMail     = emptyEmail
        }

-- | Accept a client, and communicate with him to get emails.
-- -1- send code 221 ;
-- -2- read/answer any commands
acceptClient :: SMTPConfig -> Handle -> SMTPChan -> IO ()
acceptClient config h chan = do
    con <- handleToSMTPConnection h "server"
    -- list the managed domains and send it to the client
    listDoms <- listDomains $ storageDir config :: IO [String]
    respond' con 220 $ ("service ready"):listDoms
    -- Start the connection loop:
    clientLoop newClientConnectionState con
    where
        clientLoop :: ClientConnectionState -> SMTPConnection -> IO ()
        clientLoop ccs con = do
            (err, ccs') <- runStateT (commandProcessor config con) ccs
            case err of
                CPQUIT   -> closeHandle con
                CPEMAIL  -> publishEmail chan (smtpMail ccs')
                              >> runStateT doClearBuffer ccs'
                              >>= \(_, ccs'') -> clientLoop ccs'' con
                CPAGAIN  -> clientLoop ccs' con
                CPVRFY u -> do mails <- findMailStorageUsers (storageDir config) u
                               case mails of
                                    [] -> respond252 con
                                    l  -> respond' con 250 $ userMailToVRFY l
                               clientLoop ccs' con

        -- a helper to print (show) a list of user :)
        userMailToVRFY :: [MailStorageUser] -> [String]
        userMailToVRFY []        = []
        userMailToVRFY (user:xs) = (userMailsString (firstName user) (lastName user) (emails user)) ++ (userMailToVRFY xs)

        -- print a user and it's email address
        --
        -- > firstname lastname <email1@domain>
        -- > firstname lastname <email2@domain>
        -- > ..
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
    con <- handleToSMTPConnection h "server"
    respond554 con
    loopRejectClient con
    where
        loopRejectClient con = do
            buff <- liftIO $ hcGetLine con
            case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
                Right QUIT -> closeHandle con
                _          -> respond503 con >> loopRejectClient con

------------------------------------------------------------------------------
--                            Responding to Client                          --
------------------------------------------------------------------------------

respond :: SMTPConnection -> Int -> String -> IO ()
respond con code s = do
    let msg = (show code) ++ (if null s then "" else " " ++ s) ++ "\r\n" 
    logMessage con DEBUG (show msg)
    hcPut con $ BC.pack msg 

respond' :: SMTPConnection -> Int -> [String] -> IO ()
respond' con code []     = respond con code []
respond' con code [s]    = respond con code s
respond' con code (s:xs) = do
    let msg = (show code) ++ "-" ++ s ++ "\r\n" 
    logMessage con DEBUG (show msg)
    hcPut con $ BC.pack msg
    respond' con code xs

-- Standard answer messages:
respond500 con        = respond con 500 "Syntax error, command not recognized"
respond501 con        = respond con 501 "Syntax error in parameters"
respond502 con        = respond con 502 "Command not implemented"
respond503 con        = respond con 503 "Bad sequence of commands"
respond504 con        = respond con 504 "Command parameter not implemented"

respond211 con status = respond con 211 $ "System status: " ++ status
respond214 con help   = respond con 214 $ "Help: " ++ help

respond220 con domain = respond con 220 $ domain ++ " Service ready"
respond221 con        = respond con 221 $ "Service closing transmission channel"
respond421 con domain = respond con 421 $ domain ++ " Service not available, closing transmission channel"

respond250 con        = respond con 250 "Action completed"
respond251 con fwd    = respond con 251 $ "User not local, will forward to: <" ++ fwd ++ ">"
respond252 con        = respond con 252 "Cannot VRFY user, but will accept and attempt delivery"

respond455 con        = respond con 455 "Server unable to accomodate parameters"
respond555 con        = respond con 555 "MAIL FROM/RCPT TO not recognized or not implemented"
respond450 con        = respond con 450 "Requested mail action not taken: mailbox unavailable"
respond550 con        = respond con 550 "Requested action not taken: mailbox unavailable"
respond451 con        = respond con 451 "Requested action aborted: error in processing"
respond551 con fwd    = respond con 551 $ "User not local: please try <" ++ fwd ++ ">"
respond452 con        = respond con 452 "Requested action not taken: insufficient system storage"
respond552 con        = respond con 552 "Requested mail action aborted: exceeded storage allocation"
respond553 con        = respond con 553 "Requested action not taken: mailbox name not allowed"

respond334 con        = respond con 334 "Start auth input"
respond354 con        = respond con 354 "Start mail input; end with <CRLF>.<CRLF>"
respond554 con        = respond con 554 "Transaction failed or no SMTP Service here"

closeHandle con = respond221 con >> hcClose con

-- Command State machine

type CCStateS a = StateT ClientConnectionState IO a

commandHandleHELO' _ client = do
    mdc <- gets (\s -> domainClient s)
    maybe (return ()) (\_ -> doClearBuffer) mdc
    modify $ \s -> s { domainClient = Just client }

commandHandleHELO con client = do
    commandHandleHELO' con client
    modify $ \s -> s { smtpType = Just SMTP }
    liftIO $ respond250 con 

commandHandleEHLO con client = do
    commandHandleHELO' con client
    modify $ \s -> s { smtpType = Just ESMTP }
    liftIO $
        respond'
            con
            250
            [ "Service ready"
            , "AUTH PLAIN" -- "LOGIN CRAM-MD5"
            ]

commandHandleMAIL :: SMTPConnection -> ReversePath -> CCStateS ()
commandHandleMAIL con from = do
    modify (\s -> s { smtpMail = addMailFrom (smtpMail s) from })
    liftIO $ respond250 con

commandHandleRCPT :: SMTPConnection -> ForwardPath -> SMTPConfig -> CCStateS ()
commandHandleRCPT con to config = do
    let Path _ addr = to :: Path
    isAuth <- gets (\s -> identified s)
    size <- gets (\s -> length $  mailTo $ smtpMail s)
    isAllowed <- liftIO $ isAllowedRCPT addr isAuth
    if isAllowed
        then do if size < 101
                    then do modify (\s -> s { smtpMail = addMailTo (smtpMail s) to })
                            liftIO $ respond250 con
                    else liftIO $ respond con 452 "too many recipients"
        else let (EmailAddress _ dom) = addr
             in  liftIO $ respond551 con $ dom
    where
        isAllowedRCPT :: EmailAddress -> Maybe a -> IO Bool
        isAllowedRCPT _    (Just _) = return True
        isAllowedRCPT addr Nothing  = isLocalAddress (storageDir config) addr

authentifyPlain :: SMTPConnection -> SMTPConfig -> BC.ByteString -> CCStateS ()
authentifyPlain con config buff =
    case serverAuthPlain buff of
        Left err         -> liftIO $ respond con 501 "5.5.2: cannot decode. Is it base64?"
        Right (user, pw) -> do
            muser <- liftIO $ getMailStorageUser (storageDir config) $ BC.unpack user
            case muser of
                Nothing -> liftIO $ respond con 500 "cannot authenticate"
                Just u  ->
                    if userDigest u == BC.unpack pw
                        then do modify (\s -> s { identified = Just u })
                                liftIO $ respond con 235 "2.7.0 Authentication granted"
                        else liftIO $ respond con 535 "Authentication failed"

commandHandleAUTH :: SMTPConnection -> SMTPConfig -> AuthType -> Maybe String -> CCStateS ()
commandHandleAUTH con config PLAIN mTxt =
    case mTxt of
        Nothing -> do liftIO $ respond334 con
                      buff <- liftIO $ hcGetLine con
                      authentifyPlain con config buff
        Just t  -> authentifyPlain con config $ BC.pack t
commandHandleAUTH con config authType mTxt =
    liftIO $ respond504 con

commandHandleDATA :: SMTPConnection -> SMTPConfig -> CCStateS ()
commandHandleDATA con config = do
    mdc          <- gets (\s -> domainClient s)
    mfromEmail   <- gets (\s -> mailFrom $ smtpMail s)
    rcptEmails   <- gets (\s -> not.null $ mailTo $ smtpMail s)
    case (mdc, mfromEmail, rcptEmails) of
        (Nothing, _        , _    ) -> liftIO $ respond con 503 "use command EHLO first"
        (_      , Nothing  , _    ) -> liftIO $ respond con 503 "use command MAIL first"
        (_      , _        , False) -> liftIO $ respond con 503 "use command RCPT first"
        (Just dc, Just from, _    ) -> do
            filename <- liftIO $ generateUniqueFilename dc (show from) (connectionID con)
            modify (\s -> s { smtpMail = addMailData (smtpMail s) filename })
            email <- gets (\s -> smtpMail s)
            mtype <- gets (\s -> smtpType s)
            liftIO $ do
                let filepath = (incomingDir $ storageDir config) </> filename
                respond354 con -- say: go on! Don't be shy, give me your data
                createIncomingDataFile (storageDir config) (smtpMxDomain config) dc mtype email
                readMailData con filepath
                respond250 con
    where
        -- TODO: insert a timestamp at the TOP of the Data content
        readMailData con p = do
            buff <- liftIO $ hcGetLine con
            if buff == BC.pack (".\r")
                then return ()
                else do BC.appendFile p $ BC.concat [buff, BC.pack "\n"]
                        readMailData con p

commandHandleRSET con = do
    doClearBuffer
    liftIO $ respond250 con

doClearBuffer :: CCStateS ()
doClearBuffer = modify clearBuffers
    where
        clearBuffers s = s { smtpMail = emptyEmail }

data CommandProcessorERROR
    = CPQUIT
    | CPEMAIL
    | CPAGAIN
    | CPVRFY String

commandProcessor :: SMTPConfig -> SMTPConnection -> CCStateS (CommandProcessorERROR)
commandProcessor config con = do
    mbuff <- liftIO $ timeout (5 * 60 * 1000000) $ hcGetSome con 2048
    case mbuff of
        Nothing -> do liftIO $ logMessage con DEBUG "connection timeout"
                      return CPQUIT
        Just buff -> do
            liftIO $ logMessage con DEBUG $ "read: " ++ (show buff)
            case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
                Right (HELO client) -> commandHandleHELO con client    >> commandProcessor config con
                Right (EHLO client) -> commandHandleEHLO con client    >> commandProcessor config con
                Right (MAIL from _) -> commandHandleMAIL con from      >> commandProcessor config con
                Right (RCPT to   _) -> commandHandleRCPT con to config >> commandProcessor config con
                Right (VRFY user)   -> return $ CPVRFY user
                Right DATA          -> commandHandleDATA con config    >> return CPEMAIL
                Right QUIT          -> return CPQUIT
                Right RSET          -> commandHandleRSET con           >> commandProcessor config con
                Right (AUTH t msg)  -> commandHandleAUTH con config t msg >> commandProcessor config con
                Right (NOOP _)      -> (liftIO $ respond250 con)       >> commandProcessor config con
                Right (INVALCMD _)  -> (liftIO $ respond500 con)       >> commandProcessor config con
                Right _             -> (liftIO $ respond502 con)       >> commandProcessor config con
                Left  _             -> (liftIO $ respond500 con)       >> return CPAGAIN

------------------------------------------------------------------------------
--                               SMTPClient                                 --
------------------------------------------------------------------------------

-- | Close the given SMTP Connection and return the handle
-- the server has certainly closed the connection, but let user manages it
smtpCloseConnection :: SMTPConnection
                    -> IO ()
smtpCloseConnection con = do
    smtpSendCommand QUIT con
    logMessage con DEBUG "close connection"
    hcClose con

smtpOpenConnection :: Domain -> PortID -> Domain -> IO (Maybe (SMTPConnection))
smtpOpenConnection d port dom = do
    h <- connectTo d port
    con <- handleToSMTPConnection h "client"
    res <- smtpInitConnection con dom
    if res
        then return $ Just con
        else do logMessage con WARNING $ "can't connect to domain " ++ d
                return Nothing

-- | After opening a socket, here is the way to know if the server is
-- providing an SMTP service (you still need to parse the return value)
smtpInitConnection :: SMTPConnection
                   -> Domain -- ^ the service domain name (us)
                   -> IO Bool
smtpInitConnection con domain = do
    -- when client connects to server, server immediately returns a code.
    respl <- smtpReadResponses con
    if not $ checkResponses RC220ServiceReady respl
        then return False
        else do res <- smtpTryCommand (HELO domain) con RC250Ok
                if res
                    then return True
                    else return False

smtpSendEmail :: ReversePath   -- the sender
              -> [ForwardPath] -- recipients
              -> BC.ByteString -- mail content
              -> SMTPConnection
              -> IO Bool
smtpSendEmail from to content con = do
    smtpTryCommand (MAIL from []) con RC250Ok
    mapM (\t -> smtpTryCommand (RCPT t []) con RC250Ok) to
    smtpTryCommand (DATA     ) con RC354StartMailInput
    sendTheData con
    where
        sendTheData :: SMTPConnection -> IO Bool
        sendTheData con = do
            hcPut con $ BC.concat [content, BC.pack "\r\n.\r\n"]
            l <- smtpReadResponses con
            return $ case l of
                [Right (Response (Right RC250Ok) _ _)] -> True
                _                                      -> False

smtpTryCommand :: Command
               -> SMTPConnection
               -> ResponseCode
               -> IO Bool
smtpTryCommand cmd con expected = do
    respList <- smtpSendCommand cmd con
    return $ checkResponses expected respList

checkResponses :: ResponseCode -> [Either String Response] -> Bool
checkResponses _ [] = error "No response... should not fall here"
checkResponses c [resp]    = checkResponse c resp -- the only or last response
checkResponses c (resp:rs) = if checkResponse c resp then checkResponses c rs else False

checkResponse :: ResponseCode -> Either String Response -> Bool
checkResponse _        (Left _)  = False
checkResponse expected (Right r) = case (code r) of
                                    Left  _ -> False
                                    Right c -> c == expected

smtpSendCommand :: Command
                -> SMTPConnection
                -> IO [Either String Response]
smtpSendCommand (HELO domain) con = smtpSendString con $ "HELO " ++ domain
smtpSendCommand (EHLO domain) con = smtpSendString con $ "EHLO " ++ domain
smtpSendCommand (MAIL from _) con = smtpSendString con $ "MAIL FROM:" ++ (maybe "<>" showPath from)
smtpSendCommand (RCPT to   _) con = smtpSendString con $ "RCPT TO:" ++ (showPath to)
smtpSendCommand DATA          con = smtpSendString con "DATA"
smtpSendCommand RSET          con = smtpSendString con "RSET"
smtpSendCommand QUIT          con = smtpSendString con "QUIT"
smtpSendCommand (NOOP s)      con = smtpSendString con $ "NOOP" ++ (maybe [] (\ss -> " " ++ ss) s)
smtpSendCommand (HELP s)      con = smtpSendString con $ "HELP" ++ (maybe [] (\ss -> " " ++ ss) s)
smtpSendCommand (EXPN arg)    con = smtpSendString con $ "EXPN " ++ arg
smtpSendCommand (VRFY arg)    con = smtpSendString con $ "VRFY " ++ arg
-- miss: AUTH
smtpSendCommand cmd           _ = return $ [Left $ "not supported yet: " ++ (show cmd)]

-- | Send a specific string to the server
-- no need to add the command line terminal chars (CRLF) they will be append
smtpSendString :: SMTPConnection
               -> String         -- ^ the command to send
               -> IO [Either String Response]
smtpSendString con s = do
    let msg = s ++ "\r\n"
    hcPut con $ BC.pack msg
    logMessage con DEBUG $ show msg
    smtpReadResponses con

smtpReadResponses :: SMTPConnection
                  -> IO [Either String Response]
smtpReadResponses con = do
    -- mbuff <- timeout (5 * 60 * 1000000) $ hcGetSome con 2048
    mbuff <- hcGetSome con 2048 >>= \s -> return $ Just s
    case mbuff of
        Nothing   -> do logMessage con DEBUG "connection timenout"
                        return [Left "server didn't respond in less than 5min"]
        Just buff -> do
            logMessage con DEBUG $ "read: " ++ (show buff)
            case parseResponseByteString buff of
                Left err -> return $ [Left err]
                Right r  -> do
                    if endOfResponse r
                        then return [Right r]
                        else smtpReadResponses con >>= \rs -> return $ (Right r):rs
