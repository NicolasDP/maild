-- |
-- Module      : Network.SMTP.Server
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Server
    ( -- * Notifications
      SMTPChan
    , newSMTPChan
    , getNextEmail
      -- * Configuration
    , SMTPConfig(..)
      -- * Main method
    , acceptClient
    , rejectClient
    ) where

import Network.SMTP.Types
import Network.SMTP.Auth
import Network.SMTP.Parser
import Network.SMTP.Connection

import Data.MailStorage
import Data.Maild.Email

import Control.Concurrent.STM
import Control.Monad.State (liftIO, modify, gets, StateT, evalStateT)

import Network (HostName)

import System.IO (Handle)
import System.Timeout (timeout)
import System.FilePath  (FilePath, (</>))
import System.Log.Logger (Priority(..))

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC

------------------------------------------------------------------------------
--                                Config                                    --
------------------------------------------------------------------------------

-- | The server configuration
data SMTPConfig = SMTPConfig
    { smtpPort       :: Int         -- ^ the port to listen at
    , smtpMxDomain   :: Domain      -- ^ the server domain name
    , smtpMaxClients :: Int         -- ^ the max clients
    , smtpBlackList  :: FilePath    -- ^ the the file which contains the blacklisted
    , storageDir     :: MailStorage
    } deriving (Show)

type ServerConnection = SMTPConnection Email

------------------------------------------------------------------------------
--                               SMTPChan                                   --
------------------------------------------------------------------------------

-- | A channel used to be notified each time Email has been received
--
-- When you start a server, it will notify you through this SMTPChan each time
-- it received an email.
type SMTPChan = TChan Email

-- | a helper to create a SMTPChan
newSMTPChan :: IO SMTPChan
newSMTPChan = atomically newTChan

-- internal command: publish the received email
publishEmail :: SMTPChan -> Email -> IO ()
publishEmail chan = atomically . writeTChan chan

-- | return a received email.
--
-- will block until the SMTP server received an email.
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
addMailFrom email from = email { mailFrom = Just from }

addMailTo :: Email -> ForwardPath -> Email
addMailTo email to = email { mailTo   = to:(mailTo email) }

addMailData :: Email -> FilePath -> Email
addMailData email datapath = email { mailData = datapath }

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
acceptClient :: SMTPConfig
             -> Handle   -- connected client
             -> HostName -- client's ip addr or hostname
             -> SMTPChan
             -> IO ()
acceptClient config h hostname chan = do
    con <- handleToSMTPConnection h hostname "server" chan
    -- list the managed domains and send it to the client
    listDoms <- listDomains $ storageDir config
    respond' con 220 $ "service ready":listDoms
    -- Start the connection loop
    clientStart newClientConnectionState con
    where
        clientStart :: ClientConnectionState -> ServerConnection -> IO ()
        clientStart ccs con = evalStateT (commandProcessorInit config con) ccs

-- | Reject a client:
-- as described in RFC5321:
-- -1- send code 554 ;
-- -2- loop until client quit but always answer him code
--     503 for any other command/string/input received from the handler.
rejectClient :: SMTPConfig -> Handle -> HostName -> IO ()
rejectClient config h hostname = do
    con <- handleToSMTPConnection h hostname "server" =<< atomically newTChan 
    respond554 con
    closeHandle con

------------------------------------------------------------------------------
--                            Responding to Client                          --
------------------------------------------------------------------------------

respond :: ServerConnection -> Int -> String -> IO ()
respond con code s = do
    let msg = (show code) ++ (if null s then "" else " " ++ s) ++ "\r\n" 
    logMessage con DEBUG (show msg)
    hcPut con $ BC.pack msg 

respond' :: ServerConnection -> Int -> [String] -> IO ()
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

------------------------------------------------------------------------------
--                          Command State machine                           --
------------------------------------------------------------------------------

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

commandHandleMAIL :: ServerConnection -> ReversePath -> CCStateS ()
commandHandleMAIL con from = do
    modify (\s -> s { smtpMail = addMailFrom (smtpMail s) from })
    liftIO $ respond250 con

commandHandleRCPT :: ServerConnection -> ForwardPath -> SMTPConfig -> CCStateS ()
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

authentifyPlain :: ServerConnection -> SMTPConfig -> ByteString -> CCStateS ()
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

commandHandleAUTH :: ServerConnection -> SMTPConfig -> AuthType -> Maybe String -> CCStateS ()
commandHandleAUTH con config PLAIN mTxt =
    case mTxt of
        Nothing -> do liftIO $ respond334 con
                      buff <- liftIO $ hcGetLine con
                      authentifyPlain con config buff
        Just t  -> authentifyPlain con config $ BC.pack t
commandHandleAUTH con config authType mTxt =
    liftIO $ respond504 con

commandHandleDATA :: ServerConnection -> SMTPConfig -> CCStateS ()
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
                -- create the incoming data file (and add MIME Received at the begin of the file)
                createIncomingDataFile (storageDir config) (smtpMxDomain config) dc mtype email
                respond354 con -- respond we are ready to receive the data
                readMailData con filepath -- read the data and store it in the incoming data file
                respond250 con -- respond the mail has been sent
                publishEmail (getChannel con) email
            doClearBuffer
    where
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

commandHandleVRFY :: SMTPConfig -> ServerConnection -> String -> CCStateS ()
commandHandleVRFY config con u = do
    mails <- liftIO $ findMailStorageUsers (storageDir config) u
    liftIO $ case mails of
         [] -> respond252 con
         l  -> respond' con 250 $ userMailToVRFY l
    return ()
    where
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

getNextCommand :: SMTPConfig -> ServerConnection -> IO Command
getNextCommand config con = do
    mbuff <- timeout (5 * 60 * 1000000) $ hcGetSome con 2048
    case mbuff of
        Nothing -> do logMessage con DEBUG "connection timeout"
                      return $ TIMEOUT
        Just buff -> do
            logMessage con DEBUG $ "read: " ++ (show buff)
            return $ case parseCommandByteString $ BC.concat [buff, BC.pack "\n\r"] of
                Right cmd    -> cmd
                Left message -> INVALCMD message

commandProcessorInit :: SMTPConfig -> ServerConnection -> CCStateS ()
commandProcessorInit config con = do
    cmd <- liftIO $ getNextCommand config con
    case cmd of
        HELO client -> commandHandleHELO con client >> commandProcessor821 config con
        EHLO client -> commandHandleEHLO con client >> commandProcessor5321 config con
        -- QUIT all connection that does not start by HELO of EHLO
        _           -> (liftIO $ closeHandle con) >> return ()

commandProcessor821 :: SMTPConfig -> ServerConnection -> CCStateS ()
commandProcessor821 config con = do
    cmd <- liftIO $ getNextCommand config con
    case cmd of
        -- Minimal implementation
        HELO client -> commandHandleHELO con client       >> commandProcessor821 config con
        MAIL from _ -> commandHandleMAIL con from         >> commandProcessor821 config con
        RCPT to   _ -> commandHandleRCPT con to config    >> commandProcessor821 config con
        DATA        -> commandHandleDATA con config       >> commandProcessor821 config con -- send mail and clear the buffer
        RSET        -> commandHandleRSET con              >> commandProcessor821 config con
        NOOP _      -> (liftIO $ respond250 con)          >> commandProcessor821 config con
        QUIT        -> liftIO $ closeHandle con
        -- Optionals
        VRFY user   -> commandHandleVRFY config con user  >> commandProcessor821 config con
        INVALCMD _  -> liftIO (respond500 con >> closeHandle con)
        TIMEOUT     -> liftIO $ closeHandle con
        -- Missing command: EXPN
        -- other commands : SEND, SOML, SAML && TURN
        _           -> (liftIO $ respond502 con)          >> commandProcessor821 config con

commandProcessor5321 :: SMTPConfig -> ServerConnection -> CCStateS ()
commandProcessor5321 config con = do
    cmd <- liftIO $ getNextCommand config con
    case cmd of
        -- Minimal implementation:
        EHLO client -> commandHandleEHLO con client       >> commandProcessor5321 config con
        MAIL from _ -> commandHandleMAIL con from         >> commandProcessor5321 config con
        RCPT to   _ -> commandHandleRCPT con to config    >> commandProcessor5321 config con
        DATA        -> commandHandleDATA con config       >> commandProcessor5321 config con -- send mail and clear the buffer
        RSET        -> commandHandleRSET con              >> commandProcessor5321 config con
        NOOP _      -> (liftIO $ respond250 con)          >> commandProcessor5321 config con
        QUIT        -> liftIO $ closeHandle con
        VRFY user   -> commandHandleVRFY config con user  >> commandProcessor5321 config con
        -- Extentions:
        AUTH t msg  -> commandHandleAUTH con config t msg >> commandProcessor5321 config con
        INVALCMD _  -> liftIO (respond500 con >> closeHandle con)
        TIMEOUT     -> liftIO $ closeHandle con
        -- Missing command  : EXPN, STARTTLS
        -- obsolote commands: SEND, SOML, SAML && TURN
        _           -> (liftIO $ respond502 con)          >> commandProcessor5321 config con
