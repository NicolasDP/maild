-- |
-- Module      : Network.SMTP.Types
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Types
    ( -- * Mail Storage User
      MailStorageUser(..)
      -- * ClientState
    , ClientConnectionState(..)
    , SMTPType(..)
      -- * Authentification
    , UserName
    , Password
    , AuthType(..)
      -- * SMTP Commands
      -- ** Command
    , Command(..)
      -- ** SMTP extensions
    , ESMTPKeyWord
    , ESMTPValue
    , ESMTPParameters
    , MailParameters
    , RcptParameters
      -- * SMTP Responses
    , Response(..)
    , ResponseCode(..)
    ) where

import Data.Map.Strict (Map)
import qualified Data.ByteString.Char8 as BC
import Data.Maild.Email

------------------------------------------------------------------------------
--                           Mail Storage Users                             --
------------------------------------------------------------------------------

-- | describe a user
data MailStorageUser = MailStorageUser
    { emails      :: [EmailAddress] -- ^ the list of email address owned
    , firstName   :: String         -- ^ user's firstname
    , lastName    :: String         -- ^ user's lastname
    , userDigest  :: String         -- ^ user's digest
    } deriving (Show, Eq)

-- | SMTP or EMSTP protocol
data SMTPType = SMTP | ESMTP
    deriving (Show, Eq)

-- | contains all the needed data to receive/forward emails
data ClientConnectionState = ClientConnectionState
    { domainClient :: Maybe Domain          -- ^ the domain name of the client
    , identified   :: Maybe MailStorageUser -- ^ if the user has been identified
    , smtpType     :: Maybe SMTPType
    , smtpMail     :: Email                 -- ^ the email
    } deriving (Show, Eq)

------------------------------------------------------------------------------
--                            Authentifications                             --
------------------------------------------------------------------------------

-- | user's name
type UserName = BC.ByteString

-- | user's digest
type Password = BC.ByteString

-- | type of authentification implemented
data AuthType
    = PLAIN
    | LOGIN
    | CRAM_MD5
    deriving (Read, Show, Eq)

------------------------------------------------------------------------------
--                               Commands                                   --
------------------------------------------------------------------------------

-- | A ESMTP key word
type ESMTPKeyWord = String

-- | A ESMTPValue
type ESMTPValue   = String

-- | A ESMTPParameter collection
type ESMTPParameters = Map ESMTPKeyWord (Maybe ESMTPValue)

-- | the MAIL command may receive parameters
type MailParameters = ESMTPParameters
-- | the RCTP command may receive parameters
type RcptParameters = ESMTPParameters

-- | (E)SMTP commands
data Command
    = HELO String
    | EHLO String
    | MAIL ReversePath MailParameters
    | RCPT ForwardPath RcptParameters
    | DATA
    | EXPN String
    | VRFY String
    | HELP (Maybe String)
    | AUTH AuthType (Maybe String)
    | NOOP (Maybe String)
    | RSET
    | QUIT
    | INVALCMD String -- ^ use to know if the command is not supported
    | TIMEOUT         -- ^ use to raise an error in case of a timeout
    deriving (Show, Eq)

------------------------------------------------------------------------------
--                               Response                                   --
------------------------------------------------------------------------------

-- | Non-exautive list of possible response code
data ResponseCode
    = RC500SyntaxCommandError
    | RC501SyntaxParameterError
    | RC502CommandNotImplemented
    | RC503BadSequenceOfCommand
    | RC504ParameterNotImplemented
    | RC211SystemStatus
    | RC214Help
    | RC220ServiceReady
    | RC221ServiceClosingChannel
    | RC421ServiceNotAvailable
    | RC250Ok
    | RC251UserNotLocalButTry
    | RC252CannotVRFYUser
    | RC455ServerUnableToAccParrameters
    | RC555FromOrToError
    | RC450MailActionRejectedMailboxUnavailable
    | RC550ActionRejectedMailboxUnavailable
    | RC451ActionAborted
    | RC551UserNotLocalFail
    | RC452ActionRejectedSystemStorage
    | RC552MailActionAbbortedSystemStorage
    | RC553ActionRejectedMailboxNotAllowed
    | RC354StartMailInput
    | RC554Failed
	deriving (Show, Read, Eq)

-- | As described in a RFC5321, a response is a CODE with message
-- see section 4.2.1
data Response = Response
    { code          :: Either Int ResponseCode
    , message       :: BC.ByteString
    , endOfResponse :: Bool
    } deriving (Show)

