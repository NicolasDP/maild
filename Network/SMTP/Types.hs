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
    , ESMTPParameter(..)
    , MailParameters
    , RcptParameters
      -- * SMTP Responses
    , Response(..)
    , ResponseCode(..)
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.Maild.Email

------------------------------------------------------------------------------
--                           Mail Storage Users                             --
------------------------------------------------------------------------------

data MailStorageUser = MailStorageUser
    { emails      :: [EmailAddress] -- ^ the list of email address owned
    , firstName   :: String         -- ^ user's firstname
    , lastName    :: String         -- ^ user's lastname
    , userDigest  :: String         -- ^ user's digest
    } deriving (Show, Eq)

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

type UserName = BC.ByteString
type Password = BC.ByteString
data AuthType
    = PLAIN
    | LOGIN
    | CRAM_MD5
    deriving (Read, Show, Eq)

------------------------------------------------------------------------------
--                               Commands                                   --
------------------------------------------------------------------------------

type ESMTPKeyWord = String
type ESMTPValue   = String
data ESMTPParameter = ESMTPParameter
    { paramKey :: ESMTPKeyWord
    , paramValue :: Maybe ESMTPValue
    } deriving (Eq, Show)

type MailParameters = [ESMTPParameter]
type RcptParameters = [ESMTPParameter]

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
    | INVALCMD String
    | TIMEOUT
    deriving (Show, Eq)

------------------------------------------------------------------------------
--                               Response                                   --
------------------------------------------------------------------------------

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

-- As described in a RFC5321, a response is a CODE with message
-- see section 4.2.1
data Response = Response
    { code          :: Either Int ResponseCode
    , message       :: BC.ByteString
    , endOfResponse :: Bool
    } deriving (Show)

