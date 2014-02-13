-- |
-- Module      : Network.SMTP.Types
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SMTP.Types
    ( -- * Commands
      Command(..)
    , Email(..)
    , Path(..)
    , EmailAddress(..)
    , showPath
    , ForwardPath(..)
    , ReversePath(..)
    , parseCommandByteString
    , parseCommandString
    , getLocalPart
    , getDomainPart
      -- * Responses
    , Response(..)
    , ResponseCode(..)
    , parseResponseByteString
    , parseResponseString
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.List as L (intercalate, takeWhile, dropWhile, tail)
import Data.Char (toUpper, isAlphaNum)
import Data.Attoparsec.ByteString.Char8 as AC

import Control.Applicative ((<|>))

import Network.SMTP.Auth

data Email = Email
    { mailClient   :: String
    , mailFrom     :: ReversePath
    , mailTo       :: [ForwardPath]
    , mailData     :: FilePath
    , identified   :: Bool
    } deriving (Show, Eq)

-- | In the case of an address email: local-part@domain
-- return local-part
getLocalPart :: String -> String
getLocalPart = L.takeWhile (\c -> c /= '@')

-- | In the cas of an address email: local-part@domain
-- return domain
getDomainPart :: String -> String
getDomainPart = L.dropWhile (\c -> c /= '@')

------------------------------------------------------------------------------
--                               Commands                                   --
------------------------------------------------------------------------------

type Domain = String
type LocalPart = String

data EmailAddress = EmailAddress LocalPart Domain
    deriving (Eq)
instance Show EmailAddress where
    show (EmailAddress local domain) = local ++ "@" ++ domain

data Path = Path [Domain] EmailAddress
    deriving (Show, Eq)

showPath :: Path -> String
showPath (Path adl from) = "<" ++ showADL ++ (show from) ++ ">"
    where
        showADL = if null adl then "" else showADL' adl
        showADL' :: [String] -> String
        showADL' [ad]    = "@" ++ (show ad) ++ ":"
        showADL' (ad:xs) = "@" ++ (show ad) ++ "," ++ (showADL' xs)

type ForwardPath = Path
type ReversePath = Path

data Command
    = HELO String
    | EHLO String
    | MAIL ReversePath
    | RCPT ForwardPath
    | DATA
    | EXPN String
    | VRFY String
    | HELP (Maybe String)
    | AUTH AuthType UserName Password
    | NOOP (Maybe String)
    | RSET
    | QUIT
    | INVALCMD String
    deriving (Show, Eq)

parseString :: Parser String
parseString = AC.many' $ do
        AC.letter_ascii
    <|> (char '\\' >> anyChar)

skipEndOfLine :: Parser ()
skipEndOfLine = char '\r' >> return ()

parseParameterPath :: Parser BC.ByteString
parseParameterPath = takeWhile1 $ \c -> c /= '>'

parseCommandHELO :: Parser String
parseCommandHELO = do
    char ' '
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    skipEndOfLine
    return param

parseCommandEHLO :: Parser String
parseCommandEHLO = parseCommandHELO

parseAtDomainList :: Parser [Domain]
parseAtDomainList =
    do char '@'
       ad <- parseDomain
       list <- parseAtDomainList'
       return $ ad:list
    <|> return []
    where
        parseAtDomainList' :: Parser [String]
        parseAtDomainList' =
            do char ','
               parseAtDomainList
            <|> do char ':'
                   return []

parseDomain :: Parser Domain
parseDomain = AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'

parseLocalPart :: Parser LocalPart
parseLocalPart = parseDomain

parseMailBox :: Parser EmailAddress
parseMailBox = do
    local <- parseLocalPart
    char '@'
    domain <- parseDomain
    return $ EmailAddress local domain

parsePath :: Parser Path
parsePath = do
    char '<'
    list <- parseAtDomainList
    mail <- parseMailBox
    char '>'
    return $ Path list mail

parseReversePath :: Parser ReversePath
parseReversePath = parsePath

parseForwardPath :: Parser ForwardPath
parseForwardPath = parsePath

parseCommandMAIL :: Parser ReversePath
parseCommandMAIL = do
    char ' '
    string <- "FROM:"
    mail <- parseReversePath
    skipEndOfLine
    return mail

parseCommandRCPT :: Parser ForwardPath
parseCommandRCPT = do
    char ' '
    string <- "TO:"
    mail <- parseForwardPath
    skipEndOfLine
    return mail

parseCommandEXPN :: Parser String
parseCommandEXPN = parseParameterString

parseCommandVRFY :: Parser String
parseCommandVRFY = parseParameterString

parseCommandNOOP :: Parser (Maybe String)
parseCommandNOOP = parseParameterMaybeString

parseCommandHELP :: Parser (Maybe String)
parseCommandHELP = parseParameterMaybeString

parseParameterMaybeString :: Parser (Maybe String)
parseParameterMaybeString =
    skipEndOfLine >> return Nothing
    <|> do s <- parseParameterString
           return $ Just s

parseParameterString :: Parser String
parseParameterString = do
    char ' '
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    skipEndOfLine
    return param

parseCommand :: Parser Command
parseCommand = do
    skipSpace -- TODO: it is not RFC...
    cmd <- AC.many' AC.letter_ascii
    case map toUpper cmd of
        "HELO" -> parseCommandHELO >>= \s -> return $ HELO s
        "EHLO" -> parseCommandEHLO >>= \s -> return $ EHLO s
        "MAIL" -> parseCommandMAIL >>= \s -> return $ MAIL s
        "RCPT" -> parseCommandRCPT >>= \s -> return $ RCPT s
        "DATA" -> skipEndOfLine >> return DATA
        "EXPN" -> parseCommandEXPN >>= \s -> return $ EXPN s
        "VRFY" -> parseCommandVRFY >>= \s -> return $ VRFY s
        "HELP" -> parseCommandHELP >>= \l -> return $ HELP l
        "AUTH" -> undefined
        "NOOP" -> parseCommandNOOP >>= \s -> return $ NOOP s
        "RSET" -> skipEndOfLine >> return RSET
        "QUIT" -> skipEndOfLine >> return QUIT
        s      -> return $ INVALCMD s

parseCommandString :: String -> Either String Command
parseCommandString s = parseCommandByteString $ BC.pack s

parseCommandByteString :: BC.ByteString -> Either String Command
parseCommandByteString bs = parseOnly parseCommand bs

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

intToResponseCode :: Int -> Either Int ResponseCode
intToResponseCode 500 = Right RC500SyntaxCommandError
intToResponseCode 501 = Right RC501SyntaxParameterError
intToResponseCode 502 = Right RC502CommandNotImplemented
intToResponseCode 503 = Right RC503BadSequenceOfCommand
intToResponseCode 504 = Right RC504ParameterNotImplemented
intToResponseCode 211 = Right RC211SystemStatus
intToResponseCode 214 = Right RC214Help
intToResponseCode 220 = Right RC220ServiceReady
intToResponseCode 221 = Right RC221ServiceClosingChannel
intToResponseCode 421 = Right RC421ServiceNotAvailable
intToResponseCode 250 = Right RC250Ok
intToResponseCode 251 = Right RC251UserNotLocalButTry
intToResponseCode 252 = Right RC252CannotVRFYUser
intToResponseCode 455 = Right RC455ServerUnableToAccParrameters
intToResponseCode 555 = Right RC555FromOrToError
intToResponseCode 450 = Right RC450MailActionRejectedMailboxUnavailable
intToResponseCode 550 = Right RC550ActionRejectedMailboxUnavailable
intToResponseCode 451 = Right RC451ActionAborted
intToResponseCode 551 = Right RC551UserNotLocalFail
intToResponseCode 452 = Right RC452ActionRejectedSystemStorage
intToResponseCode 552 = Right RC552MailActionAbbortedSystemStorage
intToResponseCode 553 = Right RC553ActionRejectedMailboxNotAllowed
intToResponseCode 354 = Right RC354StartMailInput
intToResponseCode 554 = Right RC554Failed
intToResponseCode  i  = Left i

-- As described in a RFC5321, a response is a CODE with message
-- see section 4.2.1
data Response = Response
    { code          :: Either Int ResponseCode
    , message       :: BC.ByteString
    , endOfResponse :: Bool
    } deriving (Show)

parseResponseLine :: Char -- SP character (' '  or '-'  )
                  -> Bool -- End of resp  (True or False)
                  -> Parser Response
parseResponseLine c bool = do
    xyz <- count 3 digit
    char c
    line <- AC.takeWhile $ \c -> c /= '\r'
    skipEndOfLine
    return $ Response (intToResponseCode $ read xyz) line bool

parseResponse :: Parser Response
parseResponse
    =   parseResponseLine ' ' True
    <|> parseResponseLine '-' False

parseResponseString :: String -> Either String Response
parseResponseString s = parseResponseByteString $ BC.pack s

parseResponseByteString :: BC.ByteString -> Either String Response
parseResponseByteString bs = parseOnly parseResponse bs
