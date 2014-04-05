-- |
-- Module      : Network.SMTP.Parser
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
{-# LANGUAGE OverloadedStrings #-}
module Network.SMTP.Parser
    (
      parseCommandByteString
    , parseCommandString
    , parseResponseByteString
    , parseResponseString
    ) where

import Network.SMTP.Types

import Data.Maild.Email

import qualified Data.ByteString.Char8  as BC
import Data.List                        as L (intercalate, takeWhile, dropWhile, tail)
import Data.Char (toUpper, isAlphaNum)
import Data.Attoparsec.ByteString.Char8 as AC

import Control.Applicative ((<|>))

parseParameterString :: Parser String
parseParameterString = do
    char ' '
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    skipEndOfLine
    return param

parseParameterMaybeString :: Parser (Maybe String)
parseParameterMaybeString =
    do  skipEndOfLine
        return Nothing
    <|> do s <- parseParameterString
           return $ Just s

parseParameterB64String :: Parser String
parseParameterB64String = do
    char ' '
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '='
    skipEndOfLine
    return param

parseParameterB64MaybeString :: Parser (Maybe String)
parseParameterB64MaybeString =
    do  skipEndOfLine
        return Nothing
    <|> do s <- parseParameterB64String
           return $ Just s

skipEndOfLine :: Parser ()
skipEndOfLine = char '\r' >> char '\n' >> return ()

------------------------------------------------------------------------------
--                               Command                                    --
------------------------------------------------------------------------------

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

parseCommandHELO :: Parser String
parseCommandHELO = do
    char ' '
    param <- parseDomain
    skipEndOfLine
    return param

parseCommandEHLO :: Parser String
parseCommandEHLO = parseCommandHELO

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

parseParameterAuthType :: Parser AuthType
parseParameterAuthType = do
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    return $ case map toUpper param of
                "PLAIN"    -> PLAIN
                "LOGIN"    -> LOGIN
                "CRAM-MD5" -> CRAM_MD5
                _          -> error $ "not supported auth type: " ++ param

parseCommandAUTH :: Parser Command
parseCommandAUTH = do
    char ' '
    authType <- parseParameterAuthType
    param <- parseParameterB64MaybeString
    return $ AUTH authType param

parseCommandHELP :: Parser (Maybe String)
parseCommandHELP = parseParameterMaybeString

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
        "AUTH" -> parseCommandAUTH
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
    =   parseResponseLine ' ' True  -- End of response
    <|> parseResponseLine '-' False -- we can expect some more lines

parseResponseString :: String -> Either String Response
parseResponseString s = parseResponseByteString $ BC.pack s

parseResponseByteString :: BC.ByteString -> Either String Response
parseResponseByteString bs = parseOnly parseResponse bs
