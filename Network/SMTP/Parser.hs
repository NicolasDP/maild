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
    ( parseCommandByteString
    , parseCommandString
    , parseResponseByteString
    , parseResponseString
    ) where

import Network.SMTP.Types

import Data.Maild.Email

import qualified Data.ByteString.Char8  as BC
import Data.List                        as L (intercalate, takeWhile, dropWhile, tail)
import Data.Char                             (toUpper, isAlphaNum, isControl)
import Data.Attoparsec.ByteString.Char8 as AC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Control.Applicative ((<|>))

-- | RFC5321 (4.1.2) says a ReversePath is a Path or empty.
parseReversePath :: Parser ReversePath
parseReversePath =
    do  parsePath >>= \path -> return $ Just path
    <|> do string "<>"
           return Nothing

-- | a forward path is a Path
parseForwardPath :: Parser ForwardPath
parseForwardPath = parsePath

parsePath :: Parser Path
parsePath = do
    char '<'
    list <- parseAtDomainList
    mail <- parseMailBox
    char '>'
    return $ Path list mail

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

------------------------------------------------------------------------------
--                               Extension parameters                       --
------------------------------------------------------------------------------

parseMailParameters :: Parser MailParameters
parseMailParameters = parseESMTPParameters

parseRcptParameters :: Parser RcptParameters
parseRcptParameters = parseESMTPParameters

parseESMTPParameters :: Parser ESMTPParameters
parseESMTPParameters = do
    mpair <- parseESMTPParameter
    map   <- do parseSP
                parseESMTPParameters
             <|> return Map.empty
    return $ case mpair of
                Just (k, v) ->Map.insert k v map
                Nothing -> map

parseESMTPParameter :: Parser (Maybe (ESMTPKeyWord, Maybe ESMTPValue))
parseESMTPParameter =
    do key <- parseESMTPKeyWord
       mvalue <- do char '='
                    value <- parseESMTPValue
                    return $ Just value
                 <|> return Nothing
       return $ Just (key, mvalue)
    <|> return Nothing

parseESMTPKeyWord :: Parser ESMTPKeyWord
parseESMTPKeyWord = AC.many1 $ satisfy $ \c -> isAlphaNum c || c == '-'

parseESMTPValue :: Parser ESMTPValue
parseESMTPValue =
    AC.many' $ satisfy $ \c -> not (isControl c || c == '=' || isSpace c)

------------------------------------------------------------------------------
--                               domain and address                         --
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

------------------------------------------------------------------------------
--                               Strings and specials                       --
------------------------------------------------------------------------------

parseParameterString :: Parser String
parseParameterString = do
    parseSP
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    return param

parseParameterMaybeString :: Parser (Maybe String)
parseParameterMaybeString =
    do  s <- parseParameterString
        return $ Just s
    <|> return Nothing

parseParameterB64String :: Parser String
parseParameterB64String = do
    parseSP
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '='
    return param

parseParameterB64MaybeString :: Parser (Maybe String)
parseParameterB64MaybeString =
    do  s <- parseParameterB64String
        return $ Just s
    <|> return Nothing

parseSP :: Parser Char
parseSP = char ' '

parseCRLF :: Parser String
parseCRLF = parseCR >> parseLF >> return "\r\n"

parseCR :: Parser Char
parseCR = char '\r'

parseLF :: Parser Char
parseLF = char '\n'

------------------------------------------------------------------------------
--                               Command                                    --
------------------------------------------------------------------------------

parseCommandHELO :: Parser Command
parseCommandHELO = do
    parseSP
    domain <- parseDomain
    return $ HELO domain

parseCommandEHLO :: Parser Command
parseCommandEHLO = do
    parseSP
    domain <- parseDomain
    return $ EHLO domain

parseCommandMAIL :: Parser Command
parseCommandMAIL = do
    parseSP
    string "FROM:"
    rpath <- parseReversePath
    eparams <- parseMailParameters
    return $ MAIL rpath eparams

parseCommandRCPT :: Parser Command
parseCommandRCPT = do
    parseSP
    string "TO:"
    fpath <- parseForwardPath
    eparams <- parseRcptParameters
    return $ RCPT fpath eparams

parseCommandEXPN :: Parser Command
parseCommandEXPN = do
    param <- parseParameterString
    return $ EXPN param

parseCommandVRFY :: Parser Command
parseCommandVRFY = do
    param <- parseParameterString
    return $ VRFY param

parseCommandNOOP :: Parser Command
parseCommandNOOP = do
    mparam <- parseParameterMaybeString
    return $ NOOP mparam

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
    parseSP
    authType <- parseParameterAuthType
    param <- parseParameterB64MaybeString
    return $ AUTH authType param

parseCommandHELP :: Parser Command
parseCommandHELP = do
    mparam <- parseParameterMaybeString
    return $ HELP mparam

parseCommand :: Parser Command
parseCommand = do
    cmd <- AC.many' AC.letter_ascii
    c <- case map toUpper cmd of
        "HELO" -> parseCommandHELO
        "EHLO" -> parseCommandEHLO
        "MAIL" -> parseCommandMAIL
        "RCPT" -> parseCommandRCPT
        "DATA" -> return DATA
        "EXPN" -> parseCommandEXPN
        "VRFY" -> parseCommandVRFY
        "HELP" -> parseCommandHELP
        "AUTH" -> parseCommandAUTH
        "NOOP" -> parseCommandNOOP
        "RSET" -> return RSET
        "QUIT" -> return QUIT
        s      -> return $ INVALCMD s
    parseCRLF
    return c

-- | Parses a String
parseCommandString :: String -> Either String Command
parseCommandString s = parseCommandByteString $ BC.pack s

-- | Parses a ByteString
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
    parseCRLF
    return $ Response (intToResponseCode $ read xyz) line bool

parseResponse :: Parser Response
parseResponse
    =   parseResponseLine ' ' True  -- End of response
    <|> parseResponseLine '-' False -- we can expect some more lines

-- | parses a response String
parseResponseString :: String -> Either String Response
parseResponseString s = parseResponseByteString $ BC.pack s

-- | parses a response ByteString
parseResponseByteString :: BC.ByteString -> Either String Response
parseResponseByteString bs = parseOnly parseResponse bs
