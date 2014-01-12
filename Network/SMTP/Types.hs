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
    ( Command(..)
    , Email(..)
    , parseCommandByteString
    , parseCommandString
    , getLocalPart
    , getDomainPart
    ) where

import qualified Data.ByteString.Char8 as BC
import Data.List as L (intercalate, takeWhile, dropWhile, tail)
import Data.Char (toUpper, isAlphaNum)
import Data.Attoparsec.ByteString.Char8 as AC

import Control.Applicative ((<|>))

import Network.SMTP.Auth

data Email = Email
    { mailClient :: String
    , mailFrom   :: String
    , mailTo     :: [String]
    , mailData   :: BC.ByteString
    } deriving (Show)

-- | In the case of an address email: local-part@domain
-- return local-part
getLocalPart :: String -> String
getLocalPart = L.takeWhile (\c -> c /= '@')

-- | In the cas of an address email: local-part@domain
-- return domain
getDomainPart :: String -> String
getDomainPart = L.dropWhile (\c -> c /= '@')

data Command
    = HELO String
    | EHLO String
    | MAIL String
    | RCPT String
    | DATA
    | EXPN String
    | VRFY String
    | HELP [String]
    | AUTH AuthType UserName Password
    | NOOP 
    | RSET
    | QUIT
    | INVALCMD String
    deriving (Eq)

instance Show Command where
    show (HELO s)     = "HELO "       ++ s
    show (EHLO s)     = "EHLO "       ++ s
    show (MAIL s)     = "MAIL FROM:<" ++ s ++ ">"
    show (RCPT s)     = "RCPT TO:<"   ++ s ++ ">"
    show DATA         = "DATA"
    show (EXPN s)     = "EXPN "       ++ s
    show (VRFY s)     = "VRFY "       ++ s
    show (HELP [])    = "HELP"
    show (HELP l)     = "HELP " ++ (intercalate " " l)
    show (AUTH t u p) = "AUTH " ++ (show t) ++ " " ++ (BC.unpack u) ++ " ********"
    show NOOP         = "NOOP"
    show RSET         = "RSET"
    show QUIT         = "QUIT"
    show (INVALCMD s) = "INVALCMD " ++ (show s)

parseString :: Parser String
parseString = AC.many' $ do
        AC.letter_ascii
    <|> (char '\\' >> anyChar)

skipEndOfLine :: Parser ()
skipEndOfLine = char '\r' >> return ()

parsePath :: Parser BC.ByteString
parsePath = takeWhile1 $ \c -> c /= '>'

parseCommandHELO :: Parser String
parseCommandHELO = do
    char ' '
    param <- AC.many' $ satisfy $ \c -> isAlphaNum c || c == '.' || c == '-'
    skipEndOfLine
    return param

parseCommandEHLO :: Parser String
parseCommandEHLO = parseCommandHELO

parseCommandMAIL :: Parser String
parseCommandMAIL = do
    char ' '
    string <- "FROM:"
    char '<'
    mail <- parsePath
    char '>'
    skipEndOfLine
    return $ BC.unpack mail

parseCommandRCPT :: Parser String
parseCommandRCPT = do
    char ' '
    string <- "TO:"
    char '<'
    mail <- parsePath
    char '>'
    skipEndOfLine
    return $ BC.unpack mail

parseCommandEXPN :: Parser String
parseCommandEXPN = undefined

parseCommandVRFY :: Parser String
parseCommandVRFY = undefined

parseCommandHelp :: Parser [String]
parseCommandHelp = do
    c1 <- anyChar
    case c1 of
        ' '  -> do parseString >>= \l -> parseCommandHelp >>= \ls -> return $ l:ls
        '\n' -> do char '\r' >>= \_ -> return []
        _    -> error $ "Unexpected character: '" ++ [c1] ++ "'"

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
        "HELP" -> parseCommandHelp >>= \l -> return $ HELP l
        "AUTH" -> undefined
        "NOOP" -> skipEndOfLine >> return NOOP
        "RSET" -> skipEndOfLine >> return RSET
        "QUIT" -> skipEndOfLine >> return QUIT
        s      -> return $ INVALCMD s

parseCommandString :: String -> Either String Command
parseCommandString s = parseCommandByteString $ BC.pack s

parseCommandByteString :: BC.ByteString -> Either String Command
parseCommandByteString bs = parseOnly parseCommand bs
