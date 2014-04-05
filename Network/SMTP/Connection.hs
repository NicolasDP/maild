-- |
-- Module      : Network.SMTP.Connection
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Connection
    ( SMTPConnection(..)
    , handleToSMTPConnection
    ) where

import qualified Data.ByteString.Char8 as BC
import System.IO

data SMTPConnection = SMTPConnection
    { hcGetLine :: IO BC.ByteString
    , hcGetSome :: Int -> IO BC.ByteString
    , hcPut     :: BC.ByteString -> IO ()
    , hcFlush   :: IO ()
    , hcClose   :: IO ()
    , hcIsOpen  :: IO Bool
    }

handleToSMTPConnection :: Handle -> SMTPConnection
handleToSMTPConnection h =
    SMTPConnection
        { hcGetLine = BC.hGetLine h
        , hcGetSome = BC.hGetSome h
        , hcPut     = \bs -> BC.hPut h bs >> hFlush h
        , hcFlush   = hFlush h
        , hcClose   = do isopen <- hIsOpen h
                         if isopen then hClose h else return ()
        , hcIsOpen  = hIsOpen h
        }

