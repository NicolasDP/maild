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
    , ConnectionID
    , handleToSMTPConnection
    ) where

import qualified Crypto.Hash           as Hash

import qualified Data.ByteString.Char8 as BC
import Data.Hourglass (Elapsed)
import Control.Concurrent.STM (TChan)

import System.Hourglass
import System.IO
import System.Random    (getStdRandom, randomR)
import System.Log.Logger
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Formatter

type ConnectionID = String

data SMTPConnection a = SMTPConnection
    { hcGetLine    :: IO BC.ByteString
    , hcGetSome    :: Int -> IO BC.ByteString
    , hcPut        :: BC.ByteString -> IO ()
    , hcFlush      :: IO ()
    , hcClose      :: IO ()
    , hcIsOpen     :: IO Bool
    , logMessage   :: Priority -> String -> IO ()
    , getHostName  :: String
    , timestamp    :: Elapsed
    , connectionID :: ConnectionID
    , getChannel   :: TChan a
    }

handleToSMTPConnection :: Handle -> String -> String -> TChan a -> IO (SMTPConnection a)
handleToSMTPConnection h name hostname chan = do
    t <- timeCurrent
    random <- getStdRandom (randomR (10000000, 99999999)) :: IO Int
    let hash = show random
    loggingMethod hash DEBUG $ "connection created with: " ++ hostname
    return $ SMTPConnection
        { hcGetLine    = BC.hGetLine h
        , hcGetSome    = BC.hGetSome h
        , hcPut        = \bs -> BC.hPut h bs >> hFlush h
        , hcFlush      = hFlush h
        , hcClose      = do isopen <- hIsOpen h
                            if isopen then hClose h else return ()
        , hcIsOpen     = hIsOpen h
        , logMessage   = loggingMethod hash
        , getHostName  = hostname
        , timestamp    = t
        , connectionID = hash
        , getChannel   = chan
        }
    where
        loggingMethod :: String -> Priority -> String -> IO ()
        loggingMethod h p s = logM ("smtp." ++ name ++ "." ++ h) p s

        getHash :: BC.ByteString -> BC.ByteString
        getHash buff = Hash.digestToHexByteString $ (Hash.hash buff :: Hash.Digest Hash.SHA3_224)
