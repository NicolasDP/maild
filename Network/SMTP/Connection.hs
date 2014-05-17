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

-- | unique identifier for a given connection
type ConnectionID = String

-- | A SMTPConnection
--
-- define basis and method to properly handle connections in a context
-- of SMTP protocol
data SMTPConnection a = SMTPConnection
    { hcGetLine    :: IO BC.ByteString
    , hcGetSome    :: Int -> IO BC.ByteString
    , hcPut        :: BC.ByteString -> IO ()
    , hcFlush      :: IO ()
    , hcClose      :: IO ()
    , hcIsOpen     :: IO Bool
    , logMessage   :: Priority -> String -> IO () -- ^ log messages with connection context information
    , getHostName  :: String                      -- ^ the client hostname (domain name)
    , timestamp    :: Elapsed                     -- ^ the date-time when the connection has been established
    , connectionID :: ConnectionID                -- ^ a unique identifier for this connection
    , getChannel   :: TChan a                     -- ^ get the Connection channel for notification
    }

-- | Create an SMTPConnection from the given handle
handleToSMTPConnection :: Handle  -- ^ the connection Handle
                       -> String  -- ^ the client hostname
                       -> String  -- ^ a connection name (use for debug purpose only)
                       -> TChan a
                       -> IO (SMTPConnection a)
handleToSMTPConnection h hostname name chan = do
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
