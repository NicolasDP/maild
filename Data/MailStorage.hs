-- |
-- Module      : Data.MailStorage
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Mail are managing as follow:
-- -1- incoming emails are stored in "incoming" directory
-- -2- when the mail has been fully received by the server, the server move it
--     to "fordelivery". At this point, the file is not yet in the recipient
--     mailbox. The email is waiting to be managed/filter/delivered.
module Data.MailStorage
    ( -- * helpers
      getIncomingDir
    , getForDeliveryDir
    , generateUniqueFilename
      -- * General
    , isMailStorageDir
    , initMailStorageDir
    , fromIncomingToFordelivery
    ) where

import System.Random (getStdRandom, randomR)
import qualified Crypto.Hash as Hash

import Control.Monad    (when)
import System.FilePath  (FilePath, (</>))
import System.Directory (getDirectoryContents, doesDirectoryExist, createDirectory, renameFile)

import System.Hourglass (timeCurrent)
import Data.Hourglass   (Elapsed, timePrint, ISO8601_DateAndTime(..))

import qualified Data.ByteString.Char8 as BC (unpack, pack, ByteString)

------------------------------------------------------------------------------
--                              Mail Storages                               --
------------------------------------------------------------------------------

getIncomingDir :: FilePath
getIncomingDir = "incoming"

getForDeliveryDir :: FilePath
getForDeliveryDir = "for-delivery"

-- | list the mandatory directory needed in a MailStorageDir
getMandatorySubDir :: [FilePath]
getMandatorySubDir =
    [ getIncomingDir
    , getForDeliveryDir
    ]


-- | init or create a MailStorageDir
-- if the directory already exists, then it attempt to create the sub-directories
initMailStorageDir :: FilePath -> IO ()
initMailStorageDir dir = do
    isDir <- doesDirectoryExist dir
    when (not isDir) $ createDirectory dir
    createMailStorageSubDir getMandatorySubDir
    where
        createMailStorageSubDir :: [FilePath] -> IO ()
        createMailStorageSubDir []     = return ()
        createMailStorageSubDir (d:ds) = do
            isDir <- doesDirectoryExist $ dir </> d
            when (not isDir) $ createDirectory $ dir </> d
            createMailStorageSubDir ds

-- | check the directory exists and the mandatory subdirectories exist
isMailStorageDir :: FilePath -> IO Bool
isMailStorageDir dir = do
    isDir <- doesDirectoryExist dir
    case isDir of
        True -> isMailStorageDir' getMandatorySubDir
        _    -> return False
    where
        isMailStorageDir' :: [FilePath] -> IO Bool
        isMailStorageDir' []     = return True
        isMailStorageDir' (d:ds) = do
            isPresent <- doesDirectoryExist $ dir </> d
            if isPresent
                then isMailStorageDir' ds
                else return False

-- | This function aims to generate a unique filename.
-- the filename is given as follow:
-- <ISO8601-date>-+-<SHA3 of @client domain@ @from address@ @current time@ @random number@>
generateUniqueFilename :: String -- ^ client domain
                       -> String -- ^ from address
                       -> IO (FilePath)
generateUniqueFilename client from = do
    time <- timeCurrent
    random <- getStdRandom (randomR (1, 9999999)) :: IO Int
    return $ (timePrint getISOTimeFormat time) ++ "_" ++ (BC.unpack $ getHash $ randomThing (show time) (show random))
    where
        getISOTimeFormat = ISO8601_DateAndTime
        getHash :: BC.ByteString -> BC.ByteString
        getHash buff = Hash.digestToHexByteString $ (Hash.hash buff :: Hash.Digest Hash.SHA3_224)

        randomThing :: String -> String -> BC.ByteString
        randomThing t r = BC.pack $ t ++ client ++ from ++ r

-- | move a file from the "bufferisation" area to the
-- "wait for filtering/delivering" area
fromIncomingToFordelivery :: FilePath    -- ^ MailStorageDirectory
                          -> FilePath    -- ^ filename
                          -> IO ()
fromIncomingToFordelivery dir filename = renameFile inCommingPath forDeliveryPath
    where
        inCommingPath   = dir </> getIncomingDir    </> filename
        forDeliveryPath = dir </> getForDeliveryDir </> filename
    
