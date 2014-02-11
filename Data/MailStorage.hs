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
    , getUserAccountDir
    , generateUniqueFilename
      -- * General
    , MailStorage(..)
    , MailUser(..)
    , isMailStorageDir
    , initMailStorageDir
    , getMailStorage
    , fromIncomingToFordelivery
    , getMailUserAccounts
    , findMailUsers
    ) where

import qualified Crypto.Hash as Hash

import Control.Monad       (when)
import Control.Monad.State

import System.FilePath  (FilePath, (</>), takeFileName)
import System.Directory (getDirectoryContents, doesDirectoryExist, createDirectory, renameFile)
import System.Random    (getStdRandom, randomR)

import System.Hourglass (timeCurrent)
import Data.Hourglass   (Elapsed, timePrint, ISO8601_DateAndTime(..))
import Data.Char        (isSpace, toLower)

import qualified Data.ByteString.Char8 as BC (unpack, pack, ByteString)

------------------------------------------------------------------------------
--                              Mail Storages                               --
------------------------------------------------------------------------------

getIncomingDir :: FilePath
getIncomingDir = "incoming"

getForDeliveryDir :: FilePath
getForDeliveryDir = "for-delivery"

getUserAccountDir :: FilePath
getUserAccountDir = "users"

-- | list the mandatory directory needed in a MailStorageDir
getMandatorySubDir :: [FilePath]
getMandatorySubDir =
    [ getIncomingDir
    , getForDeliveryDir
    , getUserAccountDir
    ]

data MailStorage = MailStorage
    { mainDir        :: FilePath
    , incomingDir    :: FilePath
    , forDeliveryDir :: FilePath
    , userAccountDir :: FilePath
    , userAccounts   :: [MailUser]
    } deriving (Eq)

-- | get mailStorage:
getMailStorage :: FilePath -> IO (Maybe MailStorage)
getMailStorage dir = do
    isDir <- isMailStorageDir dir
    case isDir of
        False -> return $ Nothing
        True  -> do
            users <- getMailUserAccounts (MailStorage [] [] [] accountsDir [])
            return $ Just $ MailStorage
                                dir
                                (dir </> getIncomingDir)
                                (dir </> getForDeliveryDir)
                                accountsDir
                                users
        where
            accountsDir = dir </> getUserAccountDir

-- | init or create a MailStorageDir
-- if the directory already exists, then it attempt to create the sub-directories
initMailStorageDir :: FilePath -> IO MailStorage
initMailStorageDir dir = do
    isDir <- doesDirectoryExist dir
    when (not isDir) $ createDirectory dir
    createMailStorageSubDir getMandatorySubDir
    return $ MailStorage dir
                         (dir </> getIncomingDir)
                         (dir </> getForDeliveryDir)
                         (dir </> getUserAccountDir)
                         []
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
fromIncomingToFordelivery :: MailStorage    -- ^ MailStorageDirectory
                          -> FilePath    -- ^ filename
                          -> IO ()
fromIncomingToFordelivery config filename = renameFile inCommingPath forDeliveryPath
    where
        inCommingPath   = (incomingDir    config) </> filename
        forDeliveryPath = (forDeliveryDir config) </> filename
    
------------------------------------------------------------------------------
--                                  User's mailbox                          --
------------------------------------------------------------------------------

data MailUser = MailUser
    { emailAddr   :: String   -- ^ user's local-part address (local-part@server.foo)
    , firstName   :: String   -- ^ user first name
    , lastName    :: String   -- ^ user last name
    , mailBoxPath :: FilePath -- ^ absolute file path to user's MailDir (/home/bar/mails)
    } deriving (Eq, Show, Read)

type MailUserS a = StateT MailUser IO a

parseUserContent :: [String] -> MailUserS ()
parseUserContent []     = return ()
parseUserContent [line] = parseUserContentLine line
parseUserContent (l:ls) = parseUserContentLine l >> parseUserContent ls

-- TODO: write a better parser using attoparsec
parseUserContentLine :: String -> MailUserS ()
parseUserContentLine line =
    case span (\c -> c /= '=') $ line of
        ("firstname", '=':r) -> modify $ \s -> s { firstName = read r }
        ("lastname",  '=':r) -> modify $ \s -> s { lastName  = read r }
        ("path",      '=':r) -> modify $ \s -> s { mailBoxPath = read r }
        e                    -> error $ "unexpected line: " ++ (show line) ++ " --> " ++ (show e)

parseMailUser :: FilePath -> IO MailUser
parseMailUser filepath = do
    contentLines <- readFile filepath >>= \contents -> return $ lines contents
    (_, userMail) <- runStateT (parseUserContent contentLines) (MailUser addr [] [] [])
    return userMail
    where
        addr :: FilePath
        addr = takeFileName filepath

getMailUserAccounts :: MailStorage -> IO [MailUser]
getMailUserAccounts (MailStorage _ _ _ dir _) = do
    users <- getDirectoryContents dir >>= \l -> return $ filter (\e -> e /= "." && e /= "..") l
    mapM (\u -> parseMailUser $ dir </> u) users

-- | find the list of user that may correspond to the given string:
-- white-space sensitive
-- not case-sensitive
findMailUsers :: String -> MailStorage -> [MailUser]
findMailUsers s (MailStorage _ _ _ _ list) =
    filter filterHelper list
    where
        filterHelper :: MailUser -> Bool
        filterHelper user = (map toLower s) `elem` (listToCheck user)

        listToCheck user =
            [ map toLower $ emailAddr user
            , map toLower $ firstName user
            , map toLower $ lastName user
            ]

