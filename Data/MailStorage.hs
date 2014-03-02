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
    , getDomainsDir
    , generateUniqueFilename
      -- * General
    , MailStorage(..)
    , isMailStorageDir
    , initMailStorageDir
    , getMailStorage
    , fromIncomingToFordelivery
      -- * Domains
    , listDomains
    , isSupportedDomain
    , isLocalPartOf
    , isLocalAddress
    , getMailStorageUser
    , findMailStorageUsers
    ) where

import Network.SMTP.Types (EmailAddress(..), Domain, LocalPart, MailStorageUser(..))

import qualified Crypto.Hash as Hash

import Control.Monad       (when)
import Control.Monad.State

import System.FilePath  (FilePath, (</>), takeFileName)
import System.Directory (getDirectoryContents, doesDirectoryExist, doesFileExist, createDirectory, renameFile)
import System.Random    (getStdRandom, randomR)

import System.Hourglass (timeCurrent)
import Data.Hourglass   (Elapsed, timePrint, ISO8601_DateAndTime(..))
import Data.Char        (isSpace, toUpper, toLower)
import Data.Maybe       (catMaybes)

import qualified Data.ByteString.Char8 as BC (unpack, pack, ByteString)

------------------------------------------------------------------------------
--                              Mail Storages                               --
------------------------------------------------------------------------------

getIncomingDir :: FilePath
getIncomingDir = "incoming"

getForDeliveryDir :: FilePath
getForDeliveryDir = "for-delivery"

getUsersDir :: FilePath
getUsersDir = "users"

getDomainsDir :: FilePath
getDomainsDir = "domains"

-- | list the mandatory directory needed in a MailStorageDir
getMandatorySubDir :: [FilePath]
getMandatorySubDir =
    [ getIncomingDir
    , getForDeliveryDir
    , getDomainsDir
    , getUsersDir
    ]

data MailStorage = MailStorage
    { mainDir        :: FilePath
    , incomingDir    :: FilePath
    , forDeliveryDir :: FilePath
    , usersDir       :: FilePath
    , domainsDir     :: FilePath
    } deriving (Eq)

-- | get mailStorage:
getMailStorage :: FilePath -> IO (Maybe MailStorage)
getMailStorage dir = do
    isDir <- isMailStorageDir dir
    return $ case isDir of
        False -> Nothing
        True  -> Just $ MailStorage
                                dir
                                (dir </> getIncomingDir)
                                (dir </> getForDeliveryDir)
                                (dir </> getUsersDir)
                                (dir </> getDomainsDir)

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
                         (dir </> getUsersDir)
                         (dir </> getDomainsDir)
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

-- It is time to define user properly:
-- what do we want?
-- The mailDir:
-- some/path:
--   |
--   |-- incoming <- a temporary area to store the incoming mail (every *DATA*)
--   |
--   |-- for-delivery <- incoming mail will be moved to this dir for delivery
--   |
--   |-- users <-- users
--   |     |
--   |     |-- nicolas <- file which list all the domains/localpart
--   |     |    |
--   |     |    ` - firstname, lastname, digest, list of <localpart@domains>
--   |     |
--   |     `-- .. <- others users
--   |
--   |
--   `-- domains
--         |
--         |-- di-prima.fr
--         |    |
--         |    `-- nicolas <-- nicolas@di-prima.fr
--         |          |
--         |          `--.config <- user config
--         |
--         |-- mail.di-prima.fr
--         |    |
--         |    `-- nicolas <-- nicolas@mail.di-prima.fr
--         |          |
--         |          `.config
--         |
--         `-- di-prima.io
--              |
--              `-- git <-- git@di-prima.fr
--                    |
--                    `.config

defaultMailStorageUser :: MailStorageUser
defaultMailStorageUser = MailStorageUser [] "" "" ""

isSupportedDomain :: MailStorage -> Domain -> IO Bool
isSupportedDomain ms d =
    let domains = domainsDir ms
        domain = domains </> d
    in  doesDirectoryExist domain

listDomains :: MailStorage -> IO [Domain]
listDomains ms =
    (getDirectoryContents $ domainsDir ms) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

listMailBoxs :: MailStorage -> Domain -> IO [LocalPart]
listMailBoxs ms d =
    (getDirectoryContents $ (domainsDir ms) </> d) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

listUsers :: MailStorage -> IO [FilePath]
listUsers ms =
    (getDirectoryContents $ usersDir ms) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

isLocalPartOf :: MailStorage -> Domain -> LocalPart -> IO Bool
isLocalPartOf ms d l =
    let domains = domainsDir ms
        domain = domains </> d
        mailBoxPath = domain </> l
    in  doesDirectoryExist mailBoxPath

isLocalAddress :: MailStorage -> EmailAddress -> IO Bool
isLocalAddress ms (EmailAddress d l) = isLocalPartOf ms d l

findMailStorageUsers :: MailStorage -> String -> IO [MailStorageUser]
findMailStorageUsers ms s = do
    l <- listUsers ms
    lmuser <- mapM (getMailStorageUser ms) l
    return $ filter findMailStorageUser $ catMaybes lmuser
    where
        exp :: String
        exp = map toUpper s
        findMailStorageUser :: MailStorageUser -> Bool
        findMailStorageUser msu
            =  (map toUpper $ firstName msu) == exp
            || (map toUpper $ lastName msu)  == exp
            || checkLocalEmails (emails msu)

        checkLocalEmails :: [EmailAddress] -> Bool
        checkLocalEmails [] = False
        checkLocalEmails ((EmailAddress l _):xs) =
            if (map toUpper l) == exp
                then True
                else checkLocalEmails xs

-- | get a user configuration
getMailStorageUser :: MailStorage -> FilePath -> IO (Maybe MailStorageUser)
getMailStorageUser ms login =
    let userFile = (usersDir ms) </> login
    in  do isFile <- doesFileExist userFile
           if isFile
                then parseMailUser userFile >>= \u -> return $ Just u
                else return Nothing

type MailStorageUserS a = StateT MailStorageUser IO a

parseUserContent :: [String] -> MailStorageUserS ()
parseUserContent []     = return ()
parseUserContent [line] = parseUserContentLine line
parseUserContent (l:ls) = parseUserContentLine l >> parseUserContent ls

-- TODO: write a better parser using attoparsec
parseUserContentLine :: String -> MailStorageUserS ()
parseUserContentLine line =
    case span (\c -> c /= '=') $ line of
        ("firstname", '=':r) -> modify $ \s -> s { firstName = read r }
        ("lastname",  '=':r) -> modify $ \s -> s { lastName  = read r }
        ("password",  '=':r) -> modify $ \s -> s { userDigest = read r }
        ("address",   '=':r) -> parseUserAddress r
        e                    -> error $ "unexpected line: " ++ (show line) ++ " --> " ++ (show e)
    where
        parseUserAddress :: String -> MailStorageUserS ()
        parseUserAddress s =
            modify $ \s -> s { emails = addr:(emails s) }
            where
                (local, pDom) = span (\c -> c /= '@') s
                dom = drop 1 pDom
                addr = EmailAddress local dom

parseMailUser :: FilePath -> IO MailStorageUser
parseMailUser filepath = do
    contentLines <- readFile filepath >>= \contents -> return $ lines contents
    (_, userMail) <- runStateT (parseUserContent contentLines) defaultMailStorageUser
    return userMail
