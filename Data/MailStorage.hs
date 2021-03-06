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
--
{-# Language OverloadedStrings #-}
module Data.MailStorage
    ( -- * helpers
      getIncomingDir
    , getForDeliveryDir
    , getDomainsDir
    , generateUniqueFilename
    , createIncomingDataFile
      -- * General
    , MailStorage(..)
    , isMailStorageDir
    , initMailStorageDir
    , getMailStorage
    , fromIncomingToFordelivery
    , deleteDataFromDeliveryDir
      -- * Domains
    , listDomains
    , isSupportedDomain
    , isLocalPartOf
    , isLocalAddress
    , getMailStorageUser
    , findMailStorageUsers
    ) where

import Network.SMTP.Types
import Network.SMTP.Connection (ConnectionID)
import Data.Maild.Email

import qualified Crypto.Hash as Hash

import Control.Monad       (when)

import System.FilePath  (FilePath, (</>), takeFileName)
import System.Directory
import System.Random    (getStdRandom, randomR)

import System.Hourglass (timeCurrent)
import Data.Hourglass
import Data.Char        (isSpace, toUpper, toLower)
import Data.Maybe       (catMaybes)
import Data.Configurator
import Data.Configurator.Types

import qualified Data.ByteString.Char8 as BC (unpack, pack, ByteString, writeFile)

------------------------------------------------------------------------------
--                              Mail Storages                               --
------------------------------------------------------------------------------

-- | the name of the directory used to store the incoming emails
getIncomingDir :: FilePath
getIncomingDir = "incoming"

-- | the name of the directory used to store the "Ready for delivery" emails
getForDeliveryDir :: FilePath
getForDeliveryDir = "for-delivery"

-- | the name of the directory used to user's configuration files
getUsersDir :: FilePath
getUsersDir = "users"

-- | the name of the directory used to manage the domains and their localpart
-- a domain is a directory (the domain name is the directory name)
-- each domain may contains localpart. A local part is a subdirectory of this domain.
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

-- | Configuration for MailStorage
data MailStorage = MailStorage
    { mainDir        :: FilePath
    , incomingDir    :: FilePath
    , forDeliveryDir :: FilePath
    , usersDir       :: FilePath
    , domainsDir     :: FilePath
    } deriving (Show, Eq)

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
-- <ISO8601-date>_<SHA3 of @client domain@ @from address@ @current time@ @random number@>_<connectionId)
generateUniqueFilename :: String -- ^ client domain
                       -> String -- ^ from address
                       -> ConnectionID -- ^ Connection ID
                       -> IO (FilePath)
generateUniqueFilename client from conId = do
    time <- timeCurrent
    random <- getStdRandom (randomR (1, 9999999)) :: IO Int
    return $ (timePrint getISOTimeFormat time)
             ++ "_" ++ (BC.unpack $ getHash $ randomThing (show time) (show random))
             ++ "_" ++ conId
    where
        getISOTimeFormat = ISO8601_DateAndTime
        getHash :: BC.ByteString -> BC.ByteString
        getHash buff = Hash.digestToHexByteString $ (Hash.hash buff :: Hash.Digest Hash.SHA3_224)

        randomThing :: String -> String -> BC.ByteString
        randomThing t r = BC.pack $ t ++ client ++ from ++ r

-- | in the case the server is receiving a new MAIL. We are required to add
-- a MIME Header "Received:".
createIncomingDataFile :: MailStorage
                       -> Domain
                       -> Domain
                       -> Maybe SMTPType
                       -> Email
                       -> IO ()
createIncomingDataFile ms by from mtype email = do
    time <- timeCurrent >>= \t -> return $ timePrint myTimeFormat t
    let receivedString = "Received: " ++ fromDomainString ++ withString
                        ++ cwfs ++ byDomainString ++ "; " ++ time
                        ++ "\r\n"
    BC.writeFile inComingPath $ BC.pack receivedString
    where
        inComingPath = (incomingDir ms) </> (mailData email)
        wsp :: Char
        wsp = ' '
        cwfs :: String
        cwfs = "\r\n  "

        fromDomainString = "From " ++ from
        withString = maybe "" (\t -> " With " ++ (show t)) mtype
        byDomainString   = "By "   ++ by

        myTimeFormat :: TimeFormatString
        myTimeFormat =
            TimeFormatString
                [ Format_Day2
                , Format_Text wsp
                , Format_MonthName_Short
                , Format_Text wsp
                , Format_Year4
                , Format_Text wsp
                , Format_Hour
                , Format_Text ':'
                , Format_Minute
                , Format_Text ':'
                , Format_Second
                , Format_Text wsp
                , Format_TzHM
                , Format_Text wsp
                , Format_Text '('
                , Format_TimezoneName
                , Format_Text ')'
                ]

-- | move a file from the "bufferisation" area to the
-- "wait for filtering/delivering" area
--
-- This action also add the "time-stamp-line" as specified in RFC5321 section
-- 4.1.1.4 (and discribed in section 4.4)
fromIncomingToFordelivery :: MailStorage    -- ^ MailStorageDirectory
                          -> Email          -- ^ the email to move for delivery
                          -> IO ()
fromIncomingToFordelivery ms email = renameFile inComingPath forDeliveryPath
    where
        inComingPath    = (incomingDir    ms) </> (mailData email)
        forDeliveryPath = (forDeliveryDir ms) </> (mailData email)

-- | Delete a file email from the Delivery Directory (you may lost of the data)
deleteDataFromDeliveryDir :: MailStorage
                          -> Email
                          -> IO ()
deleteDataFromDeliveryDir ms email = removeFile filepath
    where
        filepath = (forDeliveryDir ms) </> (mailData email)

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

-- | list the domains supported
listDomains :: MailStorage -> IO [Domain]
listDomains ms =
    (getDirectoryContents $ domainsDir ms) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

-- | list the mailbox in the given domain
listMailBoxs :: MailStorage -> Domain -> IO [LocalPart]
listMailBoxs ms d =
    (getDirectoryContents $ (domainsDir ms) </> d) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

-- | list users
listUsers :: MailStorage -> IO [FilePath]
listUsers ms =
    (getDirectoryContents $ usersDir ms) >>= \xs -> return $ filter (\x -> notElem x [".", ".."]) xs

-- | check if a domain is in the list of managed domains
isSupportedDomain :: MailStorage -> Domain -> IO Bool
isSupportedDomain ms d =
    let domains = domainsDir ms
        domain = domains </> d
    in  doesDirectoryExist domain

-- | check if a localpart is a localpart of the given domain
isLocalPartOf :: MailStorage -> Domain -> LocalPart -> IO Bool
isLocalPartOf ms d l =
    let domains = domainsDir ms
        domain = domains </> d
        mailBoxPath = domain </> l
    in  doesDirectoryExist mailBoxPath

-- | Check if an EmailAdress is a local address
--
-- > isLocalAddress ms == isLocalPartOf ms
isLocalAddress :: MailStorage -> EmailAddress -> IO Bool
isLocalAddress ms (EmailAddress l d) = isLocalPartOf ms d l

-- | return the list of users who are associated to the given string
-- (check the firstname, lastname and the email)
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
                then parseMailStorageUserFile userFile >>= \u -> return $ Just u
                else return Nothing

-- | Read a User configuration file
parseMailStorageUserFile :: FilePath -> IO MailStorageUser
parseMailStorageUserFile filepath = do
    conf <- load [Required filepath]
    firstname <- require conf "firstname"
    lastname <- require conf "lastname"
    password <- require conf "password"
    address <- require conf "address" >>= \(List lvalues) -> return $ map parseUserAddress $ catMaybes $ map convert lvalues
    return $ MailStorageUser
                { emails = address
                , firstName = firstname
                , lastName = lastname
                , userDigest = password
                }
    where
        parseUserAddress :: String -> EmailAddress
        parseUserAddress s = addr
            where
                (local, pDom) = span (\c -> c /= '@') s
                dom = drop 1 pDom
                addr = EmailAddress local dom
