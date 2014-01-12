-- |
-- Module      : MailStorage
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
-- Just provide a simple way to store Emails and manage users
--   
module MailStorage
    ( -- * User management
      User(..)
    , getUser
    , storeAnEmail
    ) where

-- import qualified Crypto.Hash.SHA1 as SHA1
import qualified Crypto.Hash as Hash

import System.FilePath
import System.Directory

import Network.SMTP.Types (Email(..))

import System.Hourglass (timeCurrent)
import Data.Hourglass (Elapsed)
import qualified Data.ByteString.Char8 as BC

------------------------------------------------------------------------------
--                              Users Management                            --
------------------------------------------------------------------------------

-- | Represent user informations:
-- the architecture is quiet simple:
--
-- > nicolas
-- >  |
-- >  |-- mails   -- the mails directory
-- >  |
-- >  `-- .digest -- the digest file
data User = User
    { userLogin  :: String   -- ^ login (the name of it's dicrectory)
    , emailPath  :: FilePath -- ^ the path to access the user's mails
    , userDigest :: FilePath -- ^ the path to the identification information
    } deriving (Show, Read, Eq)

-- | Get User information
-- If it returns Nothing, then the user doesn't exist or is not a initialized
getUser :: FilePath        -- ^ the directory where all the users are
        -> FilePath        -- ^ the user name
        -> IO (Maybe User) -- ^ return the User if it exists
getUser dir name = do
    lUser <- getDirectoryContents dir
    if name `elem` lUser
        then buildUser
        else return Nothing
    where
        userPathDir   = dir </> name
        userEmailsDir = userPathDir </> "mails"
        userDigestDir = userPathDir </> ".digest"

        buildUser :: IO (Maybe User)
        buildUser = do
            lContent <- getDirectoryContents userPathDir
            if not ("mails" `elem` lContent) || not (".digest" `elem` lContent)
                then return Nothing
                else return $ Just $ User name userEmailsDir userDigestDir

-- | Store an email
--
-- will store the email in the given recipient directory
-- email name convention: <reception-epoch>-<domain-client>-<email-from>-<sha1-data>-
storeAnEmail :: User  -- ^ the recipient
             -> Email -- ^ the email
             -> IO ()
storeAnEmail user email = do
    time <- timeCurrent
    BC.writeFile (filepath time) (mailData email)
    where
        hash time = Hash.digestToHexByteString (Hash.hash (mailData email) :: Hash.Digest Hash.SHA1)

        filepath time = (emailPath user) </> (filename time)

        filename :: Elapsed -> FilePath
        filename time =    (show time)
                        ++ "-" ++ (mailFrom email)
                        ++ "-" ++ (mailClient email)
                        ++ "-" ++ (BC.unpack $ hash time)
                        ++ "-"
