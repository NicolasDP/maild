-- |
-- Module      : DeliveryManager
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module DeliveryManager
    ( -- Types
      DeliveryManager(..)
      -- * DeliverChan
    , DeliveryChan
    , newDeliveryChan
    , deliverEmail
    , getNextEmailToDeliver
      -- * delivering methods
      -- ** to local mailboxes
    , deliverEmailToLocalRCPT
    ) where

import Data.MailStorage (MailStorage(..), isLocalAddress)
import Network.SMTP.Types
import Control.Concurrent.STM

import System.FilePath  (FilePath, (</>))
import System.Directory (createDirectoryIfMissing, copyFile)

data DeliveryManager = DeliveryManager
    { mailStorageDir :: MailStorage
    }

------------------------------------------------------------------------------
--                               DeliveryChan                               --
------------------------------------------------------------------------------

-- | A channel used to be notified each time Email has been received
type DeliveryChan = TChan Email

-- | a helper to create a DeliveryChan
newDeliveryChan :: IO (DeliveryChan)
newDeliveryChan = atomically (newTChan)

-- | internal command: publish the received email
deliverEmail :: DeliveryChan -> Email -> IO ()
deliverEmail chan = atomically . writeTChan chan

-- | a helper to wait and then get an Email.
getNextEmailToDeliver :: DeliveryChan -> IO Email
getNextEmailToDeliver = atomically . readTChan

------------------------------------------------------------------------------
--                               DeliveryMethods                            --
------------------------------------------------------------------------------

-- | method to deliver emails to local mailboxes
-- it returns the same emails but without the local RCPT
deliverEmailToLocalRCPT :: DeliveryManager
                        -> Email      -- ^ the email
                        -> IO (Email) -- ^ the email without local mailboxes
deliverEmailToLocalRCPT dmc email = do
    remainingPaths <- deliverToLocalUsers (mailStorageDir dmc) (mailTo email)
    return $ email { mailTo = remainingPaths }
    where
        deliverToLocalUsers _  []                       = return $ []
        deliverToLocalUsers ms ((Path useless addr):xs) = do
            isLocal <- isLocalAddress ms addr
            if isLocal
                then deliverToLocalUser ms email addr >> deliverToLocalUsers ms xs
                else do acc <- deliverToLocalUsers ms xs
                        return $ (Path useless addr):acc

        deliverToLocalUser ms email (EmailAddress local domain) = do
            createDirectoryIfMissing False deliveryDir
            copyFile dataFilePath (deliveryDir </> (mailData email))
            where
                dataFilePath = (forDeliveryDir ms) </> (mailData email)
                deliveryDir  = (domainsDir ms) </> domain </> local </> "new"
