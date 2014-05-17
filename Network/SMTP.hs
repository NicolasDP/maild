-- |
-- Module      : Network.SMTP
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP
    ( -- * SMTP Server
      -- ** Notifications
      SMTPChan
    , newSMTPChan
    , getNextEmail
      -- ** Configuration
    , SMTPConfig(..)
      -- ** Main method
    , acceptClient
    , rejectClient
      -- * SMTP Client
    , smtpOpenConnection
    , smtpCloseConnection
    , smtpSendEmail
    , smtpSendCommand
    ) where

import Network.SMTP.Server
import Network.SMTP.Client
