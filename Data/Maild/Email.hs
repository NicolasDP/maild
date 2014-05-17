-- |
-- Module      : Data.Maild.Email
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Data.Maild.Email
    ( -- * Email address
      Domain
    , LocalPart
    , EmailAddress(..)
      -- * Path
    , Path(..)
    , showPath
    , ForwardPath(..)
    , ReversePath(..)
      -- * Email
    , Email(..)
    ) where

------------------------------------------------------------------------------
--                               Email Address                              --
------------------------------------------------------------------------------

-- | Represent a domain name:
-- > foo.com
-- > haskell.org
type Domain = String
-- | Represent the local part of an email address:
-- > local-part@domain.net
type LocalPart = String

-- | Email address
--
-- > localpart@domain.net
data EmailAddress = EmailAddress
    { localpart  :: LocalPart
    , domainpart :: Domain
    } deriving (Eq)

instance Show EmailAddress where
    show (EmailAddress local domain) = local ++ "@" ++ domain

------------------------------------------------------------------------------
--                           From/To Path                                   --
------------------------------------------------------------------------------

-- | RFC5321 (4.1.2) specified a Path is a list of domains and an email address
data Path = Path
    { paths   :: [Domain]     -- ^ might be useless since it is deprecated
    , address :: EmailAddress
    } deriving (Show, Eq)

-- | A minor tool to be able to show a Path
showPath :: Path -> String
showPath (Path adl from) = "<" ++ showADL ++ (show from) ++ ">"
    where
        showADL :: String
        showADL = if null adl then "" else showADL' adl

        showADL' :: [String] -> String
        showADL' [ad]    = "@" ++ ad ++ ":"
        showADL' (ad:xs) = "@" ++ ad ++ "," ++ (showADL' xs)

-- | As specified in RFC5321 (4.1.2), a reverse path is a Path
type ReversePath = Maybe Path
-- | As specified in RFC5321 (4.1.2), a forward path is a Path
type ForwardPath = Path

------------------------------------------------------------------------------
--                               Email                                      --
------------------------------------------------------------------------------

-- | contains all the needed data to receive/forward emails
data Email = Email
    { mailFrom     :: Maybe ReversePath     -- ^ the email address of the sender
    , mailTo       :: [ForwardPath]         -- ^ the recipients
    , mailData     :: FilePath              -- ^ the data's filename
    } deriving (Show, Eq)

