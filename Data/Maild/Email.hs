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

data EmailAddress = EmailAddress LocalPart Domain
    deriving (Eq)

instance Show EmailAddress where
    show (EmailAddress local domain) = local ++ "@" ++ domain

------------------------------------------------------------------------------
--                           From/To Path                                   --
------------------------------------------------------------------------------

-- | This format SHOULD not be used, but keep it for compatibility with RFC821
-- See RFC5321, appendix C
data Path = Path [Domain] EmailAddress
    deriving (Show, Eq)

-- | A minor tool to be able to show a Path
showPath :: Path -> String
showPath (Path adl from) = "<" ++ showADL ++ (show from) ++ ">"
    where
        showADL :: String
        showADL = if null adl then "" else showADL' adl

        showADL' :: [String] -> String
        showADL' [ad]    = "@" ++ (show ad) ++ ":"
        showADL' (ad:xs) = "@" ++ (show ad) ++ "," ++ (showADL' xs)

type ForwardPath = Path
type ReversePath = Path

------------------------------------------------------------------------------
--                               Email                                      --
------------------------------------------------------------------------------

-- | contains all the needed data to receive/forward emails
data Email = Email
    { mailFrom     :: Maybe ReversePath     -- ^ the email address of the sender
    , mailTo       :: [ForwardPath]         -- ^ the recipients
    , mailData     :: FilePath              -- ^ the data's filename
    } deriving (Show, Eq)

