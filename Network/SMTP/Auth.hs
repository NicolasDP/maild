-- |
-- Module      : Network.SMTP.Auth
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Auth
    ( UserName
    , Password
    , AuthType(..)
    ) where

import qualified Data.ByteString.Char8 as BC

type UserName = BC.ByteString
type Password = BC.ByteString
data AuthType
    = PLAIN
    | LOGIN
    | CRAM_MD5
    deriving (Read, Show, Eq)
