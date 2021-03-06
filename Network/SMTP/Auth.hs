-- |
-- Module      : Network.SMTP.Auth
-- License     : BSD-style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Network.SMTP.Auth
    (
      clientAuth
    , serverAuthPlain 
    ) where

import Network.SMTP.Types

import qualified Data.ByteString.Base64 as B64 (encode, decode)
import qualified Data.ByteString.Char8  as BC

-- | identify a user with the PLAIN method
clientPlain :: UserName -> Password -> BC.ByteString
clientPlain user passwd =
    B64.encode $ BC.intercalate (BC.pack "\0")
                    [ user
                    , passwd
                    ]

-- | auth to a server
clientAuth :: AuthType
           -> BC.ByteString
           -> UserName
           -> Password
           -> BC.ByteString
clientAuth PLAIN    _ u p = clientPlain u p
clientAuth LOGIN    _ _ _ = undefined
clientAuth CRAM_MD5 _ _ _ = undefined

-- | auth a client
serverAuthPlain :: BC.ByteString
                -> Either String (UserName, Password)
serverAuthPlain buff =
    case B64.decode buff of
        Left err -> Left err
        Right bs ->
            let (login, pw) = BC.span (\c -> c /= '\0') bs
            in  Right (login, BC.drop 1 pw)
