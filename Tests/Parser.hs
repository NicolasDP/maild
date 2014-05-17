-- |
-- Module      : Tests.Parsers
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
module Parser where

import Control.Applicative
import Test.QuickCheck

import qualified Data.Map.Strict as Map
import Data.List (intercalate)

import Network.SMTP.Types
import Network.SMTP.Parser
import Data.Maild.Email

------------------------------------------------------------------------------
--                        Generate Random Commands                          --
------------------------------------------------------------------------------

-- Char and list of elements used for generators

charLo :: [Char]
charLo = ['a'..'z']
charUp :: [Char]
charUp = ['A'..'Z']
charLoAndUp :: [Char]
charLoAndUp = charLo ++ charUp
charNum :: [Char]
charNum = ['0'..'9']
charSpecial :: [Char]
charSpecial = ['-']

alphaNum :: [Char]
alphaNum = charLoAndUp ++ charNum

-- Generic generators

genUpperString :: Int -> Gen String
genUpperString max = do
   l <- choose (2, max) :: Gen Int
   vectorOf l $ elements charUp

genAlphaNumString :: Int -> Gen String
genAlphaNumString max = do
  l <- choose (2, max) :: Gen Int
  vectorOf l $ elements alphaNum

genMaybeAlphaNumString :: Int -> Gen (Maybe String)
genMaybeAlphaNumString max = do
  s <- genAlphaNumString 8
  elements [Just s, Nothing]

genDomainString :: Int -> Gen String
genDomainString max = do
  l <- choose (2, max) :: Gen Int
  vectorOf l $ elements $ alphaNum ++ charSpecial

genDNS :: Gen String
genDNS = do
  ext <- vectorOf 2 $ elements charLo :: Gen String
  s <- choose (1, 5) :: Gen Int
  l <- vectorOf s $ genDomainString 16 :: Gen [String]
  return $ intercalate "." (l ++ [ext])

-- Command generators

generateHELO :: Gen Command
generateHELO = HELO <$> genDNS

generateEHLO :: Gen Command
generateEHLO = EHLO <$> genDNS

generateEXPN :: Gen Command
generateEXPN = EXPN <$> genAlphaNumString 32

generateVRFY :: Gen Command
generateVRFY = VRFY <$> genAlphaNumString 32

generateHELP :: Gen Command
generateHELP = HELP <$> genMaybeAlphaNumString 16
generateNOOP :: Gen Command
generateNOOP = NOOP <$> genMaybeAlphaNumString 16

generateAUTH :: Gen Command
generateAUTH = AUTH <$> (elements [PLAIN, LOGIN, CRAM_MD5]) <*> (genMaybeAlphaNumString 128)

generateINVALCMD :: Gen Command
generateINVALCMD = INVALCMD <$> genUpperString 16

generateMAIL :: Gen Command
generateMAIL = MAIL <$> generateMaybePath <*> generateParameters

generateRCPT :: Gen Command
generateRCPT = RCPT <$> generatePath <*> generateParameters

generateMaybePath :: Gen (Maybe Path)
generateMaybePath = do
  p <- generatePath
  elements [Just p, Nothing]

generateParameters :: Gen ESMTPParameters
generateParameters = do
  l <- listOf generateParameter
  return $ Map.fromList l

generateParameter :: Gen (ESMTPKeyWord, Maybe ESMTPValue)
generateParameter = do
  k <- genAlphaNumString 16
  v <- genMaybeAlphaNumString 54
  return (k, v)

generatePath :: Gen Path
generatePath = Path <$> genPaths <*> genAddress
  where
    genPaths :: Gen [Domain]
    genPaths = listOf genDNS

    genAddress :: Gen EmailAddress
    genAddress = EmailAddress <$> genDNS <*> genDNS
 
generateCommand :: Gen Command
generateCommand =
  oneof
    [ generateHELO
    , generateEHLO
    , generateMAIL
    , generateRCPT
    , generateEXPN
    , generateVRFY
    , generateHELP
    , generateNOOP
    , generateAUTH
    , generateINVALCMD
    , generateCommand'
    ]
  where
    generateCommand' :: Gen Command
    generateCommand' = elements [DATA, RSET, QUIT]

------------------------------------------------------------------------------
--                                 Arbitrary                                --
------------------------------------------------------------------------------

instance Arbitrary Command where
  arbitrary = generateCommand

------------------------------------------------------------------------------
--                             Property checkers                            --
------------------------------------------------------------------------------

prop_parse_smtp_command :: Command -> Bool
prop_parse_smtp_command cmd =
  case parseCommandString $ cmdStr ++ "\r\n" of
    Left err -> error $ err ++ cmdStr
    Right c  ->
      if cmd == c
        then True
        else error $ "Error:\nString:" ++ cmdStr ++ "\nOriginal:" ++ (show cmd) ++ "\nparsed: " ++ (show c)
  where
    cmdStr :: String
    cmdStr = showCommand cmd
