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

genStringWithDot :: Gen String
genStringWithDot = do
  ext <- vectorOf 2 $ elements elChar :: Gen String
  s <- choose (1, 5) :: Gen Int
  l <- vectorOf s $ genString 16 :: Gen [String]
  return $ intercalate "." (l ++ [ext])

genString :: Int -> Gen String
genString max = do
  l <- choose (2, max) :: Gen Int
  vectorOf l $ elements elToChoose

genMaybeString :: Int -> Gen (Maybe String)
genMaybeString max = do
  s <- genString 8
  elements [Just s, Nothing]

elToChoose :: [Char]
elToChoose = elChar ++ elInt

elChar :: [Char]
elChar = ['a'..'z']
elInt :: [Char]
elInt = ['0'..'9']
elOther :: [Char]
elOther = ['-']

generateHELO :: Gen Command
generateHELO = HELO <$> genStringWithDot

generateEHLO :: Gen Command
generateEHLO = EHLO <$> genStringWithDot

generateEXPN :: Gen Command
generateEXPN = EXPN <$> genString 32

generateVRFY :: Gen Command
generateVRFY = VRFY <$> genString 32

generateHELP :: Gen Command
generateHELP = HELP <$> genMaybeString 16
generateNOOP :: Gen Command
generateNOOP = NOOP <$> genMaybeString 16

generateAUTH :: Gen Command
generateAUTH = AUTH <$> (elements [PLAIN, LOGIN, CRAM_MD5]) <*> (genMaybeString 128)

generateINVALCMD :: Gen Command
generateINVALCMD = VRFY <$> genString 128

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
  k <- genString 16
  v <- genMaybeString 54
  return (k, v)

generatePath :: Gen Path
generatePath = Path <$> genPaths <*> genAddress
  where
    genPaths :: Gen [Domain]
    genPaths = listOf genStringWithDot

    genAddress :: Gen EmailAddress
    genAddress = EmailAddress <$> genStringWithDot <*> genStringWithDot
 
generateCommand' :: Gen Command
generateCommand' = elements [DATA, RSET, QUIT] -- no need for TIMEOUT or INVALIDCMD

generateCommand :: Gen Command
generateCommand =
  oneof
    [ generateHELO, generateEHLO, generateEXPN, generateVRFY
    , generateHELP, generateNOOP, generateAUTH, generateINVALCMD
    , generateMAIL, generateRCPT, generateCommand']

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

instance Arbitrary Command where
  arbitrary = generateCommand
