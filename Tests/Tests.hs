-- |
-- Module      : Tests.Tests
-- License     : BSD-Style
--
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : unknown
--
import Test.QuickCheck
import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Parser

main :: IO ()
main =
  defaultMain
    [ tests_parser_command
    ]
  where
    tests_parser_command = testGroup "parser"
      [ testProperty "command parser" prop_parse_smtp_command
      ]
