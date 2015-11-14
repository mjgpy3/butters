module Main where

import Test.QuickCheck
import Butters.Parser

data ParserTypes =
  TopLevel String

parsesTo (TopLevel text) = (== (parseTopLevel text)) . Right

test_top_level_data_single_constructor_parses :: Bool
test_top_level_data_single_constructor_parses =
  TopLevel "data Zero | []" `parsesTo` DataDef "Zero" [] [BList []]

main :: IO ()
main =
  quickCheck test_top_level_data_single_constructor_parses
