module Main where

import Test.QuickCheck
import Butters.Parser

data ParserTypes =
  TopLevel String

parsesTo (TopLevel text) = (== (parseTopLevel text)) . Right

test_top_level_data_single_constructor_parses :: Bool
test_top_level_data_single_constructor_parses =
  TopLevel "data Zero | []" `parsesTo` DataDef "Zero" [] [BList []]

prop_arbitrary_depth_single_constructor_parser :: Int -> Property
prop_arbitrary_depth_single_constructor_parser n =
  n > 0 ==> (TopLevel $ "data Name | " ++ nestedNTimes) `parsesTo` DataDef "Name" [] expectedBListNesting
  where
    nestedNTimes = foldr (\_ o -> "[" ++ o ++ "]") "" [1..n]
    expectedBListNesting = [foldr (\_ o -> BList [o]) (BList []) [2..n]]

main :: IO ()
main = do
  quickCheck test_top_level_data_single_constructor_parses
  quickCheck prop_arbitrary_depth_single_constructor_parser 
