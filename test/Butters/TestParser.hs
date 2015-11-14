module Main where

import Butters.Parser
import Control.Monad
import Data.Either
import Test.QuickCheck

data Parsable =
  TopLevel String

parsableText :: Parsable -> String
parsableText (TopLevel t) = t

parsesTo (TopLevel text) = (==) (parseTopLevel text) . Right

failsToParse = isLeft . parseTopLevel . parsableText

single_constructor_text :: String -> String -> Parsable
single_constructor_text name cons = TopLevel $ "data " ++ name ++ " | " ++ cons

prop_arbitrary_depth_single_constructor_parser :: Int -> Property
prop_arbitrary_depth_single_constructor_parser n =
  n > 0 ==> single_constructor_text "Name" nestedNTimes `parsesTo` DataDef "Name" [] expectedBListNesting
  where
    nestedNTimes = foldr (\_ o -> "[" ++ o ++ "]") "" [1..n]
    expectedBListNesting = [foldr (\_ o -> BList [o]) (BList []) [2..n]]

prop_data_constructors_can_begin_with_capital_letter :: Char -> Property 
prop_data_constructors_can_begin_with_capital_letter l =
  l `elem` ['A'..'Z']
    ==> single_constructor_text (l:"oobar") "[]" `parsesTo` DataDef (l:"oobar") [] [BList []]

prop_data_constructor_names_can_have_arbitrary_length :: Int -> Bool
prop_data_constructor_names_can_have_arbitrary_length n =
  single_constructor_text constructorName "[]" `parsesTo` DataDef constructorName [] [BList []]
  where
    constructorName ='A':(replicate (abs n) 'z')

examples = [
  (TopLevel "data Zero | []", DataDef "Zero" [] [BList []], "Zero Constructor")
  ,(TopLevel "data Bool | [] | [[]]", DataDef "Bool" [] [BList [], BList [BList []]], "Bool Constructor")
  ]

main :: IO ()
main = do
  putStrLn "\n--[TEST] Butters.Parser--\n"
  forM_ examples $
    \(toParse, expected, description) ->
      quickCheck $ counterexample (description ++ " failed to parse") $ toParse `parsesTo` expected

  quickCheck prop_arbitrary_depth_single_constructor_parser 

  quickCheck prop_data_constructors_can_begin_with_capital_letter 

--  forM_ ['A'..'Z'] $
--    quickCheck . test_data_constructors_can_begin_with_capital_letter 

  quickCheck prop_data_constructor_names_can_have_arbitrary_length
