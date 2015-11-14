module Main where

import Debug.Trace
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

prop_data_constructors_can_begin_with_capital_letter :: Property 
prop_data_constructors_can_begin_with_capital_letter =
  forAll (elements ['A'..'Z']) $ \f ->
    single_constructor_text (f:"oobar") "[]" `parsesTo` DataDef (f:"oobar") [] [BList []]

constructor_name :: Gen String
constructor_name = do
  f <- elements ['A'..'Z']
  rest <- sublistOf $ ['A'..'Z'] ++ ['a'..'z']
  shuffled <- shuffle rest
  return $ f:shuffled

prop_data_constructor_names_can_have_arbitrary_length :: Property
prop_data_constructor_names_can_have_arbitrary_length =
  forAll constructor_name $ \name -> 
    single_constructor_text name "[]" `parsesTo` DataDef name [] [BList []]

prop_constructors_can_contain_other_constructors :: Property
prop_constructors_can_contain_other_constructors =
  forAll constructor_name $ \name ->
    single_constructor_text "SomeConst" ("[" ++ name ++ "]") `parsesTo` DataDef "SomeConst" [] [BList [Constructor name]]

examples = [
    (
      "data Zero | []",
      DataDef "Zero" [] [BList []],
      "Zero Constructor"
    )
    ,(
      "data Bool | [] | [[]]",
      DataDef "Bool" [] [BList [], BList [BList []]],
      "Bool Constructor"
    )
    ,(
      "data Nat | [] | [Nat]",
      DataDef "Nat" [] [BList [], BList [Constructor "Nat"]],
      "Nat Constructor"
    )
  ]

main :: IO ()
main = do
  putStrLn "\n--[TEST] Butters.Parser--\n"
  forM_ examples $
    \(toParse, expected, description) ->
      quickCheck $ counterexample (description ++ " failed to parse") $ TopLevel toParse `parsesTo` expected

  quickCheck prop_arbitrary_depth_single_constructor_parser 
  quickCheck prop_data_constructors_can_begin_with_capital_letter 
  quickCheck prop_data_constructor_names_can_have_arbitrary_length
  quickCheck prop_constructors_can_contain_other_constructors 
