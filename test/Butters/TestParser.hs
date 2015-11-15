module TestParser (testParser) where

import Butters.Parser
import Control.Monad
import Data.Either
import Test.QuickCheck

parsesTo text = (==) (parseTopLevel text) . Right
parsesToMany text = (==) (parseAll text) . Right

failsToParse = isLeft . parseTopLevel

single_constructor_text :: String -> String -> String
single_constructor_text name cons = "data " ++ name ++ " | " ++ cons

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

prop_constructors_can_be_one_character :: Property
prop_constructors_can_be_one_character =
  forAll (elements ['A'..'Z']) $ \name ->
    single_constructor_text [name] "[]" `parsesTo` DataDef [name] [] [BList []]

prop_constructors_can_contain_other_constructors :: Property
prop_constructors_can_contain_other_constructors =
  forAll constructor_name $ \name ->
    single_constructor_text "SomeConst" ("[" ++ name ++ "]") `parsesTo` DataDef "SomeConst" [] [BList [Constructor name]]

prop_multiple_constructors_can_be_parsed :: Property
prop_multiple_constructors_can_be_parsed =
  forAll constructor_name $ \name1 ->
    forAll constructor_name $ \name2 ->
      (single_constructor_text name1 "[]\n" ++ single_constructor_text name2 "[]") `parsesToMany` [
        DataDef name1 [] [BList []]
        ,DataDef name2 [] [BList []]
      ]

random_whitespace :: Gen String
random_whitespace = do
  n <- arbitrary
  ws <- shuffle $ concat $ replicate n "\t\n\r "
  return $ take (1 + mod (abs n) 10) ws

prop_spaces_are_acceptable_between_list_items :: Property
prop_spaces_are_acceptable_between_list_items =
  forAll random_whitespace $ \ws ->
    parseExpression (concat ["[", ws, "[]" ++ ws ++ "[]", ws, "]"]) == Right (BList [BList [], BList []])

prop_spaces_are_acceptable_between_application_items :: Property
prop_spaces_are_acceptable_between_application_items =
  forAll random_whitespace $ \ws ->
    parseExpression (concat ["(", ws, "Foobar " ++ ws ++ "a", ws, ")"]) == Right (App (Constructor "Foobar") [Name "a"])

test_multiple_statements_parse_successfully :: Bool
test_multiple_statements_parse_successfully =
  length v == 4
  where
    (Right v) = parseAll $ concat ["data Nat | [] | [Nat]\n\n",
                                   "data OddNat | [[]] | [[OddNat]]\n\n",
                                   "empty : (List Int) = []\n\n",
                                   "Nat subsumes OddNat"]

prop_spaces_are_acceptable_between_data_decl_items :: Property
prop_spaces_are_acceptable_between_data_decl_items =
  forAll random_whitespace $ \ws ->
    concat ["data ", ws, "List ", ws, "a ", ws, "| ", ws, "[] ", ws, "| ", ws, "[a (List a)]", ws] `parsesTo` DataDef "List" ['a'] [
      BList []
      ,BList [Name "a", (App (Constructor "List") [Name "a"])]
    ]

examples = [
    (
      "data Zero | []"
      ,DataDef "Zero" [] [BList []]
      ,"Zero Constructor"
    )
    ,(
      "data Bool | [] | [[]]"
      ,DataDef "Bool" [] [BList [], BList [BList []]]
      ,"Bool Constructor"
    )
    ,(
      "data Nat | [] | [Nat]"
      ,DataDef "Nat" [] [BList [], BList [Constructor "Nat"]]
      ,"Nat Constructor"
    )
    ,(
      "data Int | [] | [[] [Nat]] | [[[]] [Nat]]"
      ,DataDef "Int" [] [
        BList []
        ,BList [BList [], BList [Constructor "Nat"]]
        ,BList [BList[BList []], BList [Constructor "Nat"]]
      ]
      ,"Int Constructor"
    )
    ,(
      "data List a | [] | [a (List a)]"
      ,DataDef "List" ['a'] [
        BList []
        ,BList [Name "a", (App (Constructor "List") [Name "a"])]
      ]
      ,"List Constructor"
    )
    ,(
      "Nat subsumes EvenNat"
      ,SubsumptionDef "Nat" "EvenNat" []
      ,"The set of naturals subsumes even naturals"
    )
    ,(
      "empty : (List Int) = []"
      ,ValueDef (Name "empty") (App (Constructor "List") [Constructor "Int"]) (BList [])
      ,"Empty list function"
    )
  ]

testParser :: IO ()
testParser = do
  putStrLn "\n--[TEST] Butters.Parser--\n"
  forM_ examples $
    \(toParse, expected, description) ->
      quickCheck $ counterexample (description ++ " failed to parse\n" ++ (show $ parseTopLevel toParse)) $ toParse `parsesTo` expected

  quickCheck prop_arbitrary_depth_single_constructor_parser
  quickCheck prop_data_constructors_can_begin_with_capital_letter
  quickCheck prop_data_constructor_names_can_have_arbitrary_length
  quickCheck prop_constructors_can_contain_other_constructors
  quickCheck prop_multiple_constructors_can_be_parsed
  quickCheck prop_spaces_are_acceptable_between_list_items
  quickCheck prop_spaces_are_acceptable_between_data_decl_items
  quickCheck prop_spaces_are_acceptable_between_application_items

  quickCheck test_multiple_statements_parse_successfully
