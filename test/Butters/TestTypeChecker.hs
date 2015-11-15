module TestTypeChecker (testTypeChecker) where

import Butters.Ast
import Butters.TypeChecker
import Data.Either
import Test.QuickCheck

test_simple_redundant_constructors_dont_type_check :: Bool
test_simple_redundant_constructors_dont_type_check =
  consName == "BadNat" && a == (BList [], 0) && b == (BList [], 2)
  where
  (Left [RedundantConstructors consName a b]) = typeCheck dataWithRedundant
  dataWithRedundant = DataDef "BadNat" [] [BList [], BList [Name "a"], BList []]

testTypeChecker :: IO ()
testTypeChecker = do
  putStrLn "\n--[TEST] Butters.TypeChecker--\n"

  quickCheck test_simple_redundant_constructors_dont_type_check
