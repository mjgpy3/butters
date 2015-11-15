module TestTypeChecker (testTypeChecker) where

import Butters.Ast
import Butters.TypeChecker
import Data.Either
import Test.QuickCheck

test_redundant_constructors_dont_type_check :: Bool
test_redundant_constructors_dont_type_check =
  isLeft $ checkRedundantConstructors $ DataDef "BadNat" [] [BList [], BList []]

testTypeChecker :: IO ()
testTypeChecker = do
  putStrLn "\n--[TEST] Butters.TypeChecker--\n"

  quickCheck test_redundant_constructors_dont_type_check
