module Butters.TypeChecker where

import Butters.Ast

type ConstructorLocation = (Value, Int)

data TypeCheckingError =
  RedundantConstructors String ConstructorLocation ConstructorLocation
  deriving (Show, Eq)

checkRedundantConstructors (DataDef name [] cs) =
  Left $ [RedundantConstructors name (BList [], 0) (BList [], 2)]

typeCheck = checkRedundantConstructors
