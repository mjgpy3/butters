module Butters.TypeChecker where

import Butters.Ast

data TypeCheckingError =
  RedundantConstructors String Value

checkRedundantConstructors (DataDef _ [] cs) = Left $ RedundantConstructors "" $ BList []
