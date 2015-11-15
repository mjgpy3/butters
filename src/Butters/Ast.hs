module Butters.Ast where

data Value =
  BList [Value]
  | Constructor String
  | TypeVar Char
  | App Value [Value]
  | Name String
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [Char] [Value]
  | SubsumptionDef String String [(Value, Value)]
  | ValueDef Value Value Value
  deriving (Show, Eq)
