module Butters.Parser where

import Text.ParserCombinators.Parsec

data Value =
  BList [Value]
  | Constructor String
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [String] [Value]
  deriving (Show, Eq)

constructorName :: Parser String
constructorName = do
  c <- oneOf ['A'..'Z']
  rest <- many (letter <|> digit)
  return $ c:rest

constructor :: Parser Value
constructor = do
  n <- constructorName
  return $ Constructor n

expression :: Parser Value
expression = list <|> constructor

list = do
  char '['
  contents <- sepBy expression (char ' ')
  char ']'
  return $ BList contents

constructorCase :: Parser Value
constructorCase = do
  char ' '
  char '|'
  char ' '
  cons <- expression
  return $ cons

topLevel :: Parser TopLevelDefinition
topLevel = do
  string "data"
  char ' '
  name <- constructorName
  constructors <- many constructorCase
  return $ DataDef name [] constructors

parseTopLevel = parse topLevel ""
parseExpression = parse expression ""
