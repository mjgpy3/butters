module Butters.Parser where

import Text.ParserCombinators.Parsec

data Value =
  BList [Value]
  | Constructor String
  | TypeVar Char
  | App Value [Value]
  | Name String
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [Char] [Value]
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

application :: Parser Value
application = do
  char '('
  op <- expression
  char ' '
  values <- expression `sepBy` char ' '
  char ')'
  return $ App op values

expression :: Parser Value
expression = application <|> list <|> constructor <|> name

list = do
  char '['
  contents <- expression `sepBy` char ' '
  char ']'
  return $ BList contents

typeVar :: Parser Char
typeVar = do
  c <- oneOf ['a'..'z']
  char ' '
  return c

name :: Parser Value
name = do
  c <- oneOf ['a'..'z']
  rest <- many (letter <|> digit)
  return $ Name $ c:rest

dataDef :: Parser (String, [Char])
dataDef = do
  string "data "
  name <- constructorName
  char ' '
  typeVars <- typeVar `sepBy` char ' '
  string "| "
  return (name, typeVars)

topLevel :: Parser TopLevelDefinition
topLevel = do
  (consName, typeVars) <- dataDef
  constructors <- expression `sepBy` string " | "
  return $ DataDef consName typeVars constructors

parseTopLevel = parse topLevel ""
parseExpression = parse expression ""
