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

whiteSpace = char ' ' <|> char '\n'

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
  whiteSpace
  values <- expression `sepBy` whiteSpace
  char ')'
  return $ App op values

expression :: Parser Value
expression = application <|> list <|> constructor <|> name

list = do
  char '['
  contents <- expression `sepBy` whiteSpace
  char ']'
  return $ BList contents

typeVar :: Parser Char
typeVar = do
  c <- oneOf ['a'..'z']
  whiteSpace
  return c

name :: Parser Value
name = do
  c <- oneOf ['a'..'z']
  rest <- many (letter <|> digit)
  return $ Name $ c:rest

dataDef :: Parser (String, [Char])
dataDef = do
  string "data"
  whiteSpace
  name <- constructorName
  whiteSpace
  typeVars <- typeVar `sepBy` whiteSpace
  string "|"
  whiteSpace
  return (name, typeVars)

topLevel :: Parser TopLevelDefinition
topLevel = do
  (consName, typeVars) <- dataDef
  constructors <- expression `sepBy` (try $ whiteSpace >> char '|' >> whiteSpace)
  return $ DataDef consName typeVars constructors

parseAll = parse (topLevel `sepBy` whiteSpace) ""

parseTopLevel = parse topLevel ""
parseExpression = parse expression ""
