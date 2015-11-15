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
  | SubsumptionDef String String [(Value, Value)]
  deriving (Show, Eq)

whiteSpace = many (char ' ' <|> char '\n' <|> char '\r' <|> char '\t') <?> "whitespace"

constructorName :: Parser String
constructorName = do
  c <- oneOf ['A'..'Z']
  rest <- many (letter <|> digit)
  return $ c:rest

constructor :: Parser Value
constructor = Constructor <$> constructorName

application :: Parser Value
application = do
  char '('
  optional whiteSpace
  op <- expression
  whiteSpace
  values <- expression `sepEndBy` whiteSpace
  char ')'
  return $ App op values

expression :: Parser Value
expression = application <|> list <|> constructor <|> name

list = BList
  <$> between
    (char '[' >> optional whiteSpace)
    (char ']')
    (expression `sepEndBy` whiteSpace)

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
  typeVars <- oneOf ['a'..'z'] `sepEndBy` whiteSpace
  string "|"
  whiteSpace
  return (name, typeVars)

dataDeclaration :: Parser TopLevelDefinition
dataDeclaration = do
  (consName, typeVars) <- dataDef
  constructors <- expression `sepBy` (try $ whiteSpace >> char '|' >> whiteSpace)
  return $ DataDef consName typeVars constructors

subsumptionDeclaration :: Parser TopLevelDefinition
subsumptionDeclaration = do
  superSet <- constructorName
  whiteSpace
  string "subsumes"
  whiteSpace
  subSet <- constructorName
  return $ SubsumptionDef superSet subSet []

topLevel :: Parser TopLevelDefinition
topLevel = dataDeclaration <|> subsumptionDeclaration

parseAll = parse (topLevel `sepBy` whiteSpace) ""

parseTopLevel = parse topLevel ""
parseExpression = parse expression ""
