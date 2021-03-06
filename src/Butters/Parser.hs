module Butters.Parser where

import Butters.Ast
import Text.ParserCombinators.Parsec

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

valueDefinition :: Parser TopLevelDefinition
valueDefinition = do
  valueName <- name
  whiteSpace
  char ':'
  whiteSpace
  ty <- expression
  whiteSpace
  char '='
  whiteSpace
  value <- expression
  return $ ValueDef valueName ty value

topLevel :: Parser TopLevelDefinition
topLevel = dataDeclaration <|> subsumptionDeclaration <|> valueDefinition

parseAll = parse (topLevel `sepBy` whiteSpace) ""

parseTopLevel = parse topLevel ""
parseExpression = parse expression ""
