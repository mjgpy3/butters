module Butters.Parser where

import Text.ParserCombinators.Parsec

data Constructor =
  BList [Constructor]
  deriving (Show, Eq)

data TopLevelDefinition =
  DataDef String [String] [Constructor]
  deriving (Show, Eq)

constructorName :: Parser String
constructorName = do
  c <- oneOf ['A'..'Z']
  rest <- many (oneOf $ ['a'..'z'] ++ ['A'..'Z'])
  return $ c:rest

expression :: Parser Constructor
expression = do
  char '['
  contents <- many expression
  char ']'
  return $ BList contents

topLevel :: Parser TopLevelDefinition
topLevel = do
  string "data"
  char ' '
  name <- constructorName
  char ' '
  char '|'
  char ' '
  cons <- expression
  return $ DataDef name [] [cons]

parseTopLevel = parse topLevel ""
