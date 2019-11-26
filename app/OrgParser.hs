{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}

module OrgParser where

import Text.Megaparsec
import Data.Text
import Data.Void
import Control.Applicative hiding (many, some)
import Text.Megaparsec.Char

data Org = Docs 
  | CodeBlock Text [CodeBlockOption] Text

instance Show Org where
  show Docs = "Docs"
  show (CodeBlock fileType options code) = unpack ("CodeBlock " <> fileType <> "\n" 
    <> (pack $ show options) <> "\n" 
    <> code
    )

type CodeBlockOption = (Text, Text)

type Parser = Parsec Void Text

orgParser :: Parser Org
orgParser = do
  skipManyTill latin1Char codeBlock

codeBlockOptions :: Parser [CodeBlockOption]
codeBlockOptions = many $ do
  char ':'
  key <- (string "noweb" <|> string "tangle" <|> string "comments")
  space1
  value <- pack <$> many (noneOf [' ', '\n'])
  space1
  return $ (key, value)

codeBlock :: Parser Org
codeBlock = do
  string "#+BEGIN_SRC"
  space1
  filetype <- pack <$> many alphaNumChar
  space1
  options <- codeBlockOptions
  code <- pack <$> manyTill latin1Char (string "\n#+END_SRC")
  return $ CodeBlock filetype options code

test = parseTest orgParser testString 

testString = "\
\...\n\
\#+BEGIN_SRC js :noweb strip-export :tangle \"../src/constraint-system.js\" :comments no\n\
\\n\
\  // @flow\n\
\\n\
\  import type { AdjacencyIteration,\n\
\                UndirectedAdjacencyIteration } from \"./graph-concepts\";\n\
\  import type { Subscription } from \"./observable\";\n\
\\n\
\  import { reverseTopoSortFrom } from \"./graph-algorithms\";\n\
\\n\
\  import * as Obs from \"./observable\";\n\
\  import { VariableActivation, MethodActivation } from \"./computation-graph\";\n\
\  import { UnionReference } from './union-reference';\n\
\\n\
\  import { hdconsole, HdError } from \"./hdconsole\";\n\
\  import * as util from \"./utilities\"\n\
\#+END_SRC\n\
\"

