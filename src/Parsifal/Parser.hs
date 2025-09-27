module Parsifal.Parser where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Text (Text, pack)
import Data.Void
import Parsifal.Ungrammar
import Text.Megaparsec (Parsec, between, many, sepBy)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

grammar :: Parser Grammar
grammar = Grammar <$> many node

node :: Parser Node
node = Node <$> upperIdent <* symbol "=" <*> rule

rule :: Parser Rule
rule = lexeme (tokenRule <|> nodeRule <|> sequenceRule <|> altRule <|> optRule <|> repRule <|> parens)
  where
    tokenRule = RuleToken <$> token
    nodeRule = RuleNode <$> upperIdent
    sequenceRule = RuleSeq <$> many rule
    altRule = RuleAlt <$> sepBy rule (symbol "|")
    optRule = RuleOpt <$> rule <* symbol "?"
    repRule = RuleRep <$> rule <* symbol "*"
    parens = between (symbol "(") (symbol ")") rule

label :: Parser Text
label = pack <$> many L.charLiteral

token :: Parser Token
token = Token <$> between (symbol "'") (symbol "'") ident

upperIdent :: Parser Text
upperIdent = pack <$> ((:) <$> upperChar <*> many alphaNumChar)

ident :: Parser Text
ident = pack <$> ((:) <$> letterChar <*> many alphaNumChar)

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 empty empty
