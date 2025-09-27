module Parsifal.Parser (parseGrammar) where

import Control.Applicative (Alternative ((<|>)), empty)
import Data.Functor (($>))
import Data.Text (Text, pack)
import Data.Void
import Parsifal.Ungrammar
import Text.Megaparsec (MonadParsec (lookAhead, takeWhile1P, takeWhileP, try), Parsec, between, many, parse, some)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

parseGrammar :: Text -> Either (ParseErrorBundle Text Void) Grammar
parseGrammar = parse grammar ""

grammar :: Parser Grammar
grammar = Grammar <$> ((:) <$> node <*> some node)

node :: Parser Node
node = Node <$> upperIdent <* symbol "=" <*> rule <* nodeHeaderLA

nodeHeaderLA :: Parser ()
nodeHeaderLA = lookAhead . try $ upperIdent *> symbol "=" $> ()

rule :: Parser Rule
rule =
  dbg "rule" $
    try sequenceRule
      <|> try optRule
      <|> try repRule
      <|> try atom
      <|> try altRule
  where
    sequenceRule = RuleSeq <$> ((:) <$> atom <*> some rule)
    altRule = RuleAlt <$> ((:) <$> atom <*> some (symbol "|" *> rule))
    optRule = RuleOpt <$> atom <* symbol "?"
    repRule = RuleRep <$> atom <* symbol "*"

atom :: Parser Rule
atom = try nodeRule <|> try tokenRule <|> parens <|> labelRule
  where
    tokenRule = RuleToken <$> token
    nodeRule = RuleNode <$> upperIdent
    parens = between (symbol "(") (symbol ")") rule
    labelRule = RuleLabeled <$> ident <* symbol ":" <*> rule

token :: Parser Token
token = Token <$> lexeme (between (symbol "'") (symbol "'") (takeWhile1P Nothing (/= '\'')))

upperIdent :: Parser Text
upperIdent = pack <$> lexeme ((:) <$> upperChar <*> many alphaNumChar)

ident :: Parser Text
ident = pack <$> lexeme ((:) <$> letterChar <*> many alphaNumChar)

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "//"
