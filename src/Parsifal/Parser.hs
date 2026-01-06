module Parsifal.Parser (parseGrammar) where

import Control.Applicative (Alternative ((<|>)), empty)
import Control.Monad.Combinators (skipManyTill)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (void, ($>))
import Data.Text (Text, pack)
import Data.Void
import Parsifal.Ungrammar
import Text.Megaparsec (MonadParsec (eof, lookAhead, takeWhile1P, takeWhileP, try, withRecovery), Parsec, anySingle, between, many, parse, some, someTill)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

parseGrammar :: Text -> Either (ParseErrorBundle Text Void) Grammar
parseGrammar = parse grammar ""

grammar :: Parser Grammar
grammar = Grammar <$> ((:) <$> (sc *> node) <*> many nodeR) <* eof

-- node with recovery
nodeR :: Parser Node
nodeR = withRecovery handler node
  where
    handler err = do
      dbg ("Error parsing node: " ++ show err) syncToNextNode
      -- syncToNextNode
      pure $ Node "ErrorNode" (RuleSeq [])

node :: Parser Node
node = Node <$> upperIdent <* symbol "=" <*> rule

syncToNextNode :: Parser ()
syncToNextNode =
  skipManyTill anySingle (nodeStart <|> eof)

nodeStart :: Parser ()
nodeStart = void $ lookAhead . try $ upperIdent *> symbol "="

barLA, rparenLA, eofLA :: Parser ()
barLA = lookAhead (void (symbol "|"))
rparenLA = lookAhead (void (symbol ")"))
eofLA = lookAhead eof

rule :: Parser Rule
rule = altRule
  where
    altRule = do
      x <- seqRule
      xs <- many (symbol "|" *> seqRule)
      pure $ case xs of
        [] -> x
        _ -> RuleAlt (x : xs)

    seqRule = do
      xs <- someTill postfix (nodeStart <|> barLA <|> rparenLA <|> eofLA)
      pure $ case xs of
        [x] -> x
        _ -> RuleSeq xs

    postfix = do
      a <- atom
      -- zero or more postfix operators
      ops <-
        many
          ( RuleOpt <$ symbol "?"
              <|> RuleRep <$ symbol "*"
          )
      pure (foldl' (\r f -> f r) a ops)

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
ident = pack <$> lexeme ((:) <$> identStart <*> many identLetter)
  where
    identStart = letterChar <|> char '_'
    identLetter = alphaNumChar <|> char '_'

symbol :: Text -> Parser Text
symbol = L.symbol sc

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

sc :: Parser ()
sc = L.space space1 lineComment empty

lineComment :: Parser ()
lineComment = L.skipLineComment "//"
