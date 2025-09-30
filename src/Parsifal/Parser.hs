module Parsifal.Parser (parseGrammar) where

import Control.Applicative (Alternative ((<|>)), empty)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Functor (void, ($>))
import Data.Text (Text, pack)
import Data.Void
import Parsifal.Ungrammar
import Text.Megaparsec (MonadParsec (eof, lookAhead, takeWhile1P, takeWhileP, try), Parsec, between, many, parse, some, someTill)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Debug (MonadParsecDbg (dbg))
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = Parsec Void Text

parseGrammar :: Text -> Either (ParseErrorBundle Text Void) Grammar
parseGrammar = parse grammar ""

grammar :: Parser Grammar
grammar = Grammar <$> ((:) <$> (sc *> node) <*> some node)

node :: Parser Node
node = Node <$> upperIdent <* symbol "=" <*> rule

nodeHeaderLA :: Parser ()
nodeHeaderLA = lookAhead . try $ upperIdent *> symbol "=" $> ()

barLA, rparenLA, eofLA :: Parser ()
barLA = lookAhead (void (symbol "|"))
rparenLA = lookAhead (void (symbol ")"))
eofLA = lookAhead eof

rule :: Parser Rule
rule =
  dbg "rule" altRule
  where
    -- altRule = RuleAlt <$> ((:) <$> seqRule <*> some (symbol "|" *> seqRule))
    -- seqRule = try (RuleSeq <$> someTill postfix nodeHeaderLA) <|> atom
    altRule = do
      x <- seqRule
      xs <- many (symbol "|" *> seqRule)
      pure $ case xs of
        [] -> x
        _ -> RuleAlt (x : xs)

    seqRule = do
      xs <- someTill postfix (nodeHeaderLA <|> barLA <|> rparenLA <|> eofLA)
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

-- optRule = RuleOpt <$> atom <* symbol "?"
-- repRule = RuleRep <$> atom <* symbol "*"

-- altOp :: Operator Parser Rule
-- altOp = InfixR (mkAlt <$ symbol "|")

-- mkAlt :: Rule -> Rule -> Rule
-- mkAlt l r = RuleAlt (flatten l <> flatten r)
--   where
--     flatten (RuleAlt xs) = xs
--     flatten x = [x]

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
