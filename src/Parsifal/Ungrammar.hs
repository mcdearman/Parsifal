module Parsifal.Ungrammar
  ( Token (..),
    Node (..),
    Grammar (..),
    Rule (..),
  )
where

import Data.Text (Text)

newtype Grammar = Grammar {gramNodes :: [Node]} deriving (Show, Eq, Ord)

data Node = Node {nodeName :: Text, nodeRule :: Rule} deriving (Show, Eq, Ord)

newtype Token = Token {tokenName :: Text} deriving (Show, Eq, Ord)

data Rule
  = RuleLabeled Text Rule
  | RuleNode Node
  | RuleToken Token
  | RuleSeq [Rule]
  | RuleAlt [Rule]
  | RuleOpt Rule
  | RuleRep Rule
  deriving (Show, Eq, Ord)
