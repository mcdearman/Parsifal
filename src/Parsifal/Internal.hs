{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Parsifal.Internal where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Primitive (Prim, PrimArray, SmallArray)
import Data.Primitive.PrimArray (indexPrimArray)
import Data.Word (Word16)

newtype SyntaxKind = SyntaxKind Word16
  deriving (Show, Eq, Ord, Prim)

newtype TokenKind = TokenKind Word16
  deriving (Show, Eq, Ord, Prim)

data GreenNode = GreenNode
  { greenNodeKind :: {-# UNPACK #-} !SyntaxKind,
    greenNodeChildren :: [Either Token GreenNode],
    greenNodeWidth :: {-# UNPACK #-} !Word
  }
  deriving (Show, Eq, Ord)

data Token = Token
  { tokenKind :: {-# UNPACK #-} !TokenKind,
    tokenText :: !ByteString
  }
  deriving (Show, Eq, Ord)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: {-# UNPACK #-} !Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: !GreenNode
  }
  deriving (Show, Eq, Ord)

nodeKind :: SyntaxNode -> SyntaxKind
nodeKind node = greenNodeKind (syntaxNodeGreen node)

nodeChildren :: SyntaxNode -> [SyntaxNode]
nodeChildren (SyntaxNode off p g) = map toSyntaxNode children
  where
    children = [c | Right c <- greenNodeChildren g]
    toSyntaxNode gn =
      SyntaxNode
        { syntaxNodeOffset = off,
          syntaxNodeParent = Just (SyntaxNode off p g),
          syntaxNodeGreen = gn
        }

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f
