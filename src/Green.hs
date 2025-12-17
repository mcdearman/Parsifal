{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Green where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Primitive (Prim)
import Data.Traversable (mapAccumL)
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word16)
import Data.Maybe ( listToMaybe, mapMaybe )

newtype SyntaxKind = SyntaxKind Word16
  deriving (Show, Eq, Ord, Prim)

data Green = GreenToken Token | GreenNode Node deriving (Eq, Show, Ord)

greenKind :: Green -> SyntaxKind
greenKind (GreenToken (Token k _)) = k
greenKind (GreenNode (Node k _ _)) = k

greenChildren :: Green -> Vector Green
greenChildren (GreenToken _) = V.empty
greenChildren (GreenNode gn) = nodeChildren gn

data Node = Node
  { nodeKind :: {-# UNPACK #-} !SyntaxKind,
    nodeChildren :: Vector Green,
    nodeWidth :: {-# UNPACK #-} !Word
  }
  deriving (Show, Eq, Ord)

data Token = Token
  { tokenKind :: {-# UNPACK #-} !SyntaxKind,
    tokenText :: !ByteString
  }
  deriving (Show, Eq, Ord)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: {-# UNPACK #-} !Int,
    syntaxNodeParent :: !(Maybe SyntaxNode),
    syntaxNodeGreen :: !Green
  }
  deriving (Show, Eq, Ord)

syntaxNodeKind :: SyntaxNode -> SyntaxKind
syntaxNodeKind node = greenKind $ syntaxNodeGreen node

syntaxNodeChildren :: SyntaxNode -> Vector SyntaxNode
syntaxNodeChildren n@(SyntaxNode off _ g) =
  snd $
    mapAccumL (\o c -> (o + childLength c, SyntaxNode o (Just n) c)) off (greenChildren g)
  where
    childLength :: Green -> Int
    childLength (GreenToken (Token _ text)) = BS.length text
    childLength (GreenNode (Node _ _ w)) = fromIntegral w

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f

pattern SyntaxKindTerm :: SyntaxKind
pattern SyntaxKindTerm = SyntaxKind 0
pattern SyntaxKindVar :: SyntaxKind
pattern SyntaxKindVar = SyntaxKind 1
pattern SyntaxKindLam :: SyntaxKind
pattern SyntaxKindLam = SyntaxKind 2
pattern SyntaxKindApp :: SyntaxKind
pattern SyntaxKindApp = SyntaxKind 3
pattern SyntaxKindIdent :: SyntaxKind
pattern SyntaxKindIdent = SyntaxKind 4
pattern SyntaxKindBackslash :: SyntaxKind
pattern SyntaxKindBackslash = SyntaxKind 5
pattern SyntaxKindDot :: SyntaxKind
pattern SyntaxKindDot = SyntaxKind 6

