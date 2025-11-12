{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Parsifal.Internal where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Primitive (Prim, PrimArray, SmallArray)
import Data.Primitive.PrimArray (indexPrimArray)
import Data.Word (Word16)

data GreenCtx = GreenCtx
  { greenNodes :: !GreenNodes,
    greenChildren :: !GreenChildren,
    tokens :: !Tokens
  }

data GreenNodes = GreenNodes
  { nodeKinds :: !(PrimArray NodeKind),
    nodeChildStarts :: !(PrimArray Int),
    nodeChildCounts :: !(PrimArray Int),
    nodeWidths :: !(PrimArray Int)
  }

type ChildWord = Word

packTok :: TokenId -> ChildWord
packTok (TokenId !ix) = (fromIntegral ix `shiftL` 1) .|. 0

packNode :: NodeId -> ChildWord
packNode (NodeId !ix) = (fromIntegral ix `shiftL` 1) .|. 1

isNode :: ChildWord -> Bool
isNode !w = (w .&. 1) /= 0

childIx :: ChildWord -> Int
childIx !w = fromIntegral (w `shiftR` 1)

data NodeChild = CToken !TokenId | CNode !NodeId

decodeChild :: ChildWord -> NodeChild
decodeChild !w
  | isNode w = CNode (NodeId (childIx w))
  | otherwise = CToken (TokenId (childIx w))

pattern Child :: NodeChild -> ChildWord
pattern Child cref <- (decodeChild -> cref)
  where
    Child (CToken (TokenId ix)) = packTok (TokenId ix)
    Child (CNode (NodeId ix)) = packNode (NodeId ix)

data GreenNode = GreenNode
  { nodeKind :: {-# UNPACK #-} !NodeKind,
    nodeChildren :: ![ChildWord],
    nodeWidth :: {-# UNPACK #-} !Int
  }

decodeGreenNode :: GreenCtx -> NodeId -> GreenNode
decodeGreenNode ctx (NodeId ix) =
  let kind = nodeKinds (greenNodes ctx) `indexPrimArray` ix
      start = nodeChildStarts (greenNodes ctx) `indexPrimArray` ix
      count = nodeChildCounts (greenNodes ctx) `indexPrimArray` ix
      width = nodeWidths (greenNodes ctx) `indexPrimArray` ix
      children = indexGreenChildren (greenChildren ctx) start count
   in GreenNode kind children width

newtype NodeId = NodeId Int deriving (Show, Eq, Ord)

newtype TokenId = TokenId Int deriving (Show, Eq, Ord)

newtype GreenChildren = GreenChildren (PrimArray ChildWord)

indexGreenChildren :: GreenChildren -> Int -> Int -> [ChildWord]
indexGreenChildren (GreenChildren arr) start count =
  [arr `indexPrimArray` (start + i) | i <- [0 .. count - 1]]

data Tokens = Tokens
  { tokKinds :: !(PrimArray TokenKind),
    tokTexts :: !(SmallArray ByteString)
  }

newtype TokenKind = TokenKind Word16 deriving (Show, Eq, Ord, Prim)

newtype NodeKind = SyntaxKind Word16 deriving (Show, Eq, Ord, Prim)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: {-# UNPACK #-} !Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: {-# UNPACK #-} !NodeId
  }
  deriving (Show, Eq, Ord)

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f
