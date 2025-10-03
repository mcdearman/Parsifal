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
  { nodeKinds :: !(PrimArray SyntaxKind),
    nodeChildStarts :: !(PrimArray Int),
    nodeChildCounts :: !(PrimArray Int),
    nodeWidths :: !(PrimArray Int)
  }

type ChildWord = Word

{-# INLINE packTok #-}
packTok :: TokenId -> ChildWord
packTok (TokenId !ix) = (fromIntegral ix `shiftL` 1) .|. 0

{-# INLINE packNode #-}
packNode :: NodeId -> ChildWord
packNode (NodeId !ix) = (fromIntegral ix `shiftL` 1) .|. 1

{-# INLINE isNode #-}
isNode :: ChildWord -> Bool
isNode !w = (w .&. 1) /= 0

{-# INLINE childIx #-}
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
  { nodeKind :: {-# UNPACK #-} !SyntaxKind,
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
  { tokKinds :: !(PrimArray SyntaxKind),
    tokTexts :: !(SmallArray ByteString)
  }

newtype SyntaxKind = SyntaxKind Word16 deriving (Show, Eq, Ord, Prim)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: {-# UNPACK #-} !Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: {-# UNPACK #-} !NodeId
  }
  deriving (Show, Eq, Ord)

-- nodeKind :: SyntaxNode -> SyntaxKind
-- nodeKind node = greenNodeKind (syntaxNodeGreen node)

-- nodeChildren :: SyntaxNode -> [SyntaxNode]
-- nodeChildren node = undefined

findMap :: (a -> Maybe b) -> [a] -> Maybe b
findMap f = listToMaybe . mapMaybe f
