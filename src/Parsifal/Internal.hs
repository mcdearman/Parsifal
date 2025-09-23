module Parsifal.Internal where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.Primitive (PrimArray, SmallArray)
import Data.Word (Word16)

data GreenCtx = GreenCtx
  { greenNodes :: !GreenNodes,
    greenChildren :: !GreenChildren,
    tokens :: !Tokens
  }

data GreenNodes = GreenNodes
  { nodeKind :: !(PrimArray SyntaxKind),
    nodeChildStart :: !(PrimArray Int),
    nodeChildCount :: !(PrimArray Int),
    nodeWidth :: !(PrimArray Int)
  }

type ChildWord = Word

packTok :: TokenId -> ChildWord
packTok (TokenId ix) = (fromIntegral ix `shiftL` 1) .|. 0

packNode :: NodeId -> ChildWord
packNode (NodeId ix) = (fromIntegral ix `shiftL` 1) .|. 1

isNode :: ChildWord -> Bool
isNode w = (w .&. 1) /= 0

childIx :: ChildWord -> Int
childIx w = fromIntegral (w `shiftR` 1)

data ChildRef = CToken !TokenId | CNode !NodeId

decodeChild :: ChildWord -> ChildRef
decodeChild w
  | isNode w = CNode (NodeId (childIx w))
  | otherwise = CToken (TokenId (childIx w))

pattern Child :: ChildRef -> ChildWord
pattern Child cref <- (decodeChild -> cref)
  where
    Child (CToken (TokenId ix)) = packTok (TokenId ix)
    Child (CNode (NodeId ix)) = packNode (NodeId ix)

newtype NodeId = NodeId Int deriving (Show, Eq, Ord)

newtype TokenId = TokenId Int deriving (Show, Eq, Ord)

newtype GreenChildren = GreenChildren (PrimArray ChildWord)

data Tokens = Tokens
  { tokKind :: !(PrimArray SyntaxKind),
    tokText :: !(SmallArray ByteString)
  }

newtype SyntaxKind = SyntaxKind Word16 deriving (Show, Eq, Ord)