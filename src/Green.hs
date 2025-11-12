{-# LANGUAGE PatternSynonyms #-}
module Parsifal.Green where

import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))
import Data.ByteString (ByteString)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Primitive (PrimArray, SmallArray)
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

pattern SyntaxKindExpr :: SyntaxKind
pattern SyntaxKindExpr = SyntaxKind 0
pattern SyntaxKindLit :: SyntaxKind
pattern SyntaxKindLit = SyntaxKind 1
pattern SyntaxKindVar :: SyntaxKind
pattern SyntaxKindVar = SyntaxKind 2
pattern SyntaxKindLet :: SyntaxKind
pattern SyntaxKindLet = SyntaxKind 3
pattern SyntaxKindBind :: SyntaxKind
pattern SyntaxKindBind = SyntaxKind 4
pattern SyntaxKindBindFun :: SyntaxKind
pattern SyntaxKindBindFun = SyntaxKind 5
pattern SyntaxKindPatternList :: SyntaxKind
pattern SyntaxKindPatternList = SyntaxKind 6
pattern SyntaxKindPattern :: SyntaxKind
pattern SyntaxKindPattern = SyntaxKind 7
pattern SyntaxKindIf :: SyntaxKind
pattern SyntaxKindIf = SyntaxKind 8
pattern SyntaxKindMatch :: SyntaxKind
pattern SyntaxKindMatch = SyntaxKind 9
pattern SyntaxKindMatchCase :: SyntaxKind
pattern SyntaxKindMatchCase = SyntaxKind 10
pattern SyntaxKindLam :: SyntaxKind
pattern SyntaxKindLam = SyntaxKind 11
pattern SyntaxKindApp :: SyntaxKind
pattern SyntaxKindApp = SyntaxKind 12
pattern SyntaxKindInfix :: SyntaxKind
pattern SyntaxKindInfix = SyntaxKind 13
pattern SyntaxKindInfixOp :: SyntaxKind
pattern SyntaxKindInfixOp = SyntaxKind 14
pattern SyntaxKindNeg :: SyntaxKind
pattern SyntaxKindNeg = SyntaxKind 15
pattern SyntaxKindInt :: SyntaxKind
pattern SyntaxKindInt = SyntaxKind 16
pattern SyntaxKindString :: SyntaxKind
pattern SyntaxKindString = SyntaxKind 17
pattern SyntaxKindIdent :: SyntaxKind
pattern SyntaxKindIdent = SyntaxKind 18
pattern SyntaxKindLet :: SyntaxKind
pattern SyntaxKindLet = SyntaxKind 19
pattern SyntaxKindEq :: SyntaxKind
pattern SyntaxKindEq = SyntaxKind 20
pattern SyntaxKindIn :: SyntaxKind
pattern SyntaxKindIn = SyntaxKind 21
pattern SyntaxKindLParen :: SyntaxKind
pattern SyntaxKindLParen = SyntaxKind 22
pattern SyntaxKindRParen :: SyntaxKind
pattern SyntaxKindRParen = SyntaxKind 23
pattern SyntaxKindIf :: SyntaxKind
pattern SyntaxKindIf = SyntaxKind 24
pattern SyntaxKindThen :: SyntaxKind
pattern SyntaxKindThen = SyntaxKind 25
pattern SyntaxKindElse :: SyntaxKind
pattern SyntaxKindElse = SyntaxKind 26
pattern SyntaxKindMatch :: SyntaxKind
pattern SyntaxKindMatch = SyntaxKind 27
pattern SyntaxKindWith :: SyntaxKind
pattern SyntaxKindWith = SyntaxKind 28
pattern SyntaxKindRArrow :: SyntaxKind
pattern SyntaxKindRArrow = SyntaxKind 29
pattern SyntaxKindBackslash :: SyntaxKind
pattern SyntaxKindBackslash = SyntaxKind 30
pattern SyntaxKindPlus :: SyntaxKind
pattern SyntaxKindPlus = SyntaxKind 31
pattern SyntaxKindMinus :: SyntaxKind
pattern SyntaxKindMinus = SyntaxKind 32
pattern SyntaxKindStar :: SyntaxKind
pattern SyntaxKindStar = SyntaxKind 33
pattern SyntaxKindSlash :: SyntaxKind
pattern SyntaxKindSlash = SyntaxKind 34
pattern SyntaxKindEqEq :: SyntaxKind
pattern SyntaxKindEqEq = SyntaxKind 35
pattern SyntaxKindBangEq :: SyntaxKind
pattern SyntaxKindBangEq = SyntaxKind 36
pattern SyntaxKindLt :: SyntaxKind
pattern SyntaxKindLt = SyntaxKind 37
pattern SyntaxKindLeq :: SyntaxKind
pattern SyntaxKindLeq = SyntaxKind 38
pattern SyntaxKindGt :: SyntaxKind
pattern SyntaxKindGt = SyntaxKind 39
pattern SyntaxKindGeq :: SyntaxKind
pattern SyntaxKindGeq = SyntaxKind 40

