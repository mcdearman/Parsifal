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
  } deriving (Show, Eq, Ord)

data GreenNodes = GreenNodes
  { nodeKind :: !(PrimArray SyntaxKind),
    nodeChildStart :: !(PrimArray Int),
    nodeChildCount :: !(PrimArray Int),
    nodeWidth :: !(PrimArray Int)
  } deriving (Show, Eq, Ord)

type ChildWord = Word

packTok :: TokenId -> ChildWord
packTok (TokenId !ix) = (fromIntegral ix `shiftL` 1) .|. 0

packNode :: NodeId -> ChildWord
packNode (NodeId !ix) = (fromIntegral ix `shiftL` 1) .|. 1

isNode :: ChildWord -> Bool
isNode !w = (w .&. 1) /= 0

childIx :: ChildWord -> Int
childIx !w = fromIntegral (w `shiftR` 1)

data ChildRef = CToken !TokenId | CNode !NodeId

pattern Child :: ChildRef -> ChildWord
pattern Child cref <- (decodeChild -> cref)
  where
    Child (CToken (TokenId ix)) = packTok (TokenId ix)
    Child (CNode (NodeId ix)) = packNode (NodeId ix)

newtype NodeId = NodeId Int deriving (Show, Eq, Ord)

newtype TokenId = TokenId Int deriving (Show, Eq, Ord)

newtype GreenChildren = GreenChildren (PrimArray ChildWord) deriving (Show, Eq, Ord)

data Tokens = Tokens
  { tokKind :: !(PrimArray SyntaxKind),
    tokText :: !(SmallArray ByteString)
  } deriving (Show, Eq, Ord)

newtype SyntaxKind = SyntaxKind Word16 deriving (Show, Eq, Ord)

data SyntaxNode = SyntaxNode
  { syntaxNodeOffset :: {-# UNPACK #-} !Int,
    syntaxNodeParent :: Maybe SyntaxNode,
    syntaxNodeGreen :: {-# UNPACK #-} !NodeId
  } deriving (Show, Eq, Ord)

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

