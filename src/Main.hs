module Main where

import Data.ByteString (ByteString)
import Data.Primitive (PrimArray, SmallArray)
import Data.Word (Word16, Word8)

main :: IO ()
main = putStrLn "Hello, Haskell!"

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

data GreenChildren = GreenChildren
  { childTag :: !(PrimArray Word8),
    childIx :: !(PrimArray Int)
  }

data Tokens = Tokens
  { tokKind :: !(PrimArray SyntaxKind),
    tokText :: !(SmallArray ByteString)
  }

newtype SyntaxKind = SyntaxKind Word16 deriving (Show, Eq, Ord)