module Parsifal.Gen where

import Data.List (nub)
import Data.Text (Text, pack, splitOn, toUpper)
import qualified Data.Text as Text
import Parsifal.Ungrammar

genModuleHeader :: Text -> Text
genModuleHeader moduleName =
  Text.unlines
    [ "{-# LANGUAGE PatternSynonyms #-}",
      "module " <> moduleName <> " where"
    ]

genImportDecls :: Text
genImportDecls =
  Text.unlines
    [ "import Data.Bits (Bits (shiftL, shiftR, (.&.), (.|.)))",
      "import Data.ByteString (ByteString)",
      "import Data.Maybe (listToMaybe, mapMaybe)",
      "import Data.Primitive (PrimArray, SmallArray)",
      "import Data.Word (Word16)"
    ]

genRedGreenTrees :: Text
genRedGreenTrees =
  Text.unlines
    [ """
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
      """
    ]

specMap :: [(Text, Text)]
specMap =
  [ ("+", "Plus"),
    ("-", "Minus"),
    ("*", "Star"),
    ("/", "Slash"),
    ("%", "Percent"),
    ("&", "Amp"),
    ("|", "Pipe"),
    ("^", "Caret"),
    ("~", "Tilde"),
    ("=", "Eq"),
    ("<", "Lt"),
    (">", "Gt"),
    ("!", "Bang"),
    ("?", "Question"),
    (":", "Colon"),
    (".", "Dot"),
    (",", "Comma"),
    (";", "Semicolon"),
    ("(", "LParen"),
    (")", "RParen"),
    ("{", "LBrace"),
    ("}", "RBrace"),
    ("[", "LBracket"),
    ("]", "RBracket"),
    ("@", "At"),
    ("#", "Hash"),
    ("$", "Dollar"),
    ("/", "Slash"),
    ("\\", "Backslash"),
    ("`", "Backtick"),
    ("\"", "DQuote"),
    ("\'", "Quote"),
    (" ", "Space"),
    ("\t", "Tab"),
    ("\n", "Newline"),
    ("\r", "CarRet"),
    ("_", "Underscore")
  ]

multiSpecMap :: [(Text, Text)]
multiSpecMap =
  [ ("<=", "Leq"),
    (">=", "Geq"),
    ("->", "RArrow"),
    ("<-", "LArrow"),
    ("=>", "FatRArrow")
  ]

collectNames :: Grammar -> [Text]
collectNames g = map nodeName (gramNodes g) <> collectTokenNames
  where
    collectTokenNames :: [Text]
    collectTokenNames = map (handleTokenName . tokenName) grammarTokens

    grammarTokens :: [Token]
    grammarTokens = nub [t | Node _ r <- gramNodes g, t <- collectRuleTokens r]

    collectRuleTokens :: Rule -> [Token]
    collectRuleTokens (RuleLabeled _ r) = collectRuleTokens r
    collectRuleTokens (RuleNode _) = []
    collectRuleTokens (RuleToken t) = [t]
    collectRuleTokens (RuleSeq rs) = concatMap collectRuleTokens rs
    collectRuleTokens (RuleAlt rs) = concatMap collectRuleTokens rs
    collectRuleTokens (RuleOpt r) = collectRuleTokens r
    collectRuleTokens (RuleRep r) = collectRuleTokens r

    handleTokenName :: Text -> Text
    handleTokenName name =
      case lookup name multiSpecMap of
        Just special -> special
        Nothing ->
          -- If the name starts with a special character, check specMap and keep appending
          -- special names until no more are found. Then convert the rest using snakeToPascal.
          case lookup (Text.take 1 name) specMap of
            Just special -> special <> handleTokenName (Text.drop 1 name)
            Nothing -> snakeToPascal name

    snakeToPascal :: Text -> Text
    snakeToPascal s = Text.concat $ map capitalize (splitOn "_" s)
      where
        capitalize :: Text -> Text
        capitalize txt = toUpper (Text.take 1 txt) <> Text.drop 1 txt

genSyntaxKindDecls :: [Text] -> Text
genSyntaxKindDecls names =
  let nameMap :: [(Text, Int)] = zip names [0 ..]
      sigs = map (\n -> "pattern SyntaxKind" <> n <> " :: SyntaxKind") names
      defs =
        map
          ( \(n, ix) ->
              "pattern SyntaxKind"
                <> n
                <> " = SyntaxKind "
                <> (pack . show $ ix)
          )
          nameMap
   in Text.unlines $ zipWith (\sig def -> sig <> "\n" <> def) sigs defs
