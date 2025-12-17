module Parsifal.Gen (genModule) where

import Data.List (nub)
import Data.Text (Text, pack, splitOn, toUpper, unpack)
import qualified Data.Text as Text
import Parsifal.Ungrammar

genModule :: Grammar -> Text -> String
genModule g moduleName =
  unpack $
    Text.unlines
      [ genModuleHeader moduleName,
        genImportDecls,
        genRedGreenTrees,
        genSyntaxKindDecls $ collectNames g
      ]

genModuleHeader :: Text -> Text
genModuleHeader moduleName =
  Text.unlines
    [ "{-# LANGUAGE PatternSynonyms #-}",
      "{-# LANGUAGE GeneralisedNewtypeDeriving #-}",
      "module " <> moduleName <> " where"
    ]

genImportDecls :: Text
genImportDecls =
  Text.unlines
    [ "import Data.ByteString (ByteString)",
      "import qualified Data.ByteString as BS",
      "import Data.Primitive (Prim)",
      "import Data.Traversable (mapAccumL)",
      "import Data.Vector (Vector)",
      "import qualified Data.Vector as V",
      "import Data.Word (Word16)",
      "import Data.Maybe ( listToMaybe, mapMaybe )"
    ]

genRedGreenTrees :: Text
genRedGreenTrees =
  Text.unlines
    [ """
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
          mapAccumL (\\o c -> (o + childLength c, SyntaxNode o (Just n) c)) off (greenChildren g)
        where
          childLength :: Green -> Int
          childLength (GreenToken (Token _ text)) = BS.length text
          childLength (GreenNode (Node _ _ w)) = fromIntegral w

      findMap :: (a -> Maybe b) -> [a] -> Maybe b
      findMap f = listToMaybe . mapMaybe f
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
