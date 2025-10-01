module Parsifal.Gen where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text, pack, splitOn, toUpper)
import qualified Data.Text as Text
import Parsifal.Ungrammar

genModuleHeader :: Text -> Text
genModuleHeader moduleName =
  Text.unlines
    [ "{-# LANGUAGE PatternSynonyms #-}",
      "module " <> moduleName <> " where"
    ]

genImportDecl :: Text -> Text
genImportDecl name = "import " <> name

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
    collectTokenNames = [handleTokenName $ tokenName t | Node _ r <- gramNodes g, t <- Set.toList $ Set.fromList $ collectTokens r]

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

    -- collectTokens :: Rule -> [Token]
    -- collectTokens r = Set.toList $ go r
    --   where
    --     go :: Rule -> Set Token
    --     go (RuleLabeled _ r') = go r'
    --     go (RuleNode _) = Set.empty
    --     go (RuleToken t) = Set.singleton t
    --     go (RuleSeq rs) = foldMap go rs
    --     go (RuleAlt rs) = foldMap go rs
    --     go (RuleOpt r') = go r'
    --     go (RuleRep r') = go r'

    collectTokens :: Rule -> [Token]
    collectTokens (RuleLabeled _ r) = collectTokens r
    collectTokens (RuleNode _) = []
    collectTokens (RuleToken t) = [t]
    collectTokens (RuleSeq rs) = concatMap collectTokens rs
    collectTokens (RuleAlt rs) = concatMap collectTokens rs
    collectTokens (RuleOpt r) = collectTokens r
    collectTokens (RuleRep r) = collectTokens r

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
