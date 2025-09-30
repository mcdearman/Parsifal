module Parsifal.Gen where

import Data.Text (Text, intercalate, unpack)
import Parsifal.Ungrammar

collectNames :: Grammar -> Text
collectNames g = intercalate "\n" $ map nodeName (gramNodes g) <> collectTokenNames g
  where
    collectTokenNames :: [Text]
    collectTokenNames = [tokenName t | Node _ r <- gramNodes g, t <- collectTokens r]

    collectTokens :: Rule -> [Token]
    collectTokens (RuleLabeled _ r) = collectTokens r

genSyntaxKindDecls :: [String] -> String
genSyntaxKindDecls names =
  let nameMap = zip names [0 ..]
      sigs = map (\n -> "pattern SyntaxKind" <> n <> " :: SyntaxKind") names
      defs =
        map
          ( \(n, ix) ->
              "pattern SyntaxKind"
                <> n
                <> " = SyntaxKind "
                <> show ix
          )
          nameMap
   in unlines $ zipWith (\sig def -> sig <> "\n" <> def) sigs defs
