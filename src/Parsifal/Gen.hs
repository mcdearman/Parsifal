module Parsifal.Gen where

import Data.Text (Text, intercalate, unpack)

genSyntaxKindDecls :: [Text] -> String
genSyntaxKindDecls names =
  let nameMap = zip names [0 ..]
      sigs = map (\n -> "pattern SyntaxKind" <> unpack n <> " :: SyntaxKind") names
      defs =
        map
          ( \(n, ix) ->
              "pattern SyntaxKind"
                <> unpack n
                <> " = SyntaxKind "
                <> show ix
          )
          nameMap
   in unlines $ sigs ++ defs
