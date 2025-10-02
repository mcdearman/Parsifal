module Main where

import Data.Text (pack, unpack)
import Parsifal.Gen (genModule)
import Parsifal.Parser (parseGrammar)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  contents <- readFile "examples/rust.ug"
  case parseGrammar (pack contents) of
    Left err -> putStrLn $ "Error parsing grammar: " ++ errorBundlePretty err
    Right grammar ->
      writeFile "src/Green.hs" $ genModule grammar "Parsifal.Green"
