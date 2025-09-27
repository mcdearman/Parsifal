module Main where

import Data.Text (pack)
import Parsifal.Parser (parseGrammar)
import Text.Pretty.Simple (pPrint)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  contents <- readFile "examples/ungram.ug"
  case parseGrammar (pack contents) of
    Left err -> putStrLn $ "Error parsing grammar: " ++ errorBundlePretty err
    Right grammar -> pPrint grammar