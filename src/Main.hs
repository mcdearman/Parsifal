module Main where

import Data.Text (pack, unpack)
import Parsifal.Gen (genModule)
import Parsifal.Parser (parseGrammar)
import Text.Megaparsec (errorBundlePretty)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = do
  contents <- readFile "examples/lc.ug"
  case parseGrammar (pack contents) of
    Left err -> putStrLn $ "Error parsing grammar: " ++ errorBundlePretty err
    Right grammar ->
      writeFile "src/Green.hs" $ genModule grammar "Green"

-- files <- listDirectory "."
--   -- Keep only regular files with allowed grammar extensions
--   let isGrammar f = takeExtension f `elem` [".ungrammar", ".grammar"]
--   candidates <- filterM doesFileExist (filter isGrammar files)

--   case candidates of
--     [] ->
--       die "No grammar file found in current directory. Add a *.ungrammar or *.grammar file."
--     [inp] -> do
--       spec <- T.readFile inp
--       let out = takeBaseName inp ++ ".cst.hs"
--           outPath = "." </> out
--       T.writeFile outPath (generateCST spec)
--       hPutStrLn stderr $ "Generated: " ++ outPath
--     many ->
--       die $ unlines
--         [ "Multiple grammar files found; pick one before running with no args:"
--         , unlines (map ("  - " ++) many)
--         ]