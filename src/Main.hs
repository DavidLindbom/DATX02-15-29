module Main (main) where

import System.Environment ( getArgs )

import Parser.ParHopper
import Parser.LayoutHopper
import Parser.ErrM

import Renamer.Renamer (transformModule)
import CodeGenerator.CodeGenerator

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> putStr usage
    fs -> (flip mapM_) fs $ \f -> do
      code <- readFile f
      case compile code of
        Bad str -> putStrLn $ "ERROR: " ++ str
        Ok  str -> do
          let f' = (reverse . drop 3 . reverse $ f) ++ "core"
          writeFile f' str

compile :: String -> Err String
compile str = do
  parsetree <- parse str
  ast <- transformModule parsetree
  -- Other passes...
  return $ compileModuleString ast
  where 
    parse = pModule . resolveLayout True . myLexer

usage :: String
usage = unlines $ [ "The Hopper Language Compiler"
                  , ""
                  , "Usage: hopper FILES"
                  , ""
                  , "Created by:"
                  , "Chalmers University Bachelor Project group DATX2-15-29"
                  ]


