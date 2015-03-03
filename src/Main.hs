module Main (main) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import System.Exit

import Control.Monad

import Parser.PrintHopper (printTree)
import Parser.ParHopper
import Parser.LayoutHopper
import Parser.ErrM

import Renamer.Renamer (transformModule)
import CodeGenerator.CodeGenerator
import Utils.BeamWriter

data Flag = Verbose | Parse | TypeCheck | Core | NoBeam
  deriving (Show, Eq)

options :: [OptDescr Flag]
options = 
  [ Option ['v'] ["verbose"]   (NoArg Verbose)   "show everything thats happening"
  , Option ['p'] ["parse"]     (NoArg Parse)     "write a hopper parse file"
  , Option ['t'] ["typecheck"] (NoArg TypeCheck) "print the type checked tree" 
  , Option ['c'] ["core"]      (NoArg Core)      "write a erlang core file"
  , Option ['b'] ["nobeam"]    (NoArg NoBeam)    "don't do the erlc compilation"
  ]


main :: IO ()
main = do
  args <- getArgs
  main' args

main' :: [String] -> IO ()
main' args = do
  (opts, files) <- argparse args
  
  -- Verbose aware writer
  let write = write' opts

  -- Opens Err if flag is set
  let whenFlag = whenFlag' opts

  case files of
    [] -> putStr (usageInfo header options ++ footer) 
    fs -> (flip mapM_) fs $ \f -> do
      let f' = dropHPR f
      
      code <- readFile f
      
      -- Parse
      let treeE = parse code

      whenFlag Parse treeE $ \tree -> do
        writeFile (f'++".parse.hpr") (printTree tree)
        write $ "Wrote parse file to " ++ f' ++ ".parse.hpr"

      -- Convert to AST
      let astE = treeE >>= transform

      -- Typechecker
      -- todo

      -- whenFlag TypeCheck

      -- Code generation
      let coreE = astE >>= compileModuleString 

      whenFlag Core coreE $ \core -> do
        writeFile (f'++".core") core
        write $ "Wrote core file to " ++ f' ++ ".core"

      -- erlc compilation
      when (NoBeam `notElem` opts) $ do
        case coreE of
          Bad e -> putStrLn e >> exitFailure
          Ok  c -> do
            write $ "Running erlc on " ++ f' ++ ".core"
            writeBeam f' c (Core `elem` opts)

  write "Done!"
  where 
    argparse argv = case (getOpt Permute options argv) of
                      (o,n,[] ) -> return (o,n)
                      (_,_,err) -> do
                        putStr (concat err ++ usageInfo header options ++ footer)
                        exitFailure
    
    header = unlines $ [ "The Hopper Language Compiler"
                       , "hopper [OPTIONS] FILES..."
                       ]
    
    footer = unlines $ [ ""
                       , "Created by: Chalmers University Bachelor Project DATX2-15-29"
                       , "2015 All rights reserved"
                       ]
    
    write' opt a = if Verbose `elem` opt
                   then putStrLn a
                   else return ()
    
    dropHPR = reverse . drop 4 . reverse

    parse = pModule . resolveLayout True . myLexer

    whenFlag' opt flag a f = 
      when (flag `elem` opt) $ case a of
        Bad e -> putStrLn e >> exitFailure 
        Ok  t -> f t 


