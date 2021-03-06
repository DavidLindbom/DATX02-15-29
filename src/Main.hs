module Main (main) where

import System.Environment ( getArgs )
import System.Console.GetOpt
import System.Exit

import Control.Monad

import DependencyChecker.DependencyChecker (dependencyCheck)

import Parser.PrintHopper (printTree)
import Parser.ParHopper
import Parser.LayoutHopper
import Parser.Parser

import Utils.ErrM
import Utils.HipFile

import Renamer.Renamer (transform)
import TypeChecker.TypeChecker (typeCheck)
import CodeGenerator.CodeGenerator
import Utils.BeamWriter

data Flag = Verbose | Parse | AST | TypeCheck | Core | NoBeam
  deriving (Show, Eq)

options :: [OptDescr Flag]
options = 
  [ Option ['v'] ["verbose"]   (NoArg Verbose)   "show everything thats happening"
  , Option ['p'] ["parse"]     (NoArg Parse)     "write a hopper parse file"
  , Option ['a'] ["ast"]       (NoArg AST)       "print the ast to haskell file"
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
  
  case files of
    [] -> putStr (usageInfo header options ++ footer) 
    fs -> forM_ fs (compile opts)
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

-- | Compile a module along with all its dependencies.
compile :: [Flag] -> FilePath -> IO ()
compile opts f = do
  fs <- dependencyCheck f
  forM_ fs (compileFile opts)

-- | Compile a single file.
compileFile :: [Flag] -> FilePath -> IO ()
compileFile opts f = do
  -- Verbose aware writer
  let write = write' opts

  -- Opens Err if flag is set
  let whenFlag = whenFlag' opts

  let f' = dropHPR f
  
  code <- readFile f
  
  -- Parse
  let treeE = parse code

  whenFlag Parse treeE $ \tree -> do
    writeFile (f'++".parse.hpr") (printTree tree)
    write $ "Wrote parse file to " ++ f' ++ ".parse.hpr"

  -- Convert to AST
  let astE = treeE >>= transform

  whenFlag AST astE $ \ast -> do
    writeFile (f'++".ast.hs") ("import AST.AST\nast="++show ast)
    write $ "Wrote ast to " ++ f' ++ ".ast.hs"

  -- Type information for imports
  -- importTypes :: (Map Identifier Type, Map Constructor Type)
  -- TODO: integrate into typechecker
  importTypes <- getImportsInfo astE

  -- Typechecker
  let typedE = astE >>= typeCheck
  
  whenFlag TypeCheck typedE $ \typed -> do
    writeFile (f'++".typed.hs") ("import AST.AST\ntyped="++show typed)
    write $ "Wrote typed ast to " ++ f' ++ ".typed.hs"

  -- Write .hip file
  storeModuleInfo typedE
  -- Code generation
  let coreE = typedE >>= compileModuleString

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
  write $ "Done with " ++ f ++ "!"
  where
    write' opt a = if Verbose `elem` opt
                   then putStrLn a
                   else return ()
    
    dropHPR = reverse . drop 4 . reverse

    whenFlag' opt flag a f = 
      when (flag `elem` opt) $ case a of
        Bad e -> putStrLn e >> exitFailure 
        Ok  t -> f t 

