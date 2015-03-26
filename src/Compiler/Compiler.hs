
module Compiler.Compiler where

import Parser.LexHopper
import Parser.ParHopper
import Parser.SkelHopper
import Parser.PrintHopper
import Parser.AbsHopper
import Parser.LayoutHopper
import Utils.ErrM

import Language.CoreErlang.Pretty (prettyPrint)

import Renamer.Rename (rename)
import TypeChecker.TC (typecheckModule)
import CodeGenerator.CodeGen (codeGen)

--Takes a given module (e.g. M1.M2.M3),
--parses;
--renames;
--typechecks;
--codegens;
--c('M1.M2.M3',to_core)
compile :: [String] -> IO ()
compile mds = do s <- readFile $ (foldr1 (\m ms -> m ++ "/" ++ ms) mds)++".hpr"
                 case parseModule s of
                   Bad s -> putStrLn $ "Parser error: " ++ s
                   Ok absmodule -> let mod = (rename absmodule)
                                   in case typecheckModule mod
                                      of
                                        Left message -> putStrLn $
                                                        "TC error:" ++ message
                                        Right tcm -> writeFile 
                                                     ((mds>>=(++"."))++"CORE")
                                                     $ prettyPrint $
                                                       codeGen tcm
parseModule s = pModule $ myLLexer s
                 where myLLexer = resolveLayout True . myLexer