
module Compiler.Compiler where

import Parser.LexHopper
import Parser.ParHopper
import Parser.SkelHopper
import Parser.PrintHopper
import Parser.AbsHopper
import Parser.LayoutHopper
import Utils.ErrM

import Renamer.Rename (rename)
import TypeChecker.TC (typecheck)

--Takes a given module (e.g. M1.M2.M3),
--parses;
--renames;
--typechecks;
--codegens;
--c('M1.M2.M3',to_core)
compile :: [String] -> IO ()
compile mds = do s <- readFile $ (foldr1 (\m ms -> m ++ "/" ++ ms) mds)++".hpr"
                 case parseModule s of
                   Bad s -> putStrLn $ "Compiler error: " ++ s
                   Ok absmodule -> let mod = (rename absmodule)
                                   in case typecheck 
                                          (cons mod)-- ++imports
                                          (defs mod) of
                                        Left message -> putStrLn $
                                                        "TC error:" ++ s
                                        Right _ -> error "TBD"
parseModule s = pModule $ myLexer s
                 