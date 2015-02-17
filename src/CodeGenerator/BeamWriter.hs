{-| 
Module      : BeamWriter
Description : Writes .beam files from CoreErlang data
Copyright   : -
License     : -
Status      : Stable

Module that generates a .beam file from an 
abstract module as seen in Language.CoreErlang.Syntax

This module is part of the Hopper language project
-}

module BeamWriter where

import System.Cmd
import System.Directory
import System.IO

-- |The 'writeBeam' function compiles a .beam file from
--  the given abstract Language.CoreErlang.Syntax.Module
--  using the erlc program
writeBeam :: String -> String -> IO ()
writeBeam moduleName code = 
  do writeFile coreFile code
     rawSystem "erlc" [coreFile]
     removeFile coreFile
  where coreFile = moduleName ++ ".core"
