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

module CodeGenerator.BeamWriter (writeBeam) where

import System.Cmd
import System.Directory
import Control.Monad

-- |The 'writeBeam' function compiles a .beam file from
--  the given core erlang code string
--  using the erlc program
writeBeam :: String -> String -> Bool -> IO ()
writeBeam path code keepCore = 
  do writeFile coreFile code
     _ <- rawSystem "erlc" [coreFile]
     unless keepCore $ removeFile coreFile
  where coreFile = path ++ ".core"


-- TODO:
-- Writing .beam to wrong dir
