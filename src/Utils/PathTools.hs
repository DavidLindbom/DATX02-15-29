module Utils.PathTools where

import System.Directory
import System.FilePath

import AST.AST

-- | Takes a file path and makes it absolute relative the working directory
-- if it isn't already absolute
makeAbsolutePath :: FilePath -> IO FilePath
makeAbsolutePath fp =
  if isAbsolute fp
  then return fp
  else do
    dir <- getCurrentDirectory
    return $ dir </> fp

-- | Convert a module name to a file path
toFilePath :: Modulename -> FilePath
toFilePath fps = replace '.' pathSeparator fps <.> hopExt

hopExt :: String
hopExt = "hpr"

-- | Replace each occurence of the first argument with the second in the list.
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)
