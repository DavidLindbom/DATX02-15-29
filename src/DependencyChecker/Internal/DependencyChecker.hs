module DependencyChecker.Internal.DependencyChecker where

{-
insikter från workshop

problem som dep-check ska lösa
  tillhandahålla typinfo från importerade moduler till tc
  ordning för att kompilera modulgraf

dep check
  arbetar på
    import-data
    modulnamn
    katalogstruktur
  rapporterar
    fel namn på modul givet katalogstruktur
    cykliska modulberoenden
    importerade moduler som ej existerar

idéer
  dep-check genererar endast en kompilerings/typcheckningsordning där
  ordningen garanterar att nödvändiga hip/beamfiler genererats i ett
  tidigare steg
  dependencyCheck :: Kvalificerat modulnamn
                  -> [Root path] -- sorterad i prioritetsordning
                  -> IO (Err [File path]) -- [Filepath] topologiskt sorterad
  main = do
    mod:roots <- getArgs
    fs <- dependencyCheck mod roots
    ps <- map parse fs
    rs <- map renamer ps
    map typechecker rs  -- producerar och läser .hip-filer
    map codegen rs -- läser .hip-filer

insikter
  importer måste till en början vara fullt kvalificerade
  dep-check kommer behöva göra någon sorts parsning för att komma åt modulnamn
  och importer, till en början den vanliga parsern vilket gör att dep-check
  kommer kunna ge parser-fel
-}

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Graph
import System.Directory
import System.FilePath

import AST.AST (Module (..), Modulename)
import Parser.Parser
import Renamer.Renamer (transform)
import Utils.ErrM

type Env = Map.Map FilePath [FilePath]

type CheckM a = StateT Env IO a

emptyEnv :: Env
emptyEnv = Map.empty

-- | Recursively check module dependencies and generate a topologically
-- sorted compilation order, such that all dependencies of a module are
-- compiled before that module.
dependencyCheck :: FilePath -> IO [FilePath]
dependencyCheck fp =
  if not $ isValid fp
    then fail $ "Bad file path " ++ fp
    else do
      fp' <- makeAbsolute fp
      themap <- execStateT (reccheck fp') emptyEnv
      let graph = map (\(k, v) -> (k, k, v)) $ Map.assocs themap
      let topsorted = stronglyConnComp graph
      acyclicOrder topsorted

-- | Extract the module filepaths from the SCC datatype, meanwhile asserting
-- that no cyclical dependencies are present.
acyclicOrder :: (Show a) => [SCC a] -> IO [a]
acyclicOrder =
  mapM cycleCheck
  where
    cycleCheck (AcyclicSCC x) = return x
    cycleCheck (CyclicSCC xs) = fail $ "Cyclic dependencies found: " ++
                                       show xs

-- | Recursively call check on imported statements.
-- Base cases are the empty list and file paths already processed.
reccheck :: FilePath -> CheckM ()
reccheck fp = do
  notProcessed <- gets (Map.notMember fp)
  when notProcessed process
  where
    process = do
      fps <- check fp
      mapM_ reccheck fps

-- | Check a unit of compilation.
-- Checks if file exists and if module name is correct relative to file path.
-- Puts the module file path and the list of imported modules in the
-- environment.
-- Returns the list of imported modules in the module.
check :: FilePath -> CheckM [FilePath]
check fp = do
  fp' <- liftIO $ makeAbsolute fp
  f <- liftIO $ readFile fp'
  case parse f of
    Ok m  -> case transform m of
      Ok (Mod name _ imps _ _) -> do
        let mfp = toFilePath name
        mfp' <- liftIO $ makeAbsolute mfp
        if fp' == mfp'
          then do
            let fps = map toFilePath imps
            fps' <- liftIO $ mapM makeAbsolute fps
            modify (Map.insert fp' fps')
            return imps
          else fail $ "Bad module name '" ++ name ++ "' in path '"
               ++ fp'
      Bad msg -> fail msg
    Bad msg -> fail msg

-- | Verifies module name matches file path
verifies :: FilePath -> Modulename -> Bool
verifies fp name = fp == toFilePath name

-- | Takes a file path and makes it absolute relative the working directory
-- if it isn't already absolute
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute fp =
  if isAbsolute fp
  then return fp
  else do
    dir <- getCurrentDirectory
    return $ dir </> fp

-- | Convert a module name to a file path
toFilePath :: Modulename -> FilePath
toFilePath fps = replace '.' pathSeparator fps <.> hopExt

hopExt :: String
hopExt = "hop"

-- | Replace each occurence of the first argument with the second in the list.
replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\z -> if z == x then y else z)
