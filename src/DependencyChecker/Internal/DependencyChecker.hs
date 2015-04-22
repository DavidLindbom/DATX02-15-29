module DependencyChecker.Internal.DependencyChecker (dependencyCheck) where

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

import Control.Monad.Trans.State
import qualified Data.Map as Map
import Data.Graph


type Env = Map.Map FilePath [FilePath]

type CheckM a = StateT Env IO a

emptyEnv :: Env
emptyEnv = Map.empty

-- | Recursively check module dependencies and generate a topologically
-- sorted compilation order, such that all dependencies of a module are
-- compiled before that module.
dependencyCheck :: FilePath -> IO [FilePath]
dependencyCheck file = do
  themap <- execStateT (reccheck file) emptyEnv
  let graph = map (\(k,v) -> (k, k, v)) $ Map.assocs themap
  let topsorted = stronglyConnComp graph
  compileOrder <- acyclicOrder topsorted
  return compileOrder

-- | Extract the module filepaths from the SCC datatype, meanwhile asserting
-- that no cyclical dependencies are present.
acyclicOrder :: (Show a) => [SCC a] -> IO [a]
acyclicOrder =
  mapM cycleCheck
  where
    cycleCheck (AcyclicSCC x) = return x
    cycleCheck (CyclicSCC xs) = fail $ "Cyclic dependencies found: " ++
                                       show xs

reccheck :: FilePath -> CheckM ()
reccheck = undefined

check :: FilePath -> CheckM [FilePath]
check = undefined
