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

import Control.Monad.Trans.State
import Data.Map


type Env = Map FilePath [FilePath]

type CheckM a = StateT Env IO a

depcheck :: FilePath -> IO [FilePath]
depcheck = undefined

reccheck :: FilePath -> CheckM ()
reccheck = undefined

check :: FilePath -> CheckM [FilePath]
check = undefined
