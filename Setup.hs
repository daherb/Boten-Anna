import Distribution.Simple
import System.Process

main = defaultMainWithHooks simpleUserHooks{ postBuild = gfPostBuild }
  where
    gfPostBuild _ _ _ _ = callCommand "gf -make AnnaEngQ.gf AnnaEngR.gf AnnaAct.gf"
