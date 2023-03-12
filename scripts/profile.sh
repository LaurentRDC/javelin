cabal clean
cabal run profiling --project-file=cabal.project.profiling 
hp2ps -e8in profiling.hp