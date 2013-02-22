#ghc main.hs -threaded -rtsopts -prof -auto-all -fforce-recomp -O2
ghc main.hs -threaded -rtsopts -fforce-recomp -O2 $*
#ghc main.hs -fforce-recomp 
