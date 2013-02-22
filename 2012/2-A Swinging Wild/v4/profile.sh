ghc -O2 -prof -rtsopts -auto-all -fforce-recomp main.hs
main.exe $* +RTS -p
