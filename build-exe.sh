cabal build all
cp dist-newstyle/build/x86_64-linux/ghc-9.4.4/neonote-0.1.0.0/x/neonote/build/neonote/neonote nn
strip nn
upx nn