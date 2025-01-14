cabal build all -O2
rm release/nn
mkdir -p release
cp dist-newstyle/build/x86_64-linux/ghc-*/neonote-0.2.0.0/x/neonote/build/neonote/neonote release/nn
strip release/nn
upx release/nn
