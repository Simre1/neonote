cabal build all -O2
rm release/nn-linux-x86
mkdir -p release
cp dist-newstyle/build/x86_64-linux/ghc-9.*/neonote-0.3.0.0/x/neonote/build/neonote/neonote release/nn-linux-x86
strip release/nn-linux-x86
upx release/nn-linux-x86


gh release delete latest -y
gh release create latest 'release/nn-linux-x86#NeoNote - Linux x86' -p -t Latest --notes "This release corresponds to the latest version of the main branch"

