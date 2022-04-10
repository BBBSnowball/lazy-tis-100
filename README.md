# lazy-tis100

This is lazy!

1. Haskell: It is written in Haskell because I'm lazy. Also, Haskell uses lazy evaluation.
2. Lazy Evaluation of Nodes: It won't compute nodes unless it has too: Nobody wants to read a value from that node? Who cares whether it would have produced one.
3. Lazy Evaluation of Values: It won't commit on values unless it has to: What's the value in this register? Let's call it x1 until some jump depends on it.
4. Program Synthesis: Assembly is hard! Can't the computer do it for us? Well... maybe?


## Notes for myself

Haskell on Debian in VM on Apple M1:
- install VS Code via tar.gz, not nix
- run with `--verbose -w` and apply suid rights as specified
- `apt install libtinfo5 libtinfo-dev`, see https://discourse.haskell.org/t/using-stack-on-raspberry-pi/2363/7
  - GHC also needs llvm-11 but Debian adds a "-11" suffix to everything so this doesn't work so well. We use Nixpkgs, instead.
- https://medium.com/@dogwith1eye/setting-up-haskell-in-vs-code-with-stack-and-the-ide-engine-81d49eda3ecf
- `nix-shell -p stack ghc cabal-install llvm numactl ncurses5 ncurses gmp`
- `export LANG=C.UTF-8`, see https://stackoverflow.com/questions/63746826/what-might-cause-commitandreleasebuffer-invalid-argument-invalid-character
- `stack ghc -- --version`
- `git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules && cd haskell-ide-engine && stack ./install.hs hie-9.0.2 && build-doc-9.0.2`
- Fails with an error like this: https://gitlab.haskell.org/ghc/ghc/-/issues/18228

This may also be useful:
- `nix-shell -p autoPatchelfHook numactl ncurses5 ncurses gmp --run "autoPatchelf /home/parallels/.stack/programs/"`
- `stack exec -- ghcid -c="stack ghci src/Lib.hs test/Spec.hs" -T="main" --warnings $@ --reload src`
- `stack ghci src/Lib.hs test/Tests/ASpec.hs src/LazyTIS100/Parser.hs --ghci-options -XOverloadedStrings`
