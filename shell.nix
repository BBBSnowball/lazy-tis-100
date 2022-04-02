{ pkgs ? import <nixpkgs> {} }:
pkgs.mkShell {
  packages = with pkgs; [ stack ghc cabal-install llvm numactl ncurses5 ncurses gmp autoPatchelfHook ];
  shellHook = ''
    export LC_ALL=C.UTF-8
  '';
}
