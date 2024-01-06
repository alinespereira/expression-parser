{ pkgs }: {
    deps = [
        pkgs.haskellPackages.ghc
        pkgs.haskellPackages.stack
        pkgs.haskell-language-server
    ];
}
