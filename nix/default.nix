{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, sourcesOverride ? {}
, gitrev ? null
}:
let
  sources = import ./sources.nix { inherit pkgs; }
    // sourcesOverride;
  tbcoNixMain = import sources.tbco-nix {};
  haskellNix = (import sources."haskell.nix" { inherit system sourcesOverride; }).nixpkgsArgs;
  # use our own nixpkgs if it exists in our sources,
  # otherwise use tbcoNix default nixpkgs.
  nixpkgs = if (sources ? nixpkgs)
    then (builtins.trace "Not using TBCO default nixpkgs (use 'niv drop nixpkgs' to use default for better sharing)"
      sources.nixpkgs)
    else (builtins.trace "Using TBCO default nixpkgs"
      tbcoNixMain.nixpkgs);

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/The-Blockchain-Company/haskell.nix)
    haskellNix.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ tbcoNixMain.overlays.haskell-nix-extra
    # tbcoNix: nix utilities and niv:
    ++ tbcoNixMain.overlays.tbcoNix
    # libsodium fork
    ++ tbcoNixMain.overlays.crypto
    # our own overlays:
    ++ [
      (pkgs: _: with pkgs; {

        # commonLib: mix pkgs.lib with tbco-nix utils and our own:
        commonLib = lib // tbcoNix
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources and overlays
          // { inherit overlays sources; };

        # Functions for building LaTeX documents.
        latex = import ./latex.nix { inherit (pkgs) stdenv lib texlive; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.config // config;
  };

in pkgs
