# our packages overlay
pkgs: _:
with pkgs; {
  bccLedgerSpecsHaskellPackages = import ./haskell.nix {
    inherit config lib stdenv pkgs haskell-nix buildPackages;
  };

  cbor-diag = pkgs.callPackage ./pkgs/cbor-diag { };
  cddl = pkgs.callPackage ./pkgs/cddl { };
}
