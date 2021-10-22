{ pkgs ? import ../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "aurum-spec";
  texFiles = [ "aurum-changes" ];
  meta = with lib; {
    description = "Charles ledger specification";
    license = licenses.asl20;
    platforms = platforms.linux;
  };
  src = latex.filterLatex ./.;

  texInputs = {
    inherit (texlive)
      scheme-small
      collection-latexextra
      collection-latexrecommended
      collection-mathscience
      bclogo
      ;
  };
}
