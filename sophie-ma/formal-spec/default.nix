{ pkgs ? import ../../nix/default.nix {} }:

with pkgs;

latex.buildLatex {
  name = "sophie-ma-spec";
  texFiles = [ "sophie-ma" ];
  meta = with lib; {
    description = "Sophie multi-asset specification";
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
