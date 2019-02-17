{ nixpkgs ? import <nixpkgs> {}, static ? false, compiler ? "default" }:

let
  pkgs = if static then nixpkgs.pkgsMusl
                   else nixpkgs.pkgs;
  configureFlags = if static then [
    "--ghc-option=-optl=-static"
    "--extra-lib-dirs=${pkgs.gmp6.override { withStatic = true; }}/lib"
    "--extra-lib-dirs=${pkgs.zlib.static}/lib"
  ] else [];

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  ws-fetch = haskellPackages.callPackage ./ws-fetch.nix {
    inherit configureFlags;
  };

in {
  inherit ws-fetch;
}
