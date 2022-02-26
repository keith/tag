{ pkgs ? import <nixpkgs> {} }:
with pkgs;
stdenv.mkDerivation rec {
  pname = "tag";
  version = "0.8.0";
  src = ./.;
  buildPhase = ''
    ${clang}/bin/clang++ -std=c++17 -O3 tag.cpp -o tag
  '';
  installPhase = ''
    mkdir -p $out/bin
    mv tag $out/bin
  '';
}
