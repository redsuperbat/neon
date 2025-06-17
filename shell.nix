{pkgs ? import <nixpkgs> {}}:
pkgs.mkShell {
  name = "neon";

  buildInputs = [
    pkgs.rustc
    pkgs.cargo
    pkgs.rustfmt
    pkgs.clippy
    pkgs.cargo-watch
    pkgs.rust-analyzer
    pkgs.nodejs
    pkgs.yarn
    pkgs.wasm-pack
  ];
}
