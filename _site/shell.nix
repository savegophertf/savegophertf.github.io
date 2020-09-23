{pkgs ? import <nixpkgs> {} }:

let my_rb = pkgs.ruby.withPackages( ps: with ps; [
  jekyll
]);
in pkgs.mkShell {
  inputsFrom = with pkgs; [
    bundix
    which
    my_rb
  ];
}