with import (builtins.fetchGit {
  url = "https://github.com/NixOS/nixpkgs-channels";
  ref = "nixos-18.03";
}) {};

haskell.lib.buildStackProject {
  name = "Frames-beam";
  buildInputs = [
    ghc
    postgresql
  ];
}
