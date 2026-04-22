{
  description = "Tlon: A game about obligatory forms of gambling.";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-25.11";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        hpkgs = pkgs.haskellPackages;
        tlon = hpkgs.callCabal2nix "tlon" ./. { };
        mkApp = exePath: flake-utils.lib.mkApp { drv = tlon; inherit exePath; };
      in
      {
        packages = {
          tlon = tlon;
          default = tlon;
        };

        apps = {
          tlon = mkApp "/bin/tlon";
          tlon-web = mkApp "/bin/tlon-web";
          default = mkApp "/bin/tlon";
        };

        devShells.default = hpkgs.shellFor {
          packages = p: [ tlon ];
          buildInputs = [
            hpkgs.cabal-install
            hpkgs.haskell-language-server
            hpkgs.hlint
            hpkgs.fourmolu
            hpkgs.ghcid
            hpkgs.cabal-fmt
            pkgs.curl
            pkgs.fd
            pkgs.just
            pkgs.jq
            pkgs.lsof
            pkgs.nodejs
            pkgs.ripgrep
            pkgs.watchexec
          ];
          withHoogle = false;
        };
      });
}
