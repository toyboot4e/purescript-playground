{
  description = "A basic flake with a shell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
     purescript-overlay = {
       url = "github:thomashoneyman/purescript-overlay";
       inputs.nixpkgs.follows = "nixpkgs";
     };
  };

  outputs = inputs@{ nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ inputs.purescript-overlay.overlays.default ];
        };
      in
      {
        devShells.default = with pkgs; mkShell {
          nativeBuildInputs = [
            purs
	          spago-unstable
            purs-tidy-bin.purs-tidy-0_10_0
            purs-backend-es

            # bower
            # pkg-config
            # llvmPackages.bintools
          ];

          packages = [
            # atcoder-cli is from npm

            online-judge-tools
            python311Packages.selenium
            python311Packages.pyaml
            python311Packages.importlab
            nodejs
          ];
        };
      });
}
