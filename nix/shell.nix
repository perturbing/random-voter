{ repoRoot, inputs, pkgs, system, lib }:

cabalProject:

{
  name = "random-voter";

  packages = lib.traceSeq inputs.CHaP [
    pkgs.jq
    inputs.cardano-node.packages.cardano-node
    inputs.cardano-node.packages.cardano-cli
  ];

  preCommit = {
    cabal-fmt.enable = true;
    cabal-fmt.extraOptions = "--no-tabular";
    nixpkgs-fmt.enable = true;
    shellcheck.enable = false;
    fourmolu.enable = true;
    fourmolu.extraOptions = "-o -XCPP";
    hlint.enable = true;
  };

  tools = {
    haskell-language-server =
      let
        hlsProject = pkgs.haskell-nix.cabalProject' {
          name = "haskell-language-server";
          src = inputs.iogx.inputs.haskell-nix.inputs."hls-2.6";
          configureArgs = "--disable-benchmarks --disable-tests";
          compiler-nix-name = lib.mkDefault "ghc96";
          modules = [ ];
        };
      in
      hlsProject.hsPkgs.haskell-language-server.components.exes.haskell-language-server;
  };

  scripts = {
    deploy-local-testnet = {
      description = "Start and run an ephemeral local testnet";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)/local-testnet
        [ -d example ] || scripts/babbage/mkfiles.sh
        example/run/all.sh
      '';
    };

    purge-local-testnet = {
      description = "Cleanup the local testnet directory";
      group = "general";
      exec = ''
        set -e
        set -u
        set -o pipefail

        cd $(git rev-parse --show-toplevel)/local-testnet
        rm -rf example logs
      '';
    };
  };

  shellHook = ''
    export REPO_ROOT="$(pwd)"
    export CARDANO_NODE_SOCKET_PATH="$REPO_ROOT/local-testnet/example/node-spo1/node.sock"
  '';
}
