#!/bin/sh

case "$1" in
  "dev")
    exec ghcid -c "cabal v2-repl -O0 exe:repld" --restart repld.cabal
    ;;

  "install")
    set -e
    cabal -v0 v2-build
    cp $(cabal-plan list-bin repld) ~/.local/bin
    cp $(cabal-plan list-bin repld-send) ~/.local/bin
    ;;

  "repl")
    cabal v2-repl -O0 -O exe:repld
    ;;

  "run")
    shift
    exec cabal v2-run -- repld "$@"
    ;;

  *)
    echo "Usage: do COMMAND"
    echo ""
    echo "Available commands:"
    echo ""
    echo "  dev"
    echo "  install"
    echo "  repl"
    echo "  run"
esac
