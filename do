#!/bin/sh

case "$1" in
  "dev")
    exec ghcid -c "cabal v2-repl -O0" --restart repld.cabal
    ;;

  "install")
    set -e
    cabal -v0 v2-build
    cp $(cabal-plan list-bin repld) ~/.local/bin
    ;;

  "repl")
    cabal v2-repl -O0
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
