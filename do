#!/bin/sh

case "$1" in
  "dev")
    exec ghcid -c "cabal v2-repl -O0" --restart repld.cabal
    ;;

  "install")
    cabal v2-build
    cp $(cabal-plan list-bin repld) ~/.local/bin
    ;;

  "run")
    shift
    exec cabal v2-run -- repld "$@"
    ;;
esac
