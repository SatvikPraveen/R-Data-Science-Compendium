#!/usr/bin/env bash
# Thin compatibility wrapper around the Makefile, e.g. ./dev-helpers.sh test
set -euo pipefail
cd "$(dirname "$0")"

case "${1:-help}" in
  test | lint | check | coverage | document | install | analysis) make "$1" ;;
  docs | site) make site ;;
  style) Rscript -e 'styler::style_pkg()' ;;
  shiny) Rscript -e "shiny::runApp('shiny-apps/data-dashboard')" ;;
  *) make help ;;
esac
