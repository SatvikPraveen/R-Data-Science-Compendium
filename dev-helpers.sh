#!/bin/bash

case "$1" in
  "test")
    echo "🧪 Running tests..."
    Rscript -e "testthat::test_dir('tests/')"
    ;;
  "lint")
    echo "🔍 Checking code style..."
    Rscript -e "lintr::lint_package()"
    ;;
  "docs")
    echo "📚 Building documentation..."
    Rscript -e "pkgdown::build_site()"
    ;;
  "style")
    echo "✨ Formatting code..."
    Rscript -e "styler::style_pkg()"
    ;;
  "check")
    echo "🔍 Checking package..."
    R CMD check .
    ;;
  "shiny")
    echo "🌟 Starting Shiny app..."
    Rscript -e "shiny::runApp('shiny-apps/data-dashboard')"
    ;;
  *)
    echo "Usage: $0 {test|lint|docs|style|check|shiny}"
    ;;
esac
