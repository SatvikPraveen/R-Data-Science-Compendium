#!/usr/bin/env bash
# Install all dependencies, run the test suite and check the package.
#
#   ./setup.sh
set -euo pipefail
cd "$(dirname "$0")"

command -v Rscript >/dev/null 2>&1 || {
  echo "R is not installed. See https://cloud.r-project.org/" >&2
  exit 1
}

Rscript setup.R
make install
make test
echo
echo "Setup complete. Next steps:"
echo "  make check      # full R CMD check"
echo "  make analysis   # reproduce the simulation studies and paper"
