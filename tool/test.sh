#!/bin/bash

# This script can be used to run the unit tests of the library for debugging
# purposes. The command line options are forwarded to the test suite, such
# that the `--match` option can be used to select which tests to run for
# example.
#
# During debugging the `-Wwarn` flag of GHC is set such that warnings are not
# fatal. However, the compiler is intended to compile without warnings, during
# production. Thus, the `-Wall` and `-Werror` flags are set by default.
# Before local changes are pushed, all warnings should be fixed. Otherwise,
# the CI pipeline will fail.

# Change into the library's root directory.
script=$(realpath "$0")
script_dir=$(dirname "$script")
root_dir=$(dirname "$script_dir")
cd "$root_dir"

# The file to write failure reports to.
# We pass the `--failure-report` option by default such that the user can
# add the `--rerun` option easily.
failure_report_file=".hspec-failure-report"

# Run tests with Cabal.
cabal new-run haskell-src-transformations-unit-tests \
  --ghc-option -Wwarn --                             \
  --failure-report="$failure_report_file"            \
  --rerun-all-on-success                             \
  "$@"
