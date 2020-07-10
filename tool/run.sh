#!/bin/bash

# This script can be used to run the pattern matching compiler for debugging
# purposes. It compiles the executable using Cabal. The command line options
# are forwarded to the pattern matching compiler's executable.
#
# During debugging the `-Wwarn` flag of GHC is set such that warnings are not
# fatal. However, the executable is intended to compile without warnings, in
# production. Thus, the `-Wall` and `-Werror` flags are set by default.
# Before local changes are pushed, all warnings should be fixed. Otherwise,
# the CI pipeline will fail.

# Change into the package's root directory.
script=$(realpath "$0")
script_dir=$(dirname "$script")
root_dir=$(dirname "$script_dir")
cd "$root_dir"

# Disable fatal warnings and forward arguments to the executable.
cabal new-run exe:haskell-src-transformations --ghc-option -Wwarn -- "$@"
