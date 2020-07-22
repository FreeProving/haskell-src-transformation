# Changelog

## Unreleased

- **Added command line option for front end selection**
  - A front end can be specified with `--frontend=FRONTEND ` or `-f FRONTEND`.
  - Allowed values are: `haskell-src-exts`, `ghc-lib.`
  - `haskell-src-exts` is used by default.
  - No `ghc-lib` front end has been implemented yet.

## 0.1.1.0 / 2020-04-19

 - **Fixed compile-time warnings**  
   The library and executable can now be compiled with GHC's `-Wall` and `-Werror` flags set.

## 0.1.0.0 / 2020-01-13

 - **Initial version**
