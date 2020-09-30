# Changelog

## 0.2.0.0 / 2020-09-30

- **Added a run script**
  - Usage instructions are included in the readme.

- **Added GHC-lib front end and command line option for front end selection**
  - A front end can be specified with `--frontend=FRONTEND ` or `-f FRONTEND`.
  - Allowed values are: `haskell-src-exts`, `ghc-lib.`
  - `haskell-src-exts` is used by default.

- **Improved error messages**
  - Added and updated error messages state the cause of many errors more clearly.
  - Warnings for skipped unsupported features have been added.
  - Most error messages contain input code excerpts showing the location the error occurred.

- **Added support for module imports**
  - Regular, qualified and aliased imports between input modules are supported.
  - Import specifications (explicitly importing or `hiding` certain types, constructors or functions) are not supported and skipped.

- **Source spans are now largely retained during the compilation**
  - This does not affect the output of the pattern matching compiler, but can be useful when using the compiler as a library.

- **Various changes in the internal structure**

- **Various bug fixes**

## 0.1.1.0 / 2020-04-19

 - **Fixed compile-time warnings**  
   The library and executable can now be compiled with GHC's `-Wall` and `-Werror` flags set.

## 0.1.0.0 / 2020-01-13

 - **Initial version**
