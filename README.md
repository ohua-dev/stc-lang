# statefulness

To make this work, please change the `extra-lib-dirs` path in `statefulness.cabal` accordingly.

Build:
```
stack build
```

Run:
```
stack exec statefulness-exe
```

Tests:
```
stack test --resolver lts-9.21
```
(Currently, the streams-based version does not work with the development setup (GHC 8.2.2) of the project.
 This is due to problems with type resolution. It is therefore necessary to change the GHC version if the
 streams-based version is used in other code!)
