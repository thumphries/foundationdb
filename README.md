# foundationdb

Haskell FoundationDB bindings

This is a BayHac weekend project that is very, very unlikely to be usable in
its current state. It may yet be useful as a starting point for similar work.

## Building with Nix

FoundationDB has only just landed in nixpkgs unstable, so first update your
unstable channel:

```
nix-channel --update unstable
```

From here you should be able to pop a nix-shell in which development can happen:

```
nix-shell -I nixpkgs=$HOME/.nix-defexpr/channels/unstable
cabal build
```

... and you should be able to compile the library for use elsewhere:

```
nix-build -I nixpkgs=$HOME/.nix-defexpr/channels/unstable
```


This Nix setup may not be idiomatic. I don't really know what I'm doing.
PRs welcome.
