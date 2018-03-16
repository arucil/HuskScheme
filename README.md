# HuskScheme
This is a simple Scheme interpreter written in Haskell. The parser and some infrastructures are written from scratch, for fun and practice.

# Run

Build:
```shell
$ stack build
```

Test:
```shell
$ stack test
```

Run:
```shell
$ stack exec HuskScheme
```

# To Do

- [ ] Add support for `eq?` test. Add a `FakePtr` field for reference types.
- [ ] Garbage collection. Reclaim unused cells in the store.
- [ ] Add rationals.