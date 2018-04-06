# HuskScheme
This is a simple Scheme interpreter written in Haskell. The parser and some infrastructures are written from scratch, for fun and practice.

# Features

- [x] Built-in Special Forms (`Quote`, `If`, `Lambda`, `Let`, `Set!`, `Define`, `Begin`)
- [x] First-class Procedures
- [x] Rationals
- [x] Support for pointer-wise equivalence test (`eq?`)
- [ ] More primitive procedures
- [ ] Support for Template Haskell
- [ ] First-class Continuations
- [ ] Quasiquotation
- [x] Macros (`defmacro`)
- [ ] Eval
- [x] `Load` Special Form
- [ ] Module System (Import)

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
