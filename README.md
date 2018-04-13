# HuskScheme
This is a simple Scheme interpreter written in Haskell. The parser and some infrastructures are written from scratch just for fun.
**NB. More progress is being made on branch `cps`. Please check out that branch for recent changes.**  

# Features

- [x] Built-in Special Forms (`Quote`, `If`, `Lambda`, `Let`, `Set!`, `Define`, `Begin`)
- [x] First-class Procedures
- [x] Rationals
- [ ] Support for reference equality test (`eq?`)
- [ ] More primitive procedures
- [ ] Support for Template Haskell
- [x] First-class Continuations (on branch `cps`)
- [x] Quasiquotation
- [x] Macros (`defmacro`)
- [ ] `Eval` function
- [x] `Load` Special Form
- [ ] Simple Module System (Import)

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
