---
title: OCaml
entangled:
  version: "2.4"
  style: "default"
---

Here is "Hello World" in OCaml:

``` {.ocaml file=src/ocaml/hello.ml}
let () = print_endline "Hello, World!"
```

Most OCaml tutorials let you build this with `opam`, but you can use the OCaml compiler directly:

```bash
ocamlopt src/ocaml/hello.ml -o hello
./hello
```

OCaml is mostly a bog standard functional language with nothing setting it particullarly apart from other languages:

- Performs tail-call elimination.
- Is strongly typed.
- Supports mutability through `ref` bindings.
- Is compiled to bytecode using `ocamlc` or native using `ocamlopt`.
- Eager evaluation.
- An interesting module system.
- A system of typed effects.

## Modules

``` {.ocaml file=src/ocaml/athens.ml}
let hello () = print_endline "Hello from Athens"
```

``` {.ocaml file=src/ocaml/berlin.ml}
let () = Athens.hello ()
```
