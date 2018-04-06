# Shcaml

Shcaml is an OCaml library for Unix shell programming.

Unix shells provide easy access to Unix functionality such as pipes, signals,
file descriptor manipulation, and the file system. Shcaml hopes to excel at
these same tasks.

## Installation

Shcaml can be installed with `opam`:

    opam install shcaml

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation & tutorial

You can find Shcaml's documentation (including a tutorial) online at
http://tov.github.io/shcaml/doc or build a local copy with `make
doc`.

See also the [companion
paper](http://users.eecs.northwestern.edu/%7Ejesse/pubs/caml-shcaml/). Note than
as of Shcaml 0.2.0 and onwards, the solution to encode row types described in
section 4.3 has been replaced by a more lightweight policy, where the absence or
presence of fields is only checked dynamically. This trades static guarantees
for maintainability of the library.
