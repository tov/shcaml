Caml-Shcaml 0.2.0
-----------------

- Drop the dependency on camlp4, mainly by replacing the static typing of
  structured fields by a lighter, dynamically-checked discipline. This trades
  static guarantees for maintenability.

- Build system changes, switch to ocamlbuild+topkg

- Some API refactoring; improve API browsing at the price of some signature
  duplication in the source.

- Documentation improvements

Caml-Shcaml 0.1.2
-----------------
- [23 Sep 2008] We no longer distinguish input and output dups.  This
  change is because the old way we did it no longer types under 3.10.2.
  There is probably a better way.

- [19 Sep 2008] Added DepDAG module for running dependency DAGs in
  parallel.

- [19 Sep 2008] Added Fitting.run_list as a convenient fitting runner.

Caml-Shcaml 0.1.1
-----------------

- [05 Feb 2008] Sed script used to build documentation is now portable;
  no longer relies on GNU sed.

- [05 Feb 2008] Now supports (and in fact requires) OCaml 3.10.  (Should
  we somehow make it work in both?  Seems like a pain.)

- [07 Aug 2007, 05 Feb 2008] pa_linetype now supports anonymous row
  variables, such as <| .. >.  When the entire row is anonymous, it
  expands to < .. > rather than a list of fields.

- [15 Aug 2007] Several places that used to use ints for process statuses
  now use Proc.status instead.

Caml-Shcaml 0.1.0
-----------------

- [06 Aug 2007] Initial release
