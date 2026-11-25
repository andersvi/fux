# Applying this tree to the existing Git repository

The uploaded ZIP had no `.git` directory. The files in this package were
prepared as a separate tree; no branch or commit was made in your repository.

1. Start a branch in your repository (for example `cleanup/single-file`) and
   ensure the working tree is clean.
2. Copy this development tree into that branch. `src/` replaces the old
   top-level implementation files, and `tests/` replaces the old root test
   files. Keep existing local changes if they are newer than the upload.
3. Load `load-reference.lisp` in a fresh LispWorks image and run
   `(fux:run-cleanup-regression-tests :basic)` and the other groups listed
   in README.md. This checks the modular source without one giant call.
4. In another fresh LispWorks image load `dist/fux.lisp`, then
   `load-tests-only.lisp`, and run the same groups. This checks the generated
   single file against the same C fixtures.
5. The public `dist/fux-public.zip` contains one code file. Unpack it into
   an empty directory and load `fux.lisp` there for a final smoke check.

`tools/build.py` lists all 24 implementation modules in their load order.
`load-dev.lisp` uses the same order. The historical C source is unchanged;
the earlier generated CMN, PDF and EPS artifacts and old step documents can
be recovered from `history/original-upload-2026-11-24.zip`. The public ZIP
contains none of these files.

The source move removed 52 embedded test and fixture definitions from the
implementation modules. They are preserved, unchanged, in
`tests/embedded-tests.lisp`. The only executable code adjustment replaces
two search-cost `assert` forms with equivalent explicit errors, so the
standalone generator contains no test `assert`. The test-specific exports
were moved to the tests loader.

The C/CL and LispWorks checks in steps 3–5 have to run on your machine;
the authoring environment has no Common Lisp implementation installed.
