# Historical reference notes

The implementation follows Schottstaedt's *Automatic Species Counterpoint*
and the later chapter presentation, using the archived C translation as an
executable reference. The public `fux.lisp` contains the rule and search
implementation; all C result lists and historical analysis are kept here.

- `:reference t` selects the tested C-like search configuration (integer
  bounds, maximum-penalty final bound and stored-pitch crossing checks).
- The source-specific test files compare complete improvement sequences,
  pitch lines, rhythms and Figure 16.8's individual costs on their specified
  inputs. Passing fixtures do not establish equivalence for every input.
- `fux:run-figure-16-8-c-audit` checked 89/89 local CL/C costs in the
  transcribed printed score; original C agrees with 80/89 printed costs.
  The known score/notation discrepancies remain open historical questions.
  `python3 reference/verify-figure-c.py` independently regenerates those
  89 original-C note costs from the transcribed fixture in `evidence/`.
- C `SaveResults` can raise notes approaching a raised cadential leading
  tone after computing the search score. The score belongs to the candidate
  before this adjustment. Bill distinguishes this from general musica
  ficta, which his program does not attempt to solve.
- A positive six-staff C/CL fixture is in `tests/six-voice-positive-test.lisp`.

The uploaded snapshot with the earlier detailed investigations and
benchmarks is retained in `history/`. The two PDFs are deliberately not
included in this package: the locally available report is 3.1 MB and the
chapter scan is 67 MB. They are source readings rather than dependencies.
Keep your own copies in `reference/papers/` if you want an offline archive:

- Bill Schottstaedt, *Automatic Species Counterpoint*, 1984, 51-page PDF
  (`Schottstaedt_Automatic-Counterpoint.pdf`).
- William Schottstaedt, “Automatic Counterpoint,” chapter 16, pp. 199–214,
  in Max V. Mathews and John R. Pierce (eds.), *Current Directions in
  Computer Music Research*, MIT Press, 1989. The 16-page scan was supplied
  under the misleading filename `Schottstaedt - A Compouter Music Language -
  scan 2026-11-10.pdf`; the chapter itself is “Automatic Counterpoint.”

The exact C source used by the fixtures **is** included as `reference/fux.c`.
When discussing a discrepancy, cite the particular C fragment or page and
include a small reproducible example. We have not attached unverified
third-party links to the two scans.
