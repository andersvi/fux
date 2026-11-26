# FUX for Common Lisp

One file containing a working port of Bill Schottstaedt's species counterpoint
generator. The source of this file is maintained as smaller modules; this
distribution is generated from those modules.

```lisp
(load "fux.lisp")
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :starts '(57) :species 5
                            :patterns '(3 4 7 9) :with-rhythm t))
```

`CANTUS` and `:STARTS` are integer pitches in Bill's low-C numbering. One to
five start pitches create one to five generated voices. `:species` (1–5)
applies to the last generated voice. Previous generated voices follow
first-species rules, even if `:inner-half-notes t` gives some of them a
half-note rhythm. The generator returns improving solutions; use
`fux:best-counterpoint` for the last one. `:with-rhythm t` attaches onsets,
durations and ties. `:with-search-costs t` records per-note search costs for
three to five generated voices, or one generated fifth-species voice.
`:patterns` for 5th species are indices into the following table of rhythms:

(defparameter *fifth-rhythm-patterns*
  #(()
    (4 4)
	(4 2 2)
	(2 2 2 2)
	(2 2 4)
	(2 1 1 4)
	(2 1 1 2 2)
	(4 2 1 1)
	(2 1 1 2 1 1)
	(2 2 2 1 1)))

Interactive example forms are in `examples/examples.lisp`. 

`fux:write-counterpoint-cmn` writes a CMN form as text. The output can
be evaluated separately with CMN
(https://ccrma.stanford.edu/software/cmn/).  FUX itself does not load
CMN.  Load `examples/scratch-cmn.lisp` for the optional CMN macro,
then evaluate selected example forms interactively.

The search is bounded and its numerical weights reflect this program's
rules; a returned line is not a proof of an optimal or generally Fux-correct
composition. For C comparisons and historical caveats, see the companion
development/source package.

## Sources

- Bill Schottstaedt, *Automatic Species Counterpoint*, CCRMA technical report
  STAN-M-19, Stanford University, May 1984 (report and SAIL listing).
- William Schottstaedt, “Automatic Counterpoint,” in Max V. Mathews and
  John R. Pierce (eds.), [*Current Directions in Computer Music Research*](https://mitpress.mit.edu/9780262132411/current-directions-in-computer-music-research/),
  MIT Press, 1989, pp. 199–214 (chapter 16).

The development package also contains the C translation used as an
executable comparison source.
