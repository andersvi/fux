## Download

- [fux.lisp](https://github.com/andersvi/fux/releases/latest/download/fux.lisp)
- [Complete distribution (ZIP)](https://github.com/andersvi/fux/releases/latest/download/fux-public.zip)
- [Musical examples – scores and audio](https://github.com/andersvi/fux/releases/tag/examples)


# Small interactive examples

Run these after `(load (compile-file "fux.lisp"))` or after loading `load-dev.lisp`.

```lisp
;; One generated florid voice; explicit rhythm patterns for four bars.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :starts '(57) :species 5
                            :patterns '(3 4 7 9)
                            :with-rhythm t :with-search-costs t))

;; Lydian, six voices in all; the preceding inner voices have half notes.
(defparameter *lydian-result*
  (fux:best-counterpoint
   (fux:generate-counterpoint '(53 55 57 59 53 52 50 52 53)
                              :starts '(41 48 57 60 65)
                              :mode :lydian :species 5
                              :patterns '(2 3 4 5 6 7 8 9)
                              :inner-half-notes t :reference t
                              :with-rhythm t :with-search-costs t)))

(with-open-file (out "lydian.cmn" :direction :output
                          :if-exists :supersede)
  (fux:write-counterpoint-cmn *lydian-result* :stream out
                              :mode :lydian :score-notes :search
                              :staff-order '(5 4 3 0 2 1)))
```

Load `scratch-cmn.lisp` after FUX to define the optional CMN macro.
Evaluate selected forms from `examples.lisp` interactively; do not load
the entire file. Examples are not embedded in the stand-alone build.

