;;;; EXS fixtures and regression checks for the first-species CL port.
;;;; Load after fux.lisp, first-species.lisp, and search.lisp.
;;;; Each case starts with a fresh, unnormalized cantus.
;;;; Reference fixtures are complete output sequences from the original fux.c.
;;;; Default fixtures use exact bounds and :terminal-bound :best-fit.

(in-package :fux)

(defparameter *exs-fixtures*
  '((:number 1 :mode :dorian :start 57
     :cantus (50 53 52 50 55 53 57 55 53 52 50)
     :reference-results
     ((11 (57 57 55 59 59 62 60 64 62 61 62))
      (9 (57 57 60 59 59 62 60 64 62 61 62)))
     :default-results
     ((11 (57 57 55 59 59 62 60 64 62 61 62))
      (9 (57 57 60 59 59 62 60 64 62 61 62))))

    (:number 2 :mode :dorian :start 38
     :cantus (50 53 52 50 55 53 57 55 53 52 50)
     :reference-results
     ((28 (38 38 43 41 40 45 41 40 38 37 38))
      (25 (38 45 43 41 40 38 36 40 38 37 38)))
     :default-results
     ((28 (38 38 43 41 40 45 41 40 38 37 38))
      (27 (38 38 43 41 40 38 36 40 38 37 38))
      (25 (38 45 43 41 40 38 36 40 38 37 38))))

    (:number 3 :mode :phrygian :start 59
     :cantus (52 48 50 48 45 57 55 52 53 52)
     :reference-results
     ((155 (59 57 53 57 60 53 52 55 50 52))
      (71 (59 57 53 57 60 53 52 55 62 64))
      (68 (59 57 53 57 60 53 52 64 62 64))
      (46 (59 57 53 57 60 65 64 67 62 64))
      (35 (59 57 53 57 60 65 64 60 62 64))
      (31 (59 57 53 57 57 60 62 60 62 64))
      (29 (59 57 53 55 57 60 62 60 62 64))
      (20 (59 57 59 60 64 60 62 60 62 64)))
     :default-results
     ((155 (59 57 53 57 60 53 52 55 50 52))
      (71 (59 57 53 57 60 53 52 55 62 64))
      (68 (59 57 53 57 60 53 52 64 62 64))
      (46 (59 57 53 57 60 65 64 67 62 64))
      (35 (59 57 53 57 60 65 64 60 62 64))
      (31 (59 57 53 57 57 60 62 60 62 64))
      (29 (59 57 53 55 57 60 62 60 62 64))
      (20 (59 57 59 60 64 60 62 60 62 64))
      (19 (59 57 59 60 64 60 59 60 62 64))))

    (:number 4 :mode :phrygian :start 40
     :cantus (52 48 50 48 45 57 55 52 53 52)
     :reference-results
     ((40 (40 40 41 45 41 38 40 43 38 40))
      (29 (40 40 41 45 41 38 40 36 38 40))
      (25 (40 40 41 41 41 38 40 36 38 40)))
     :default-results
     ((40 (40 40 41 45 41 38 40 43 38 40))
      (29 (40 40 41 45 41 38 40 36 38 40))
      (25 (40 40 41 41 41 38 40 36 38 40))
      (24 (40 45 43 40 41 38 40 36 38 40))))

    (:number 5 :mode :lydian :start 65
     :cantus (53 55 57 53 50 52 53 60 57 53 55 53)
     :reference-results
     ((17 (65 64 60 62 65 60 62 64 65 62 64 65)))
     :default-results
     ((17 (65 64 60 62 65 60 62 64 65 62 64 65))
      (16 (65 64 60 62 65 60 62 64 64 62 64 65))))

    (:number 6 :mode :lydian :start 41
     :cantus (53 55 57 53 50 52 53 60 57 53 55 53)
     :reference-results
     ((40 (41 40 36 38 41 36 38 40 41 45 40 41))
      (33 (41 40 36 38 41 36 38 40 41 38 40 41))
      (30 (41 40 36 38 41 40 38 40 36 38 40 41))
      (28 (41 40 41 45 41 43 38 40 36 38 40 41)))
     :default-results
     ((40 (41 40 36 38 41 36 38 40 41 45 40 41))
      (33 (41 40 36 38 41 36 38 40 41 38 40 41))
      (30 (41 40 36 38 41 40 38 40 36 38 40 41))
      (28 (41 40 41 45 41 43 38 40 36 38 40 41))))

    (:number 7 :mode :mixolydian :start 55
     :cantus (43 48 47 43 48 52 50 55 52 48 50 47 45 43)
     :reference-results
     ((63 (55 52 55 52 52 48 53 52 55 57 54 55 54 55))
      (41 (55 52 55 52 52 48 47 47 48 45 47 50 54 55))
      (34 (55 52 55 52 52 48 47 47 43 45 47 50 54 55))
      (30 (55 52 55 52 52 55 53 52 55 57 54 55 54 55))
      (26 (55 52 55 59 57 55 59 59 59 57 57 55 54 55))
      (23 (55 52 55 59 57 60 59 59 59 57 57 55 54 55))
      (20 (55 57 50 52 52 55 53 52 55 57 54 55 54 55)))
     :default-results
     ((63 (55 52 55 52 52 48 53 52 55 57 54 55 54 55))
      (41 (55 52 55 52 52 48 47 47 48 45 47 50 54 55))
      (34 (55 52 55 52 52 48 47 47 43 45 47 50 54 55))
      (30 (55 52 55 52 52 55 53 52 55 57 54 55 54 55))
      (29 (55 52 55 52 57 55 53 52 55 57 54 55 54 55))
      (26 (55 52 55 59 57 55 59 59 59 57 57 55 54 55))
      (23 (55 52 55 59 57 60 59 59 59 57 57 55 54 55))
      (20 (55 57 50 52 52 55 53 52 55 57 54 55 54 55))))

    (:number 8 :mode :mixolydian :start 43
     :cantus (43 48 47 43 48 52 50 55 52 48 50 47 45 43)
     :reference-results
     ((41 (43 45 50 47 45 43 47 47 48 52 54 55 54 55))
      (36 (43 45 50 47 45 43 47 47 48 45 47 50 54 55))
      (30 (43 45 50 47 45 43 47 47 43 45 47 50 54 55))
      (28 (43 45 50 47 45 48 47 47 43 45 47 50 54 55))
      (24 (43 45 50 52 52 55 53 52 55 52 54 55 54 55))
      (22 (43 45 50 52 52 55 53 52 55 52 54 51 54 55))
      (18 (43 45 50 52 52 55 53 52 55 57 54 55 54 55)))
     :default-results
     ((41 (43 45 50 47 45 43 47 47 48 52 54 55 54 55))
      (40 (43 45 50 47 45 43 47 47 48 52 54 51 54 55))
      (33 (43 45 50 47 45 43 47 47 48 45 47 50 54 55))
      (30 (43 45 50 47 45 43 47 47 43 45 47 50 54 55))
      (28 (43 45 50 47 45 48 47 47 43 45 47 50 54 55))
      (24 (43 45 50 52 52 55 53 52 55 52 54 55 54 55))
      (22 (43 45 50 52 52 55 53 52 55 52 54 51 54 55))
      (18 (43 45 50 52 52 55 53 52 55 57 54 55 54 55))))

    (:number 9 :mode :aeolian :start 57
     :cantus (45 48 47 50 48 52 53 52 50 48 47 45)
     :reference-results
     ((23 (57 57 55 59 57 55 62 60 59 57 56 57))
      (18 (57 57 55 59 57 55 57 60 59 57 56 57))
      (16 (57 57 62 59 60 60 57 60 59 57 56 57)))
     :default-results
     ((23 (57 57 55 59 57 55 62 60 59 57 56 57))
      (18 (57 57 55 59 57 55 57 60 59 57 56 57))
      (17 (57 57 59 59 60 60 57 60 59 57 56 57))
      (16 (57 57 62 59 60 60 57 60 59 57 56 57))))

    (:number 10 :mode :aeolian :start 45
     :cantus (45 48 47 50 48 52 53 52 50 48 47 45)
     :reference-results
     ((123 (45 45 43 47 45 43 50 48 47 45 44 45))
      (38 (45 45 43 47 45 43 50 48 47 52 56 57))
      (36 (45 45 43 47 45 43 38 36 35 33 32 33))
      (32 (45 45 43 47 45 43 45 48 47 52 56 57))
      (23 (45 45 43 47 45 45 57 55 59 57 56 57)))
     :default-results
     ((123 (45 45 43 47 45 43 50 48 47 45 44 45))
      (38 (45 45 43 47 45 43 50 48 47 52 56 57))
      (36 (45 45 43 47 45 43 38 36 35 33 32 33))
      (32 (45 45 43 47 45 43 45 48 47 52 56 57))
      (23 (45 45 43 47 45 45 57 55 59 57 56 57))))))

(defun exs-fixture (number)
  (or (find number *exs-fixtures*
            :key (lambda (example) (getf example :number)))
      (error "EXS number must be an integer from 1 to 10: ~S" number)))

(defun run-exs (number &key reference (stream t) (crossing-policy :candidate))
  "Run EXS01..10. REFERENCE selects integer bounds and the historical final
bound; NIL selects the CL defaults. STREAM is as in GENERATE-FIRST-SPECIES."
  (let ((example (exs-fixture number)))
    (generate-first-species (getf example :cantus) (getf example :start)
                            :mode (getf example :mode)
                            :stream stream
                            :integer-bounds reference
                            :terminal-bound (if reference
                                                :max-penalty
                                                :best-fit)
                            :crossing-policy crossing-policy)))

(defun score-exs-line (number pitches &key sequential ficta
                                          (crossing-policy :candidate))
  "Test helper: return total, evaluated pitches, and local penalties.
The fixed first note has zero cost. FICTA applies the historical cadence
adjustment first. SEQUENTIAL leaves future counterpoint cells zero, then
calls Check before SetUs, as in the earlier fixed-line diagnostics."
  (let* ((example (exs-fixture number))
         (state (make-first-species-cantus (getf example :cantus)
                                           (first pitches)
                                           (getf example :mode)))
         (base (aref state 0))
         (count (total-notes state 1)))
    (unless (= (length pitches) count)
      (error "EXS~2,'0D needs ~D counterpoint notes" number count))
    (loop for pitch in pitches
          for note from 1
          do (set-pitch state note 1 (- pitch base)))
    (when ficta
      (first-species-cadence-ficta state))
    (let ((evaluated (line-from-state state)))
      (when sequential
        (loop for note from 2 to count
              do (set-pitch state note 1 0)))
      (let ((locals
              (cons 0
                    (loop for pitch in (rest evaluated)
                          for note from 2
                          for candidate = (- pitch base)
                          for cost = (check-first-species state note candidate
                                                          :limit 10000000
                                                          :crossing-policy crossing-policy)
                          collect cost
                          do (set-pitch state note 1 candidate)))))
        (values (reduce #'+ locals) evaluated locals)))))

;;; Independent direct C Check results for the six final lines in EXS03..05.
;;; These also matched C SaveResults before/after and Check-before-SetUs.
(defparameter *exs-rescore-fixtures*
  '((3 :reference-results (0 1 2 2 4 0 2 3 4 2))
    (3 :default-results   (0 1 2 2 4 0 5 2 1 2))
    (4 :reference-results (0 0 1 2 1 4 2 2 2 11))
    (4 :default-results   (0 0 2 2 0 3 2 2 2 11))
    (5 :reference-results (0 0 1 0 2 0 1 1 1 2 2 7))
    (5 :default-results   (0 0 1 0 2 0 1 1 3 2 3 3))))

(defun run-regression-tests ()
  "Check helpers, all complete EXS01..10 output sequences in both policies,
the four EXS02 bound combinations, and independent final-line rescoring."
  (assert (run-basic-tests))
  (assert (run-first-species-tests))
  (assert (run-search-smoke-tests))
  ;; Compare every improvement and every pitch, not only the final score.
  (dolist (example *exs-fixtures*)
    (let ((number (getf example :number)))
      (dolist (reference '(nil t))
        (let ((expected (getf example (if reference
                                         :reference-results
                                         :default-results)))
              (actual (run-exs number :reference reference :stream nil
                                   :crossing-policy :stored)))
          (assert (equal actual expected) ()
                  "EXS~2,'0D, reference=~S mismatch.~%Expected: ~S~%Actual: ~S"
                  number reference expected actual)))))
  ;; In EXS02 only the conjunction of historical terminal guard and integer
  ;; bounds removes the 27-point intermediate result. Neither alone does.
  (let ((example (exs-fixture 2)))
    (dolist (terminal-bound '(:best-fit :max-penalty))
      (dolist (integer-bounds '(nil t))
        (let ((expected
                (getf example (if (and (eq terminal-bound :max-penalty)
                                       integer-bounds)
                                  :reference-results
                                  :default-results)))
              (actual
                (generate-first-species (getf example :cantus)
                                        (getf example :start)
                                        :mode (getf example :mode)
                                        :terminal-bound terminal-bound
                                        :integer-bounds integer-bounds
                                        :crossing-policy :stored)))
          (assert (equal actual expected) ()
                  "EXS02, terminal-bound=~S, integer-bounds=~S mismatch.~%Expected: ~S~%Actual: ~S"
                  terminal-bound integer-bounds expected actual)))))
  ;; Verify that printed final lines really have their reported costs, with
  ;; the current note installed or unset, and after cadence adjustment.
  (dolist (fixture *exs-rescore-fixtures*)
    (destructuring-bind (number result-key expected-locals) fixture
      (destructuring-bind (expected-score expected-pitches)
          (car (last (getf (exs-fixture number) result-key)))
        (dolist (sequential '(nil t))
          (dolist (ficta '(nil t))
            (multiple-value-bind (score pitches locals)
                (score-exs-line number expected-pitches
                                :sequential sequential :ficta ficta
                                :crossing-policy :stored)
              (assert (and (= score expected-score)
                           (equal pitches expected-pitches)
                           (equal locals expected-locals)) ()
                      "EXS~2,'0D ~S rescore mismatch (sequential=~S, ficta=~S).~%Expected score/line/locals: ~S ~S ~S~%Actual: ~S ~S ~S"
                      number result-key sequential ficta
                      expected-score expected-pitches expected-locals
                      score pitches locals)))))))
  (assert (run-crossing-candidate-tests))
  (format t "~&Fux: basic tests, 20 complete EXS sequences, 4 EXS02 bound cases, and 6 line rescoring/ficta checks passed.~%")
  t)

(defun run-crossing-candidate-tests ()
  "The same candidate and selected prefix must score identically regardless
of the stale pitch in its current work cell (first species, two voices)."
  (let ((state (make-fux-state :mode :dorian)))
    (set-total-notes state 0 6)
    (set-total-notes state 1 6)
    (loop for pitch in '(48 52 58 53 57 48)
          for note from 1
          do (set-pitch state note 0 pitch)
             (set-onset state note 0 (* 8 (1- note)))
             (set-duration state note 0 8)
             (set-onset state note 1 (* 8 (1- note)))
             (set-duration state note 1 8))
    (loop for pitch in '(60 60 55 60 53)
          for note from 1
          do (set-pitch state note 1 pitch))
    (let ((scores
            (loop for stale in '(0 42 55 60 67)
                  collect (progn
                            (set-pitch state 6 1 stale)
                            (check-first-species state 6 60
                                                 :limit 10000000
                                                 :crossing-policy :candidate)))))
      (assert (every (lambda (x) (= x (first scores))) scores) ()
              "Candidate crossing depends on stale work cell: ~S" scores)))
  t)
