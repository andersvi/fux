(in-package :fux)

(defun run-basic-tests ()
  "Sanity checks for the transcribed primitives (NOT the 11 exs tests)."
  (assert (in-mode 50 :dorian))
  (assert (not (in-mode 49 :dorian)))
  (assert (bad-melody-p -8))
  (assert (not (bad-melody-p 8)))
  (assert (eq :direct (motion-type 40 42 50 55)))
  (assert (eq :contrary (motion-type 40 42 50 48)))
  (assert (eq :oblique (motion-type 40 40 50 48)))
  (assert (eq :none (motion-type 40 40 50 50)))
  (assert (direct-motion-to-perfect-consonance-p 40 41 47 48))
  (assert (consecutive-skips-in-same-direction-p 40 43 47))
  (assert (not (consecutive-skips-in-same-direction-p 40 42 45)))
  (assert (out-of-range-p 73))
  (assert (not (out-of-range-p 72)))
  (let ((state (make-fux-state)))
    (set-total-notes state 0 3)
    (set-total-notes state 1 3)
    (set-pitch state 1 0 50)
    (set-pitch state 2 0 52)
    (set-pitch state 1 1 38)
    (set-onset state 1 1 0)
    (set-onset state 2 1 8)
    (set-duration state 1 1 8)
    (assert (= (cantus state 2 1) 52))
    (assert (= (pitch-repeats state 2 38 1) 1))
    (assert (= (total-range state 2 45 1) 7))
    (assert (dissonance-p state 1 2 40 1 1)))
  t)

(defun run-first-species-tests ()
  "Small independent tests for C Check, not yet exs regression tests."
  (let ((state (make-fux-state :mode :dorian)))
    (set-total-notes state 0 3)
    (set-total-notes state 1 3)
    (loop for p in '(50 52 50) for i from 1
          do (set-pitch state i 0 p)
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8)
             (set-onset state i 1 (* 8 (1- i)))
             (set-duration state i 1 8))
    (set-pitch state 1 1 38)
    (assert (integerp (check-first-species state 2 40 :limit 1000000)))
    (assert (>= (check-first-species state 2 40 :limit 1000000) 0)))
  t)

(defparameter *exs01-cantus* '(50 53 52 50 55 53 57 55 53 52 50))

(defun run-exs01 ()
  (generate-first-species *exs01-cantus* 57 :mode :dorian :stream t))

(defun run-exs02 ()
  (generate-first-species *exs01-cantus* 38 :mode :dorian :stream t))

(defun run-search-smoke-tests ()
  (let ((state (make-first-species-cantus *exs01-cantus* 57 :dorian)))
    (assert (= (total-notes state 1) 11))
    (assert (= (pitch-at state 1 1) 55))
    (assert (= (cantus state 2 1) 51))
    (assert (= (onset-at state 11 1) 80))
    (assert (= (duration-at state 11 1) 8)))
  t)

(defun compare-exs02-bounds ()
  (format t "~&=== CL: exact rational bounds ===~%")
  (let ((full (generate-first-species *exs01-cantus* 38
                                      :mode :dorian :stream t)))
    (format t "~&=== CL: C-style integer bounds ===~%")
    (let ((integer (generate-first-species *exs01-cantus* 38
                                            :mode :dorian :stream t
                                            :integer-bounds t)))
      (values full integer))))

(defun compare-exs02-policies ()
  "Return the four (terminal-policy integer-bounds results) combinations."
  (loop for terminal-bound in '(:best-fit :max-penalty)
        append (loop for integer-bounds in '(nil t)
                     collect
                     (progn
                       (format t "~&=== terminal ~S, integer bounds ~S ===~%"
                               terminal-bound integer-bounds)
                       (list terminal-bound integer-bounds
                             (generate-first-species
                              *exs01-cantus* 38 :mode :dorian :stream t
                              :terminal-bound terminal-bound
                              :integer-bounds integer-bounds))))))

(defun run-exs11 (&key (stream t) (reference nil))
  (generate-poly-first-species *exs01-cantus* '(57 62)
                               :mode :dorian :stream stream
                               :terminal-bound (if reference :max-penalty :best-fit)
                               :integer-bounds reference))

(defparameter *exs11-reference*
  '((50 (57 57 55 53 52 53 53 52 50 55 57)
        (62 62 60 62 60 57 62 61 62 61 62))
    (48 (57 57 55 53 52 53 53 52 50 49 50)
        (62 62 60 62 60 57 62 61 62 64 57))
    (46 (57 57 55 53 48 50 50 52 53 55 57)
        (62 62 60 62 64 62 65 60 62 61 62))
    (42 (57 57 55 57 59 60 60 59 62 61 62)
        (62 62 60 65 64 69 65 64 65 67 69))))

(defun test-exs11 ()
  "Exact C result comparison; returns T or signals an assertion failure."
  (let ((result (run-exs11 :stream nil :reference t)))
    (assert (equal result *exs11-reference*)
            (result)
            "EXS11 differs from C; compare scores and voice sequences.~%Got: ~S"
            result)
    t))

(defun compare-exs11 ()
  "Show any differences between historical C records and the new CL port."
  (let ((actual (run-exs11 :stream nil :reference t)))
    (format t "~&EXS11 C expected: ~D records~%CL obtained:      ~D records~%"
            (length *exs11-reference*) (length actual))
    (loop for expected in *exs11-reference*
          for observed in actual
          for i from 1
          unless (equal expected observed)
            do (format t "~&Record ~D~%  C:  ~S~%  CL: ~S~%"
                       i expected observed))
    (if (equal actual *exs11-reference*)
        (format t "~&EXS11 exact match.~%")
        (format t "~&EXS11 differs; retain output for diagnosis.~%"))
    actual))

(defun run-second-species-tests ()
  "Initial second-species tests: rhythm, passing dissonance and search shape."
  (let* ((state (make-second-species-cantus '(50 53 52 50 55) 57 :dorian))
         (n (total-notes state 1)))
    (assert (= n 9))
    (assert (= (total-notes state 0) 5))
    (assert (= (onset-at state n 1) 32))
    (assert (= (duration-at state n 1) 8))
    (assert (= (onset-at state 2 1) 4))
    (assert (upbeat-p state 2 1))
    (assert (downbeat-p state 3 1))
    (assert (= (cantus state 2 1) (pitch-at state 1 0)))
    ;; ADissonance: second-species dissonance on weak beat may pass by step.
    ;; At note 2, the candidate 56 is one semitone above the start 55.
    ;; A dissonant vertical interval is allowed only on the weak beat.
    (set-pitch state 1 1 55)
    (assert (not (dissonance-p state 1 2 56 1 2)))
    (assert (dissonance-p state 1 3 56 1 2))
    (assert (integerp (check-second-species state 2 56)))
    t))

(defparameter *species2-short-cantus* '(50 53 55 52 50))

(defparameter *species2-c-small-reference*
  '((243 (57 59 57 62 59 64 59 61 62))
    (61 (57 59 57 62 59 52 59 61 62))
    (29 (57 59 57 62 64 59 59 61 62))))

(defparameter *species2-c-full-reference*
  '((50 (57 59 57 62 60 67 65 64 62 64 65 62 65 64 64 62 62 57 59 61 62))
    (47 (57 59 57 62 60 67 65 64 62 64 65 62 65 60 62 64 62 57 59 61 62))
    (45 (57 59 57 62 60 67 65 64 62 64 65 62 60 62 64 59 62 57 59 61 62))
    (41 (57 59 57 62 60 67 65 64 62 64 69 67 65 60 62 64 62 57 59 61 62))))

(defun run-species2-small (&key (stream t) (reference nil))
  (generate-second-species *species2-short-cantus* 57
                           :stream stream :mode :dorian
                           :crossing-policy (if reference :stored :candidate)
                           :terminal-bound (if reference :max-penalty :best-fit)
                           :integer-bounds reference))

(defun run-species2-full (&key (stream t) (reference nil))
  (generate-second-species *exs01-cantus* 57
                           :stream stream :mode :dorian
                           :crossing-policy (if reference :stored :candidate)
                           :terminal-bound (if reference :max-penalty :best-fit)
                           :integer-bounds reference))

(defun compare-species2 (&key (full nil))
  "Print an informative C/CL comparison; no assertion until verified."
  (let* ((expected (if full *species2-c-full-reference*
                       *species2-c-small-reference*))
         (actual (if full (run-species2-full :stream nil :reference t)
                     (run-species2-small :stream nil :reference t))))
    (format t "~&Species 2 (~A): C ~D solutions, CL ~D solutions~%"
            (if full "full" "short") (length expected) (length actual))
    (format t "Match: ~A~%" (if (equal expected actual) "YES" "NO"))
    (unless (equal expected actual)
      (format t "C:  ~S~%CL: ~S~%" expected actual))
    actual))

(defun run-third-species-tests ()
  (let* ((state (make-third-species-cantus '(50 53 55 52 50) 57 :dorian))
         (n (total-notes state 1)))
    (assert (= n 17))
    (assert (= (total-notes state 0) 5))
    (assert (= (onset-at state 2 1) 2))
    (assert (= (onset-at state 5 1) 8))
    (assert (= (onset-at state n 1) 32))
    (assert (= (duration-at state n 1) 8))
    (assert (= (duration-at state (1- n) 1) 2))
    (assert (downbeat-p state 5 1))
    (assert (not (downbeat-p state 4 1)))
    (assert (integerp (check-third-species state 2 59)))
    t))

(defparameter *species3-short-cantus* '(50 53 55 52 50))

(defparameter *species3-c-small-reference*
  '((43 (57 59 60 62 57 59 60 57 59 52 59 57 55 57 59 61 62))
    (36 (57 59 60 62 57 59 60 57 59 64 62 64 60 64 62 61 62))
    (28 (57 55 53 52 50 45 50 48 47 52 59 57 55 57 59 61 62))
    (24 (57 55 53 52 50 45 50 48 47 48 50 52 55 57 59 61 62))))

(defparameter *species3-c-full-reference*
  '((140 (57 59 60 62 57 59 60 57 60 59 57 55 59 60 62 59 64 62 64 59 62 64 65 62 65 64 65 60 62 64 65 67 69 67 65 69 67 60 59 61 62))
    (127 (57 59 60 62 57 59 60 57 60 59 57 55 59 60 62 59 64 62 64 59 62 64 65 62 65 64 65 60 62 64 65 67 69 67 65 69 67 64 62 61 62))
    (114 (57 59 60 62 57 59 60 57 60 59 57 55 59 60 62 59 64 62 64 59 62 64 65 62 65 64 65 60 62 64 65 67 69 67 65 62 60 64 62 61 62))))

(defun run-species3-small (&key (stream t) (reference nil))
  (generate-third-species *species3-short-cantus* 57
                          :stream stream :mode :dorian
                          :crossing-policy (if reference :stored :candidate)
                          :terminal-bound (if reference :max-penalty :best-fit)
                          :integer-bounds reference))

(defun run-species3-full (&key (stream t) (reference nil))
  (generate-third-species *exs01-cantus* 57
                          :stream stream :mode :dorian
                          :crossing-policy (if reference :stored :candidate)
                          :terminal-bound (if reference :max-penalty :best-fit)
                          :integer-bounds reference))

(defun compare-species3 (&key (full nil))
  (let* ((expected (if full *species3-c-full-reference*
                       *species3-c-small-reference*))
         (actual (if full (run-species3-full :stream nil :reference t)
                     (run-species3-small :stream nil :reference t))))
    (format t "~&Species 3 (~A): C ~D solutions, CL ~D solutions~%"
            (if full "full" "short") (length expected) (length actual))
    (format t "Match: ~A~%" (if (equal expected actual) "YES" "NO"))
    (unless (equal expected actual)
      (format t "C: ~S~%CL: ~S~%" expected actual))
    actual))

(defun run-fourth-species-tests ()
  "Basic fourth-species rhythm, suspension and dissonance checks."
  (let* ((state (make-fourth-species-cantus '(50 53 55 52 50)
                                            57 :dorian))
         (n (total-notes state 1)))
    (assert (= n 9))
    (assert (= (onset-at state 2 1) 4))
    (assert (= (onset-at state 3 1) 8))
    (assert (= (onset-at state n 1) 32))
    (assert (= (duration-at state n 1) 8))
    (assert (upbeat-p state 2 1))
    (assert (downbeat-p state 3 1))
    ;; A dissonance is allowed on the downbeat only if tied from note 2;
    ;; on the upbeat it is forbidden even with the same candidate pitch.
    (set-pitch state 2 1 53)
    (assert (not (dissonance-p state 2 3 53 1 4)))
    (assert (dissonance-p state 2 3 49 1 4))
    (assert (dissonance-p state 2 4 53 1 4))
    (set-pitch state 3 1 53)
    (assert (equal '((2 . 3))
                   (fourth-species-ties '(57 59 59 57 55 54 53 52 50))))
    (assert (integerp (check-fourth-species state 4 52)))
    t))

(defparameter *species4-short-cantus* '(50 53 55 52 50))

(defparameter *species4-c-small-reference*
  '((167 (57 59 59 57 57 55 59 61 62))
    (57 (57 59 59 57 59 62 62 61 62))))

(defparameter *species4-c-full-reference*
  '((160 (57 59 59 57 57 55 55 53 53 52 52 50 50 48 47 52 52 50 50 49 50))
    (120 (57 59 59 57 57 55 55 53 53 52 52 50 50 48 47 52 50 57 59 61 62))
    (105 (57 59 59 57 57 55 55 53 53 52 52 50 50 48 47 52 50 62 62 61 62))
    (97  (57 59 59 57 57 55 55 53 53 52 52 50 50 48 47 59 59 57 59 61 62))
    (89  (57 59 59 57 57 55 55 53 53 52 52 50 50 53 53 52 50 62 62 61 62))
    (71  (57 59 59 57 57 55 55 53 53 52 52 50 50 53 52 64 64 62 62 61 62))))

(defun run-species4-small (&key (stream t) (reference nil))
  (generate-fourth-species *species4-short-cantus* 57
                           :stream stream :mode :dorian
                           :crossing-policy (if reference :stored :candidate)
                           :terminal-bound (if reference :max-penalty :best-fit)
                           :integer-bounds reference))

(defun run-species4-full (&key (stream t) (reference nil))
  (generate-fourth-species *exs01-cantus* 57
                           :stream stream :mode :dorian
                           :crossing-policy (if reference :stored :candidate)
                           :terminal-bound (if reference :max-penalty :best-fit)
                           :integer-bounds reference))

(defun compare-species4 (&key (full nil))
  (let* ((expected (if full *species4-c-full-reference*
                       *species4-c-small-reference*))
         (actual (if full (run-species4-full :stream nil :reference t)
                     (run-species4-small :stream nil :reference t))))
    (format t "~&Species 4 (~A): C ~D solutions, CL ~D solutions~%"
            (if full "full" "short") (length expected) (length actual))
    (format t "Match: ~A~%" (if (equal expected actual) "YES" "NO"))
    (unless (equal expected actual)
      (format t "C: ~S~%CL: ~S~%" expected actual))
    actual))

(defun run-fifth-species-tests ()
  "Rhythm, pattern and species-specific dissonance smoke tests."
  (let* ((expected '(9 4 7 8 9 4 8 3 9 3))
         (patterns (fifth-rhythm-pattern-indices 10 :seed 1))
         (state (make-fifth-species-cantus
                 '(50 53 55 52 50) 57 :dorian))
         (count (total-notes state 1)))
    (assert (equal patterns expected))
    (assert (= count 19))
    (assert (= (onset-at state 2 1) 2))
    (assert (= (onset-at state 5 1) 7))
    (assert (= (onset-at state 6 1) 8))
    (assert (= (onset-at state count 1) 32))
    (assert (= (duration-at state count 1) 8))
    ;; Weak-beat passing dissonance can be approached by step.
    (assert (not (dissonance-p state 5 2 53 1 5)))
    ;; A downbeat dissonance must be tied from the preceding note.
    (set-pitch state 5 1 53)
    (assert (not (dissonance-p state 2 6 53 1 5)))
    (assert (dissonance-p state 2 6 49 1 5))
    (assert (integerp (check-fifth-species state 2 53)))
    t))

(defparameter *species5-c-short-reference*
  '(
  (389 (57 59 60 62 59 57 55 53 52 48 52 50 48 55 53 52 59 61 62))
  (258 (57 59 60 62 59 57 55 53 52 48 52 50 48 60 59 61 59 61 62))
  (167 (57 59 60 62 59 57 62 60 60 59 64 62 60 67 65 64 59 61 62))
  (149 (57 59 60 62 59 57 62 60 60 59 64 62 60 59 57 55 59 61 62))
  (126 (57 59 57 53 55 57 50 57 59 52 59 57 55 60 59 61 59 61 62))
  (84 (57 59 57 53 55 57 50 57 59 64 59 57 55 59 57 55 59 61 62))
  (69 (57 59 57 53 55 57 50 57 59 64 59 57 55 60 59 61 59 61 62))
  (59 (57 59 57 53 55 57 50 48 47 52 59 57 55 60 59 61 59 61 62))
  (47 (57 59 57 53 55 57 62 60 60 59 64 62 60 59 57 55 59 61 62))
  ))

(defparameter *species5-c-medium-reference*
  '(
  (350 (57 59 60 62 59 57 55 53 55 48 55 57 59 57 55 53 65 64 62 60 59 62 64 62 57 61 62))
  (241 (57 59 60 62 59 57 62 60 60 55 59 60 59 57 55 53 65 64 62 64 52 59 60 62 57 61 62))
  (219 (57 59 60 62 59 57 62 60 60 55 59 60 59 57 55 53 65 64 62 64 62 59 60 62 57 61 62))
  (131 (57 59 57 53 55 57 50 57 55 59 55 57 59 57 55 53 65 64 62 64 62 59 60 62 57 61 62))
  (112 (57 59 57 53 55 57 50 57 55 59 55 57 59 57 59 60 62 60 59 64 62 59 60 62 57 61 62))
  ))

(defparameter *species5-c-full-reference*
  '(
  (805 (57 55 53 57 55 53 52 50 50 48 55 57 59 57 55 53 57 59 52 50 47 48 47 45 43 41 41 48 47 45 48 50 52 53 55 52 52 50 52 53 50 48 45 47 49 50))
  (757 (57 55 53 57 55 53 52 50 50 48 55 57 59 57 55 53 57 59 52 50 47 48 47 45 43 41 41 48 47 45 48 50 52 53 55 52 52 50 57 50 45 43 45 47 49 50))
  (648 (57 55 53 57 55 53 52 50 50 48 55 57 59 57 55 53 57 59 52 50 47 48 47 45 43 41 41 48 47 45 48 50 52 53 55 52 52 50 57 45 47 48 45 47 49 50))
  (489 (57 53 55 57 55 53 52 50 50 48 55 57 59 57 55 53 57 59 52 50 47 48 47 45 43 41 41 48 47 45 48 50 52 50 47 48 50 48 47 45 47 48 45 47 49 50))
  (423 (57 62 60 59 60 62 64 65 67 60 67 69 71 69 67 65 69 71 64 65 67 64 62 62 64 65 65 60 62 64 60 62 64 59 60 62 62 57 59 60 62 60 64 62 61 62))
  (335 (57 62 60 59 60 62 64 65 67 60 67 69 71 69 67 65 69 71 64 65 67 62 64 65 62 64 65 60 62 64 60 62 64 59 60 62 62 57 59 60 62 60 64 62 61 62))
  (277 (57 50 52 53 55 57 50 57 55 59 55 57 59 57 55 53 65 64 62 60 59 62 64 62 69 67 65 60 62 64 60 62 64 65 67 64 64 62 60 57 59 60 64 62 61 62))
  ))

(defparameter *species5-short-cantus* '(50 53 55 52 50))

(defparameter *species5-medium-cantus* '(50 53 52 50 55 53 50))

(defun run-species5-small (&key (stream t) reference (seed 1) patterns)
  (generate-fifth-species *species5-short-cantus* 57
                          :stream stream :seed seed :patterns patterns
                          :crossing-policy (if reference :stored :candidate)
                          :terminal-bound (if reference :max-penalty :best-fit)
                          :integer-bounds reference))

(defun run-species5-medium (&key (stream t) reference (seed 1) patterns)
  (generate-fifth-species *species5-medium-cantus* 57
                          :stream stream :seed seed :patterns patterns
                          :crossing-policy (if reference :stored :candidate)
                          :terminal-bound (if reference :max-penalty :best-fit)
                          :integer-bounds reference))

(defun run-species5-full (&key (stream t) reference (seed 1) patterns)
  (generate-fifth-species *exs01-cantus* 57
                          :stream stream :seed seed :patterns patterns
                          :crossing-policy (if reference :stored :candidate)
                          :terminal-bound (if reference :max-penalty :best-fit)
                          :integer-bounds reference))

(defun compare-species5 (&key (case :short))
  "Compare the complete C improvement sequence for :SHORT, :MEDIUM or :FULL."
  (let* ((expected (ecase case
                     (:short *species5-c-short-reference*)
                     (:medium *species5-c-medium-reference*)
                     (:full *species5-c-full-reference*)))
         (actual (ecase case
                   (:short (run-species5-small :stream nil :reference t))
                   (:medium (run-species5-medium :stream nil :reference t))
                   (:full (run-species5-full :stream nil :reference t)))))
    (format t "~&Species 5 (~A): C ~D solutions, CL ~D solutions~%"
            case (length expected) (length actual))
    (format t "Match: ~A~%" (if (equal expected actual) "YES" "NO"))
    (unless (equal expected actual)
      (format t "C: ~S~%CL: ~S~%" expected actual))
    actual))

(defparameter *mixed-species2-short-cantus* '(50 53 55 52 50))

(defparameter *mixed-species2-medium-cantus* '(50 53 52 50 55 53 50))

(defun run-mixed-species2 (&key (case :short) (reference nil) (stream t))
  (let ((cantus (ecase case
                  (:short *mixed-species2-short-cantus*)
                  (:medium *mixed-species2-medium-cantus*))))
    (generate-mixed-second-species
     cantus '(57 62) :mode :dorian :stream stream
     :integer-bounds reference
     :terminal-bound (if reference :max-penalty :best-fit)
     :crossing-policy (if reference :stored :candidate))))

(defparameter *mixed-species2-short-reference*
  '((38 (57 50 52 49 50)
        (62 60 59 57 60 62 64 57 57))
    (35 (57 50 52 49 50)
        (62 60 59 57 60 62 64 69 69))))

(defparameter *mixed-species2-medium-reference*
  '((90 (57 50 52 47 47 49 50)
        (62 60 59 57 55 57 59 60 62 50 52 57 57))
    (83 (57 50 52 47 52 49 50)
        (62 60 59 57 55 57 59 55 60 62 64 57 57))
    (77 (57 50 52 47 52 49 50)
        (62 60 59 57 55 57 59 62 60 62 64 57 57))
    (69 (57 50 52 53 52 49 50)
        (62 60 59 57 55 57 59 57 60 62 64 69 69))
    (64 (57 50 52 53 52 49 50)
        (62 60 59 57 55 60 59 57 60 62 64 57 57))
    (60 (57 50 52 53 52 49 50)
        (62 60 59 57 55 60 59 57 60 62 64 69 69))))

(defun compare-mixed-species2 (&key (case :short))
  (let* ((expected (ecase case
                     (:short *mixed-species2-short-reference*)
                     (:medium *mixed-species2-medium-reference*)))
         (actual (run-mixed-species2 :case case :reference t :stream nil)))
    (format t "~&Mixed species 1+2 (~A): C ~D, CL ~D solutions~%Match: ~A~%"
            case (length expected) (length actual)
            (if (equal actual expected) "YES" "NO"))
    (unless (equal actual expected)
      (format t "Expected:~%~S~%Obtained:~%~S~%" expected actual))
    actual))

(defun run-mixed-species2-tests ()
  (let ((state (make-mixed-second-species-state
                *mixed-species2-short-cantus* '(57 62) :dorian)))
    (assert (= (total-notes state 1) 5))
    (assert (= (total-notes state 2) 9))
    (assert (= (onset-at state 2 1) 8))
    (assert (= (onset-at state 2 2) 4))
    (assert (= (onset-at state 3 2) 8))
    (assert (equal (mixed-notes-at state 4) '((2 . 2))))
    (assert (equal (mixed-notes-at state 8) '((1 . 2) (2 . 3))))
    (assert (= (next-mixed-onset state 0) 4))
    (assert (= (next-mixed-onset state 4) 8))
    (assert (integerp (check-mixed-second-species state 2
                                                  (pitch-at state 1 2)))))
  ;; SaveResults must use each voice's own final index. In 1+2 the second
  ;; voice has nine notes, while the first voice has only five.
  (let ((state (make-mixed-second-species-state
                *mixed-species2-short-cantus* '(57 62) :dorian)))
    (loop for i from 2 to 5 do (set-pitch state i 1 55))
    (loop for i from 2 to 9 do (set-pitch state i 2 55))
    (set-pitch state 6 2 57)
    (set-pitch state 7 2 58)
    (set-pitch state 8 2 59) ; raised leading tone, outside Dorian
    (set-pitch state 9 2 60)
    (poly-first-species-cadence-ficta state)
    (assert (= (pitch-at state 7 2) 59)))
  ;; The old smoke test did not check its saved C reference records.
  (assert (equal (run-mixed-species2 :case :short :reference t :stream nil)
                 *mixed-species2-short-reference*))
  (assert (equal (run-mixed-species2 :case :medium :reference t :stream nil)
                 *mixed-species2-medium-reference*))
  t)

(defun run-generator-rhythm-tests ()
  "Check pitch identity, rhythmic coverage, syncopation and explicit patterns."
  (let ((cantus '(50 53 55 52 50)))
    (dolist (species '(1 2 3 4 5))
      (let* ((records (generate-counterpoint cantus :species species
                                              :starts '(57)
                                              :patterns (and (= species 5)
                                                             '(3 4 7 9))))
             (rich (counterpoint-records-with-rhythm records cantus '(57)
                                                     species :dorian 1
                                                     (and (= species 5)
                                                          '(3 4 7 9)))))
        (assert (= (length records) (length rich)))
        (loop for original in records
              for result in rich
              do (assert (= (first original) (getf result :penalty)))
                 (let* ((voices (getf result :voices))
                        (generated (second voices))
                        (onsets (getf generated :onsets))
                        (durations (getf generated :durations)))
                   (assert (equal (second original)
                                  (getf generated :pitches)))
                   (assert (equal (getf (first voices) :pitches) cantus))
                   (assert (= (reduce #'+ durations) (* 8 (length cantus))))
                   (assert (= (car onsets) 0))
                   (assert (= (car (last durations)) 8))
                   (loop for (a b) on onsets
                         for duration in durations
                         while b do (assert (= (+ a duration) b)))))))
    (let* ((durations (fifth-species-durations 5 :patterns '(3 4 7 9)))
           (state (counterpoint-rhythm-state cantus '(57 62) 5 :dorian
                                             1 '(3 4 7 9))))
      (assert (equal durations
                     (loop for i from 1 to (total-notes state 2)
                           collect (duration-at state i 2))))
      (assert (= (total-notes state 1) 5)))
    (assert (equal (counterpoint-ties '(60 60 62) '(4 8 12) '(4 4 8) 4)
                   '((1 . 2))))
    (assert (null (counterpoint-ties '(60 60 62) '(4 8 12) '(4 4 8) 2)))
    t))

(defun run-generator-api-tests ()
  "Check that the wrapper dispatches to unchanged underlying generators."
  (let ((cantus '(50 53 55 52 50)))
    (assert (equal (generate-counterpoint cantus :species 1 :starts '(57))
                   (generate-first-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 2 :starts '(57))
                   (generate-second-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 3 :starts '(57))
                   (generate-third-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 4 :starts '(57))
                   (generate-fourth-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 5 :starts '(57)
                                          :patterns '(3 4 7 9))
                   (generate-fifth-species cantus 57
                                           :patterns '(3 4 7 9))))
    (assert (equal (generate-counterpoint cantus :species 1
                                          :starts '(57 62) :reference t)
                   (generate-poly-first-species cantus '(57 62)
                                                :integer-bounds t
                                                :terminal-bound :max-penalty)))
    (assert (equal (generate-counterpoint cantus :species 2
                                          :starts '(57 62) :reference t)
                   (generate-mixed-second-species cantus '(57 62)
                                                  :integer-bounds t
                                                  :terminal-bound :max-penalty
                                                  :crossing-policy :stored)))
    (dolist (species '(3 4 5))
      (assert (equal (generate-counterpoint cantus :species species
                                            :starts '(57 62) :reference t)
                     (generate-mixed-species cantus '(57 62) species
                                             :integer-bounds t
                                             :terminal-bound :max-penalty
                                             :crossing-policy :stored))))
    t))

(export '(
  run-basic-tests
  run-first-species-tests
  run-exs01
  run-exs02
  run-search-smoke-tests
  compare-exs02-bounds
  compare-exs02-policies
  run-exs11
  test-exs11
  compare-exs11
  run-second-species-tests
  run-species2-small
  run-species2-full
  compare-species2
  run-third-species-tests
  run-species3-small
  run-species3-full
  compare-species3
  run-fourth-species-tests
  run-species4-small
  run-species4-full
  compare-species4
  run-fifth-species-tests
  run-species5-small
  run-species5-medium
  run-species5-full
  compare-species5
  run-mixed-species2
  compare-mixed-species2
  run-mixed-species2-tests
  run-generator-rhythm-tests
  run-generator-api-tests) :fux)
