;;;; Fifth species: mixed rhythms and melodic search.
;;;; C: FillRhyPat, GoodRhy, AnySpecies, Look, BestFitFirst, SaveResults.
;;;; Fourth/third species files remain unchanged.
(in-package :fux)

(defparameter *fifth-rhythm-patterns*
  #(()
    (4 4)                   ; 1: two half notes
    (4 2 2)                 ; 2: half, quarters
    (2 2 2 2)               ; 3: four quarters
    (2 2 4)                 ; 4: quarters, half
    (2 1 1 4)               ; 5: quarter, eighths, half
    (2 1 1 2 2)             ; 6: quarter, eighths, quarters
    (4 2 1 1)               ; 7: half, quarter, eighths
    (2 1 1 2 1 1)           ; 8: quarter, eighths, quarter, eighths
    (2 2 2 1 1)))          ; 9: three quarters, eighths

(defun fifth-rhythm-pattern-indices (bars &key (seed 1))
  "C: GoodRhy, with a deterministic 31-bit LCG and usage counts.
RANDOM(10.0) can produce indexes as high as 19 in the C port even though
patterns 1..9 are valid. We clamp to [1,9] before reading a count;
see README-species5.md. All current reference runs use SEED=1."
  (unless (and (integerp bars) (<= 0 bars))
    (error "Invalid number of fifth-species bars: ~S" bars))
  (let ((rng seed)
        (used (make-array 10 :initial-element 0))
        (chosen nil))
    (dotimes (bar bars (nreverse chosen))
      (declare (ignore bar))
      ;; C RANDOM: low 31 bits are sufficient for the returned 15 bits.
      (setf rng (mod (+ (* rng 1103515245) 12345) 2147483648))
      (let* ((bits (mod (floor rng 65536) 32768))
             (raw (floor (* 10 bits) 16384))
             (i (max 1 (min 9 raw)))
             (before (max 1 (1- i)))
             (after (min 9 (1+ i)))
             (choice (cond ((> (aref used i) (aref used before)) before)
                           ((<= (aref used i) (aref used after)) after)
                           (t i))))
        (incf (aref used choice))
        (push choice chosen)))))

(defun fifth-species-durations (cantus-length &key (seed 1) patterns)
  "Return durations (whole=8, half=4, quarter=2, eighth=1).
PATTERNS is an optional explicit list of per-bar indices 1..9."
  (let ((indices (or patterns
                     (fifth-rhythm-pattern-indices (1- cantus-length)
                                                   :seed seed))))
    (unless (= (length indices) (1- cantus-length))
      (error "Expected ~D rhythm patterns, got ~D"
             (1- cantus-length) (length indices)))
    (append (loop for i in indices
                  append (progn
                           (unless (and (integerp i) (<= 1 i 9))
                             (error "Invalid fifth-species pattern: ~S" i))
                           (copy-list (aref *fifth-rhythm-patterns* i))))
            '(8))))

(defun make-fifth-species-cantus (cantus start mode
                                    &key (seed 1) patterns)
  "C: AnySpecies, one moving voice in fifth species.
The rhythm is chosen once, before the pitch search, as in the C port."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (durs (fifth-species-durations n :seed seed :patterns patterns))
         (count (length durs))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (and (<= 2 n) (< count 128))
      (error "Cantus/rhythm exceeds the 128-note reference state"))
    (unless (= (reduce #'+ durs) (* 8 n))
      (error "Fifth-species rhythm does not span the cantus"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 count)
    (loop for pitch in cantus for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8))
    (loop with onset = 0
          for dur in durs for i from 1
          do (set-onset state i 1 onset)
             (set-duration state i 1 dur)
             (incf onset dur))
    (set-pitch state 1 1 (- start base))
    state))

(defun fifth-species-candidates (state note accumulated best crossing-policy)
  "C: Look + SaveIndx. Equal-cost candidates retain interval order."
  (let ((previous (pitch-at state (1- note) 1))
        (options nil))
    (loop for i from 1 to 16
          for candidate = (+ previous (aref *search-intervals* i))
          for cost = (check-fifth-species
                      state note candidate :limit (- best accumulated)
                      :crossing-policy crossing-policy)
          do (set-pitch state note 1 candidate) ; historical Look order
             (when (< cost (- best accumulated))
               (push (cons cost i) options)))
    (stable-sort (nreverse options) #'< :key #'car)))

(defun fifth-species-cadence-ficta (state)
  (third-species-cadence-ficta state))

(defun fifth-species-line-from-state (state)
  (third-species-line-from-state state))

(defun generate-fifth-species (cantus start
                               &key (mode :dorian) (stream nil)
                                 (seed 1) patterns
                                 (branch-limit 250)
                                 (integer-bounds nil)
                                 (terminal-bound :best-fit)
                                 (crossing-policy :candidate))
  "C: BestFitFirst for one generated voice in florid counterpoint.
Return ((penalty (pitches...)) ...). Rhythm is selectable by SEED or
PATTERNS, but fixed during a pitch search, as in C. The :REFERENCE
configuration corresponds to integer bounds, stored crossing and MaxPenalty."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal-bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-fifth-species-cantus cantus start mode
                                            :seed seed :patterns patterns))
         (last-note (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 95/100)
         (branches 0)
         (results nil))
    (labels ((bound (number)
               (if integer-bounds (truncate number) number))
             (save-result (cost)
               (fifth-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* cost penalty-ratio)
                                             max-penalty)))
               (let ((line (fifth-species-line-from-state state)))
                 (push (list cost line) results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))))
             (visit-node (note accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (fifth-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       (when (< total-cost
                                (if (and (= note last-note)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (if (< note last-note)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       (when (= note 2)
                         (setf max-penalty
                               (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))

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
