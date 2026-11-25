;;;; fifth-search: FUX implementation.
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
    (2 2 2 1 1)))

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
                                 (crossing-policy :candidate)
                                 (with-search-costs nil))
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
         (results nil)
         (local-costs (when with-search-costs
                        (make-array 128 :initial-element 0))))
    (labels ((bound (number)
               (if integer-bounds (truncate number) number))
             (save-result (cost)
               ;; Capture the selected branch before SaveResults changes pitches.
               (let ((raw (when with-search-costs
                            (fifth-species-line-from-state state)))
                     (costs (when with-search-costs
                              (loop for i from 1 to last-note
                                    collect (aref local-costs i)))))
                 (when with-search-costs
                   (unless (= cost (reduce #'+ costs))
                     (error "Search cost mismatch")))
                 (fifth-species-cadence-ficta state)
                 (setf best-penalty cost
                       max-penalty (bound (min (* cost penalty-ratio)
                                               max-penalty)))
                 (let ((line (fifth-species-line-from-state state)))
                   (push (if with-search-costs
                             (list cost line :search-local-costs costs
                                   :raw-pitches raw
                                   :ficta-changes
                                   (loop for before in raw for after in line
                                         for i from 1
                                         unless (= before after)
                                           collect (list i before after)))
                             (list cost line))
                         results)
                   (when stream
                     (format stream "~% [~D] ~{~D ~}~%" cost line)))))
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
                         (when with-search-costs
                           (setf (aref local-costs note) (car option)))
                         (if (< note last-note)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       (when (= note 2)
                         (setf max-penalty
                               (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))
