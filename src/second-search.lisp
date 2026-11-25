;;;; second-search: FUX implementation.
(in-package :fux)

(defun make-second-species-cantus (cantus start mode)
  "C AnySpecies: exactly 2*N-1 counterpoint notes, last duration 8."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (count (1- (* 2 n)))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (<= 2 n 64)
      (error "Cantus length must be 2..64 for this state representation"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 count)
    (loop for pitch in cantus for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8))
    (loop for i from 1 to count
          do (set-onset state i 1 (* 4 (1- i)))
             (set-duration state i 1 (if (= i count) 8 4)))
    (set-pitch state 1 1 (- start base))
    state))

(defun second-species-candidates (state note current-penalty best-penalty
                                            crossing-policy)
  "C: Look plus SaveIndx (second species, one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-second-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) ; C Look: SetUs after Check
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun second-species-cadence-ficta (state)
  "C: SaveResults. Mutates the candidate line before copying it to output.
The change is intentionally *not* undone by the search: C does not undo it."
  (let* ((n (total-notes state 1))
         (last-pitch (mod (pitch-at state (1- n) 1) 12))
         (mode (aref state 1)))
    (unless (in-mode last-pitch mode)
      (loop for k from 2 below (1- n)
            for i = (- n k)
            for pitch = (mod (pitch-at state i 1) 12)
            for next-diff = (- (pitch-at state (1+ i) 1)
                               (pitch-at state i 1))
            for prior-diff = (abs (- (pitch-at state i 1)
                                     (pitch-at state (1- i) 1)))
            do (when (or (and (< pitch 8) (/= pitch 0))
                         (skip-p next-diff)
                         (member prior-diff '(0 5 7 12)))
                 (return))
               ;; C: would stop if another voice holds the natural 7th.
               ;; For one-voice second species the only other voice is cantus.
               (when (= (mod (other-voice state i 1 0) 12) 11)
                 (return))
               (when (member (- (pitch-at state (1- n) 1)
                                (pitch-at state i 1)) '(1 3))
                 (incf (aref (aref state 3) i 1))))))
  state)

(defun second-species-line-from-state (state)
  (loop with base = (aref state 0)
        for i from 1 to (total-notes state 1)
        collect (+ base (pitch-at state i 1))))

(defun generate-second-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 1000)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate))
  "Port of C BestFitFirst/Look/SaveResults for one voice in second species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
No recomputation of a global optimum is implied."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-second-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 98/100)
         (branches 0)
         (results nil))
    ;; SAIL/C store MaxPenalty as an integer. Both switches are needed for
    ;; the tested C reference configuration (the earlier first-species EXS cases):
    ;; (generate-second-species cantus start :integer-bounds t
    ;;                         :terminal-bound :max-penalty)
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (second-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* best-penalty penalty-ratio)
                                             max-penalty)))
               (let ((line (second-species-line-from-state state)))
                 (push (list cost line) results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))))
             (visit-node (note accumulated)
               ;; C: BestFitFirst, branch count and reset of MaxPenalty.
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (second-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       ;; SAIL/C test CurTime, so MaxPenalty also applies
                       ;; to the final note. :BEST-FIT accepts any complete
                       ;; improvement; it does not change the rule costs.
                       (when (< total-cost
                                (if (and (= note total)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         ;; C: Look computes all options before the search
                         ;; loop overwrites the chosen pitch in Ctrpt.
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       ;; C resets between valid root alternatives. This
                       ;; loop also resets after rejection/last (README.md).
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))
