;;;; fourth-search: FUX implementation.
(in-package :fux)

(defun make-fourth-species-cantus (cantus start mode)
  "C AnySpecies: same half-note rhythm as second species, but with ties
  and suspensions evaluated according to the fourth-species rules."
  (make-second-species-cantus cantus start mode))

(defun fourth-species-candidates (state note current-penalty best-penalty
                                            crossing-policy)
  "C: Look plus SaveIndx (fourth species, one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-fourth-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) ; C Look: SetUs after Check
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun fourth-species-cadence-ficta (state)
  "C SaveResults: same ficta logic for all one-voice species."
  (second-species-cadence-ficta state))

(defun fourth-species-line-from-state (state)
  (second-species-line-from-state state))

(defun fourth-species-ties (pitches)
  "Return (from-note . to-note) ties in a complete pitch line.
  In this species, equal notes at an odd numbered note index (3,5,...)
  represent a half-note continued across the cantus downbeat."
  (loop for previous in pitches
        for current in (cdr pitches)
        for note from 2
        when (and (oddp note) (= previous current))
          collect (cons (1- note) note)))

(defun generate-fourth-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 500)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate))
  "Port of C BestFitFirst/Look/SaveResults for one voice in fourth species.
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
  (let* ((state (make-fourth-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 96/100)
         (branches 0)
         (results nil))
    ;; SAIL/C store MaxPenalty as an integer. Both switches are needed for
    ;; the tested historical C reference configuration:
    ;; (generate-fourth-species cantus start :integer-bounds t
    ;;                         :terminal-bound :max-penalty)
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (fourth-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* best-penalty penalty-ratio)
                                             max-penalty)))
               (let ((line (fourth-species-line-from-state state)))
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
                 (let ((options (fourth-species-candidates
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
