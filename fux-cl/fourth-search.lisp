;;;; Fourth species: one generated voice, half-notes and final whole note.
;;;; C: AnySpecies uses (2*N-1) notes and onsets at 0,4,...,8*(N-2),
;;;; followed by a final whole note. Equal pitches across downbeats are ties.  C Look/BestFitFirst reused below.
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
