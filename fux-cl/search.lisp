;;;; Search in first species, one generated voice.
;;;; Source: fux.c SaveIndx, SaveResults, Look, BestFitFirst, AnySpecies.
;;;; The musical evaluator is in first-species.lisp.
;;;; SAIL (1984): INTEGER MaxPenalty, REAL PenaltyRatio.
;;;; C (1995): integer bound truncated on assignment and by MIN(int,int).
;;;; CL default: retain exact rational bounds; :integer-bounds t tests C-style truncation.
;;;; Final-candidate acceptance is a separate, explicit policy below.
;;;; See README.md for the explained exs02 difference and tested scope.

(in-package :fux)

(defparameter *search-intervals*
  #(0 1 -1 2 -2 3 -3 0 4 -4 5 7 -5 8 12 -7 -12))

(defun make-first-species-cantus (cantus start mode)
  "C: AnySpecies, restricted to one generated voice, first species.
CANTUS and START are absolute semitone pitches."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (<= 2 n 127)
      (error "Cantus length must be between 2 and 127"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 n)
    (loop for pitch in cantus
          for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8)
             (set-onset state i 1 (* 8 (1- i)))
             (set-duration state i 1 8))
    (set-pitch state 1 1 (- start base))
    state))

(defun first-species-candidates (state note current-penalty best-penalty
                                            crossing-policy &optional provenance)
  "C: Look plus SaveIndx (one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-first-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) ; C Look: SetUs after Check
             (when provenance (setf (aref provenance note) nil))
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun first-species-cadence-ficta (state)
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
               ;; For one-voice first species the only other voice is cantus.
               (when (= (mod (other-voice state i 1 0) 12) 11)
                 (return))
               (when (member (- (pitch-at state (1- n) 1)
                                (pitch-at state i 1)) '(1 3))
                 (incf (aref (aref state 3) i 1))))))
  state)

(defun line-from-state (state)
  (loop with base = (aref state 0)
        for i from 1 to (total-notes state 1)
        collect (+ base (pitch-at state i 1))))

(defun %score-first-species-line (cantus pitches mode ficta-positions)
  "Evaluate pitches sequentially; only listed notes bypass the mode gate."
  (unless (= (length cantus) (length pitches))
    (error "Cantus and counterpoint must have the same length"))
  (let* ((state (make-first-species-cantus cantus (first pitches) mode))
         (base (aref state 0))
         (locals (list 0)))
    (loop for pitch in (rest pitches)
          for note from 2
          for candidate = (- pitch base)
          do (push (check-first-species state note candidate
                                        :limit 10000000
                                        :crossing-policy :candidate
                                        :ficta-mode-exemption
                                        (not (null (member note ficta-positions))))
                   locals)
             (set-pitch state note 1 candidate))
    (setf locals (nreverse locals))
    (values (reduce #'+ locals) locals)))

(defun score-first-species-line (cantus pitches &key (mode :dorian))
  "Fresh literal Check of an absolute-pitch first-species line.
Return total and local penalties, with zero for the fixed first note.
Evaluate against the installed prefix using candidate crossing; do not
apply or exempt ficta. A large result is not a musical validity verdict."
  (%score-first-species-line cantus pitches mode nil))

(defun score-first-species-post-ficta (cantus raw notated
                                      &key (mode :dorian) provenance)
  "Recheck NOTATED with a narrow exemption for this save's proven ficta.
RAW must reproduce NOTATED by one call to SaveResults' ficta pass.
Return total, local penalties, and the one-based positions exempted from
the ordinary mode gate. PROVENANCE is a list of (note before after) from
the generator; earlier inherited alterations require that extra history.
Other melodic and harmonic checks still use the notated pitches."
  (unless (and (= (length cantus) (length raw))
               (= (length raw) (length notated)))
    (error "Cantus, raw and notated lines must have the same length"))
  (let* ((state (make-first-species-cantus cantus (first raw) mode))
         (base (aref state 0)))
    (loop for pitch in (rest raw)
          for note from 2
          do (set-pitch state note 1 (- pitch base)))
    (first-species-cadence-ficta state)
    (unless (equal (line-from-state state) notated)
      (error "Notated line does not follow from one SaveResults ficta pass"))
    (let* ((raised (loop for before in raw for after in notated
                         for note from 1
                         unless (= before after) collect note))
           (exemptions (copy-list raised)))
      (dolist (entry provenance)
        (unless (and (listp entry) (= (length entry) 3)
                     (integerp (first entry))
                     (<= 2 (first entry) (length raw))
                     (integerp (second entry)) (integerp (third entry))
                     (= (third entry) (1+ (second entry)))
                     (= (nth (1- (first entry)) notated) (third entry))
                     (or (member (first entry) raised)
                         (= (nth (1- (first entry)) raw) (third entry))))
          (error "Invalid ficta provenance: ~S" entry))
        (pushnew (first entry) exemptions))
      (setf exemptions (sort exemptions #'<))
      (multiple-value-bind (total locals)
          (%score-first-species-line cantus notated mode exemptions)
        (values total locals exemptions)))))

(defun generate-first-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 1250)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate)
                                             (save-state-policy :historic)
                                             (result-format :legacy))
  "Port of C BestFitFirst/Look/SaveResults for one voice in first species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
SAVE-STATE-POLICY :RESTORE removes SaveResults' work-array mutation after
the notated result is copied; :HISTORIC keeps it for later search branches.
No recomputation of a global optimum is implied."
  (unless (member result-format '(:legacy :with-ficta))
    (error "Unknown result format: ~S" result-format))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (unless (member save-state-policy '(:historic :restore))
    (error "Unknown save-state policy: ~S" save-state-policy))
  (let* ((state (make-first-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 99/100)
         (branches 0)
         (provenance (make-array (1+ total) :initial-element nil))
         (results nil))
    ;; SAIL/C store MaxPenalty as an integer. Both switches are needed for
    ;; the tested C reference configuration (all ten one-voice EXS cases):
    ;; (generate-first-species cantus start :integer-bounds t
    ;;                         :terminal-bound :max-penalty)
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (let* ((detailed (eq result-format :with-ficta))
                      (raw (when (or detailed
                                     (eq save-state-policy :restore))
                            (line-from-state state))))
                 (first-species-cadence-ficta state)
                 (setf best-penalty cost
                       max-penalty (bound (min (* best-penalty penalty-ratio)
                                               max-penalty)))
                 (let* ((line (line-from-state state))
                        (changes (when raw
                                   (loop for before in raw for after in line
                                         for index from 1
                                         unless (= before after)
                                           collect (list index before after))))
                        (notated-penalty nil)
                        (notated-locals nil))
                   (dolist (entry changes)
                     (setf (aref provenance (first entry)) entry))
                   (when detailed
                     (multiple-value-setq (notated-penalty notated-locals)
                       (score-first-species-line cantus line :mode mode)))
                   (push (if detailed
                             (list :search-penalty cost
                                   :raw-pitches raw
                                   :notated-pitches line
                                   :ficta-changes changes
                                   :ficta-provenance
                                   (loop for note from 1 to total
                                         when (aref provenance note)
                                           collect (copy-list
                                                    (aref provenance note)))
                                   :notated-penalty notated-penalty
                                   :notated-local-penalties notated-locals)
                             (list cost line))
                         results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))
                 (when (eq save-state-policy :restore)
                   (loop for pitch in raw for note from 1
                         do (set-pitch state note 1
                                       (- pitch (aref state 0))))
                   (fill provenance nil)))))
             (visit-node (note accumulated)
               ;; C: BestFitFirst, branch count and reset of MaxPenalty.
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (first-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy provenance)))
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
                         (setf (aref provenance note) nil)
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       ;; C resets between valid root alternatives. This
                       ;; loop also resets after rejection/last (README.md).
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))

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
