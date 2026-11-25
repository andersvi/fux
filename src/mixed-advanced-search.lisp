;;;; mixed-advanced-search: FUX implementation.
(in-package :fux)

(defun mixed-advanced-durations (cantus-length species &key (seed 1) patterns)
  "Onset durations for the final voice. Whole=8; a final whole is appended."
  (case species
    (3 (append (make-list (* 4 (1- cantus-length)) :initial-element 2) '(8)))
    (4 (append (make-list (* 2 (1- cantus-length)) :initial-element 4) '(8)))
    (5 (fifth-species-durations cantus-length :seed seed :patterns patterns))
    (otherwise (error "Expected species 3, 4 or 5; received ~S" species))))

(defun make-mixed-advanced-state (cantus starts mode species
                                  &key (seed 1) patterns)
  "Two generated voices: voice 1 whole notes, voice 2 selected species."
  (unless (and (<= 2 (length cantus) 32) (= (length starts) 2))
    (error "Need 2..32 cantus pitches and two starts"))
  (let* ((state (make-first-species-cantus cantus (first starts) mode))
         (durations (mixed-advanced-durations (length cantus) species
                                               :seed seed :patterns patterns)))
    (unless (< (length durations) 128)
      (error "Too many events for the original 128-note state"))
    (set-total-notes state 2 (length durations))
    (loop with onset = 0
          for dur in durations
          for i from 1
          do (set-onset state i 2 onset)
             (set-duration state i 2 dur)
             (incf onset dur))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun check-mixed-advanced (state note candidate species &key
                              (limit 1000000) (crossing-policy :candidate))
  "C Check for the FINAL generated voice, with first-species accompaniment."
  (case species
    (3 (check-mixed-third-species state note candidate
                                  :voice 2 :num-parts 2 :limit limit
                                  :crossing-policy crossing-policy))
    (4 (check-mixed-fourth-species state note candidate
                                   :voice 2 :num-parts 2 :limit limit
                                   :crossing-policy crossing-policy))
    (5 (check-mixed-fifth-species state note candidate
                                  :voice 2 :num-parts 2 :limit limit
                                  :crossing-policy crossing-policy))
    (otherwise (error "Invalid mixed species ~S" species))))

(defun mixed-advanced-candidates (state notes accumulated best species
                                   crossing-policy)
  "C Look/SaveIndx: 16 best simultaneous candidate combinations."
  (let ((limit (- best accumulated))
        (options nil))
    (labels ((visit (remaining cost indices)
               (if (endp remaining)
                   (push (list cost (reverse indices)) options)
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous (aref *search-intervals* index))
                           for local = (if (= voice 1)
                                           (check-poly-first-species
                                            state note pitch :voice 1 :num-parts 2
                                            :limit (- limit cost)
                                            :crossing-policy crossing-policy)
                                           (check-mixed-advanced
                                            state note pitch species
                                            :limit (- limit cost)
                                            :crossing-policy crossing-policy))
                           for next-cost = (+ cost local)
                           do (set-pitch state note voice pitch) ; C: SetUs after Check
                              (when (< next-cost limit)
                                (visit (cdr remaining) next-cost
                                       (cons (cons voice index) indices))))))))
      (visit notes 0 nil))
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun mixed-advanced-cadence-ficta (state)
  "C SaveResults: treat each voice's own penultimate note as its cadence."
  (loop with mode = (aref state 1)
        for voice from 1 to 2
        for n = (total-notes state voice)
        do (unless (in-mode (mod (pitch-at state (1- n) voice) 12) mode)
             (loop for k from 2 below (1- n)
                   for i = (- n k)
                   for pitch = (mod (pitch-at state i voice) 12)
                   for next-diff = (- (pitch-at state (1+ i) voice)
                                      (pitch-at state i voice))
                   for previous-diff = (abs (- (pitch-at state i voice)
                                               (pitch-at state (1- i) voice)))
                   do (when (or (and (< pitch 8) (/= pitch 0))
                                (skip-p next-diff)
                                (member previous-diff '(0 5 7 12)))
                        (return))
                      (when (loop for other from 0 to 2
                                  thereis (and (/= other voice)
                                               (= (mod (other-voice state i voice other)
                                                       12)
                                                  11)))
                        (return))
                      (when (member (- (pitch-at state (1- n) voice)
                                       (pitch-at state i voice)) '(1 3))
                        (incf (aref (aref state 3) i voice))))))
  state)

(defun generate-mixed-species (cantus starts species
                               &key (mode :dorian) (stream nil)
                                 (seed 1) patterns
                                 (integer-bounds nil)
                                 (terminal-bound :best-fit)
                                 (crossing-policy :candidate)
                                 (branch-limit (* 200 (- 6 species))))
  "Generate two voices, first in species 1 and second in SPECIES 3/4/5.
Returns ((penalty (voice-1...) (voice-2...)) ...).
C reference policy: :INTEGER-BOUNDS T :TERMINAL-BOUND :MAX-PENALTY
:CROSSING-POLICY :STORED. Rhy patterns for species 5 may be explicit."
  (unless (member species '(3 4 5))
    (error "SPECIES must be 3, 4, or 5"))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-mixed-advanced-state cantus starts mode species
                                           :seed seed :patterns patterns))
         (total-time (aref state 2))
         (best 1000000)
         (max-penalty 1000000)
         (ratio (- 1 (/ species 50))) ; C: 1 - species*2*.01
         (branches 0)
         (results nil))
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (mixed-advanced-cadence-ficta state)
               (setf best cost
                     max-penalty (bound (min max-penalty (* ratio cost))))
               (let ((record (cons cost (poly-lines-from-state state))))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] ~%" cost)
                   (dolist (line (cdr record))
                     (format stream "~{~D ~}~%" line)))))
             (visit (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time (next-mixed-onset state time))
                        (notes (and next-time (mixed-notes-at state next-time))))
                   (when notes
                     (dolist (option (mixed-advanced-candidates
                                      state notes accumulated best species
                                      crossing-policy))
                       (let ((cost (+ accumulated (first option))))
                         (when (< cost
                                  (if (and (= next-time total-time)
                                           (eq terminal-bound :best-fit))
                                      best max-penalty))
                           (dolist (choice (second option))
                             (let* ((voice (car choice))
                                    (note (cdr (assoc voice notes)))
                                    (previous (pitch-at state (1- note) voice)))
                               (set-pitch state note voice
                                          (+ previous (aref *search-intervals*
                                                            (cdr choice))))))
                           (if (< next-time total-time)
                               (visit next-time cost)
                               (save-result cost)))
                         (when (zerop time)
                           (setf max-penalty (bound (* ratio best)))))))))))
      (visit 0 0)
      (nreverse results))))
