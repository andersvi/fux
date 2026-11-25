;;;; mixed-search: FUX implementation.
(in-package :fux)

(defun make-mixed-second-species-state (cantus starts mode)
  "Voice 1 uses whole notes; voice 2 uses half notes, then a whole note."
  (unless (and (<= 2 (length cantus) 32) (= (length starts) 2))
    (error "Need 2..32 cantus notes and two starting pitches"))
  (let* ((state (make-first-species-cantus cantus (first starts) mode))
         (count (1- (* 2 (length cantus)))))
    (set-total-notes state 2 count)
    (loop for note from 1 to count
          do (set-onset state note 2 (* 4 (1- note)))
             (set-duration state note 2 (if (= note count) 8 4)))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun next-mixed-onset (state time)
  "First future onset for either generated voice (C: VIndex/NextTime)."
  (let ((next nil))
    (loop for voice from 1 to 2
          do (loop for note from 2 to (total-notes state voice)
                   for onset = (onset-at state note voice)
                   when (> onset time)
                     do (setf next (if next (min next onset) onset))
                        (return)))
    next))

(defun mixed-notes-at (state time)
  "Return (VOICE . NOTE) for voices beginning new notes at TIME."
  (loop for voice from 1 to 2
        for note = (loop for i from 2 to (total-notes state voice)
                         when (= (onset-at state i voice) time)
                           return i)
        when note collect (cons voice note)))

(defun mixed-second-species-candidates (state notes accumulated best-penalty
                                      crossing-policy)
  "C Look/SaveIndx: at most 16 ranked combinations of moving voices.
Each result is (local-penalty (voice . interval-index)...)."
  (let ((limit (- best-penalty accumulated))
        (options nil))
    (labels ((visit (remaining local-cost indices)
               (if (endp remaining)
                   (push (list local-cost (reverse indices)) options)
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous (aref *search-intervals* index))
                           for local = (if (= voice 1)
                                           (check-poly-first-species
                                            state note pitch
                                            :voice 1 :num-parts 2
                                            :limit (- limit local-cost)
                                            :crossing-policy crossing-policy)
                                           (check-mixed-second-species
                                            state note pitch
                                            :voice 2 :num-parts 2
                                            :limit (- limit local-cost)
                                            :crossing-policy crossing-policy))
                           for cost = (+ local-cost local)
                           do (set-pitch state note voice pitch) ; C: SetUs after Check
                              (when (< cost limit)
                                (visit (cdr remaining) cost
                                       (cons (cons voice index) indices))))))))
      (visit notes 0 nil))
    ;; Use a stable ordering so ties follow interval enumeration in C.
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun generate-mixed-second-species (cantus starts
                                      &key (mode :dorian) (stream nil)
                                        (integer-bounds nil)
                                        (terminal-bound :best-fit)
                                        (crossing-policy :candidate)
                                        (branch-limit 800))
  "Generate voice 1 in first species, voice 2 in second species.
Return improvements as (COST (VOICE-1...) (VOICE-2...)).
Use :INTEGER-BOUNDS T and :TERMINAL-BOUND :MAX-PENALTY to match
C's documented search-bound behavior."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-mixed-second-species-state cantus starts mode))
         (total-time (aref state 2))
         (best-penalty 1000000)
         (max-penalty 1000000)
         (ratio 24/25)  ; C: 1.0 - Species(2) * NumParts(2) * .01
         (branches 0)
         (results nil))
    (labels ((bound (value) (if integer-bounds (truncate value) value))
             (save-result (cost)
               (poly-first-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min max-penalty (* ratio cost))))
               (let ((record (cons cost (poly-lines-from-state state))))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] ~%" cost)
                   (dolist (line (cdr record))
                     (format stream "~{~D ~}~%" line)))))
             (visit-node (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time (next-mixed-onset state time))
                        (notes (and next-time (mixed-notes-at state next-time))))
                   (when notes
                     (dolist (option (mixed-second-species-candidates
                                      state notes accumulated best-penalty
                                      crossing-policy))
                       (let ((total-cost (+ accumulated (first option))))
                         (when (< total-cost
                                  (if (and (= next-time total-time)
                                           (eq terminal-bound :best-fit))
                                      best-penalty
                                      max-penalty))
                           (dolist (choice (second option))
                             (let* ((voice (car choice))
                                    (note (cdr (assoc voice notes)))
                                    (previous (pitch-at state (1- note) voice)))
                               (set-pitch state note voice
                                          (+ previous
                                             (aref *search-intervals* (cdr choice))))))
                           (if (< next-time total-time)
                               (visit-node next-time total-cost)
                               (save-result total-cost)))
                         (when (= time 0)
                           (setf max-penalty
                                 (bound (* ratio best-penalty)))))))))))
      (visit-node 0 0)
      (nreverse results))))
