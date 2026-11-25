;;;; poly-search: FUX implementation.
(in-package :fux)

(defun make-poly-first-species-state (cantus starts mode)
  "As in C AnySpecies, STARTS contains one absolute pitch per generated voice."
  (unless (= (length starts) 2)
    (error "EXS11 needs exactly two starting pitches"))
  (let ((state (make-first-species-cantus cantus (first starts) mode))
        (n (length cantus)))
    (set-total-notes state 2 n)
    (loop for i from 1 to n
          do (set-duration state i 2 8)
             (set-onset state i 2 (* 8 (1- i))))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun poly-first-species-candidates (state note accumulated best-penalty)
  "C: Look/SaveIndx for two simultaneous voices. Return top 16 triples
(COST INDEX-VOICE-1 INDEX-VOICE-2), keeping discovery order on ties."
  (let ((limit (- best-penalty accumulated))
        (options nil)
        (previous1 (pitch-at state (1- note) 1))
        (previous2 (pitch-at state (1- note) 2)))
    (loop for i from 1 to 16
          for pitch1 = (+ previous1 (aref *search-intervals* i))
          for cost1 = (check-poly-first-species state note pitch1
                                                :voice 1 :num-parts 2
                                                :limit limit)
          do (set-pitch state note 1 pitch1) ; C Look: SetUs after Check
             (when (< cost1 limit)
               (loop for j from 1 to 16
                     for pitch2 = (+ previous2 (aref *search-intervals* j))
                     for cost2 = (check-poly-first-species
                                  state note pitch2 :voice 2 :num-parts 2
                                  :limit (- limit cost1))
                     for cost = (+ cost1 cost2)
                     do (set-pitch state note 2 pitch2)
                        (when (< cost limit)
                          (push (list cost i j) options)))))
    ;; C SaveIndx: the fixed-width 16 best candidates, stable on ties.
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun poly-first-species-cadence-ficta (state)
  "C: SaveResults for all generated voices. Historically mutates work state."
  (let ((mode (aref state 1)))
    (loop for voice from 1 to 2
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
                                                   (= (mod (other-voice state i voice other) 12)
                                                      11)))
                          (return))
                        (when (member (- (pitch-at state (1- n) voice)
                                         (pitch-at state i voice)) '(1 3))
                          (incf (aref (aref state 3) i voice))))))
    state))

(defun poly-lines-from-state (state)
  (loop with base = (aref state 0)
        for voice from 1 to 2
        collect (loop for i from 1 to (total-notes state voice)
                      collect (+ base (pitch-at state i voice)))))

(defun generate-poly-first-species (cantus starts
                                    &key (mode :dorian) (stream nil)
                                      (branch-limit 1000)
                                      (integer-bounds nil)
                                      (terminal-bound :best-fit))
  "EXS11-style search. Results: (COST (VOICE-1...) (VOICE-2...)).
TERMINAL-BOUND :MAX-PENALTY and INTEGER-BOUNDS T approach historical C.
The default retains the CL search improvement policy."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Invalid terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-poly-first-species-state cantus starts mode))
         (n (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 1000000)
         (penalty-ratio 98/100)
         (branches 0)
         (results nil))
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (poly-first-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* cost penalty-ratio)
                                             max-penalty)))
               (let* ((lines (poly-lines-from-state state))
                      (record (cons cost lines)))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] " cost)
                   (dolist (line lines)
                     (format stream "~{~D ~}~%" line)))))
             (visit-node (note accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (poly-first-species-candidates
                                 state note accumulated best-penalty)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (first option))))
                       (when (< total-cost
                                (if (and (= note n)
                                         (eq terminal-bound :best-fit))
                                    best-penalty max-penalty))
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (second option))))
                         (set-pitch state note 2
                                    (+ (pitch-at state (1- note) 2)
                                       (aref *search-intervals* (third option))))
                         (if (< note n)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       (when (= note 2)
                         (setf max-penalty
                               (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))
