;;;; many-voice-search: FUX implementation.
(in-package :fux)

(defun many-voice-durations (bars species seed patterns)
  (case species
    (1 (make-list bars :initial-element 8))
    ((2 4) (append (make-list (* 2 (1- bars)) :initial-element 4)
                   '(8)))
    (3 (append (make-list (* 4 (1- bars)) :initial-element 2)
               '(8)))
    (5 (fifth-species-durations bars :seed seed :patterns patterns))
    (otherwise (error "Species must be 1..5: ~S" species))))

(defun make-many-voice-state (cantus starts mode species
                              &key (seed 1) patterns inner-durations
                                   inner-half-notes)
  "Build C AnySpecies state with 3..5 moving voices.
INNER-DURATIONS is a list of duration lists, one per earlier voice. These
voices still use first-species Check rules, regardless of their rhythm.
INNER-HALF-NOTES uses half notes for earlier voices 2..N-1, as in the
documented five-staff C experiment; voice 1 remains in whole notes."
  (let* ((n (length cantus))
         (num-parts (length starts)))
    (unless (and (listp cantus) (<= 2 n 127)
                 (every #'integerp cantus)
                 (listp starts) (<= 3 num-parts 5)
                 (every #'integerp starts)
                 (assoc mode *modes*)
                 (member species '(1 2 3 4 5)))
      (error "Expected cantus 2..127 notes, 3..5 starts, mode, species 1..5"))
    (when (and inner-durations inner-half-notes)
      (error "Specify either INNER-DURATIONS or INNER-HALF-NOTES"))
    (when (and patterns (/= species 5))
      (error "PATTERNS applies to fifth species only"))
    (when (and inner-durations
               (or (not (listp inner-durations))
                   (/= (length inner-durations) (1- num-parts))))
      (error "INNER-DURATIONS must contain one list for each earlier voice"))
    (let* ((state (make-first-species-cantus cantus (first starts) mode))
           (half (append (make-list (* 2 (1- n)) :initial-element 4) '(8)))
           (whole (make-list n :initial-element 8))
           (earlier (or inner-durations
                        (loop for voice from 1 below num-parts
                              collect (if (and inner-half-notes (> voice 1))
                                          half whole))))
           (durations (append earlier
                              (list (many-voice-durations n species
                                                          seed patterns))))
           (total (* 8 n)))
      (loop for ds in durations for voice from 1
            for start in starts
            do (unless (and (listp ds) (<= 2 (length ds) 127)
                            (every (lambda (x) (and (integerp x) (> x 0))) ds)
                            (= (reduce #'+ ds) total)
                            (= (car (last ds)) 8))
                 (error "Voice ~D needs 2..127 positive durations spanning ~D, last 8"
                        voice total))
               (set-total-notes state voice (length ds))
               (loop with onset = 0
                     for d in ds for i from 1
                     do (set-onset state i voice onset)
                        (set-duration state i voice d)
                        (incf onset d))
               (set-pitch state 1 voice (- start (aref state 0))))
      ;; C Look saves a combination only upon reaching CurVoice=NumParts.
      ;; Thus every earlier attack must coincide with one in the final voice.
      (let ((final-onsets (loop for i from 2 to (total-notes state num-parts)
                                collect (onset-at state i num-parts))))
        (loop for voice from 1 below num-parts
              do (loop for i from 2 to (total-notes state voice)
                       for time = (onset-at state i voice)
                       unless (member time final-onsets :test #'=)
                         do (error "Voice ~D has onset ~D without final-voice onset"
                                   voice time))))
      state)))

(defun many-voice-next-onset (state time num-parts)
  (let ((next nil))
    (loop for voice from 1 to num-parts
          do (loop for i from 2 to (total-notes state voice)
                   for onset = (onset-at state i voice)
                   when (> onset time)
                     do (setf next (if next (min next onset) onset))
                        (return)))
    next))

(defun many-voice-notes-at (state time num-parts)
  (loop for voice from 1 to num-parts
        for note = (loop for i from 2 to (total-notes state voice)
                         when (= (onset-at state i voice) time) return i)
        when note collect (cons voice note)))

(defun many-voice-local-cost (state note pitch voice num-parts species
                               limit crossing-policy)
  (if (< voice num-parts)
      (check-poly-first-species state note pitch
                                :voice voice :num-parts num-parts
                                :limit limit :crossing-policy crossing-policy)
      (case species
        (1 (check-poly-first-species state note pitch
                                     :voice voice :num-parts num-parts
                                     :limit limit :crossing-policy crossing-policy))
        (2 (check-mixed-second-species state note pitch
                                       :voice voice :num-parts num-parts
                                       :limit limit :crossing-policy crossing-policy))
        (3 (check-mixed-third-species state note pitch
                                      :voice voice :num-parts num-parts
                                      :limit limit :crossing-policy crossing-policy))
        (4 (check-mixed-fourth-species state note pitch
                                       :voice voice :num-parts num-parts
                                       :limit limit :crossing-policy crossing-policy))
        (5 (check-mixed-fifth-species state note pitch
                                      :voice voice :num-parts num-parts
                                      :limit limit :crossing-policy crossing-policy)))))

(defun many-voice-candidates (state notes accumulated best num-parts species
                                crossing-policy &optional with-search-costs)
  "Keep the lowest 16 combinations in discovery order for equal costs.
The active cutoff removes only combinations that cannot enter this set."
  (let ((ranked nil)
        (limit (- best accumulated)))
    (labels ((insert (option items)
               (cond ((endp items) (list option))
                     ((< (first option) (first (car items)))
                      (cons option items))
                     (t (cons (car items) (insert option (cdr items))))))
             (cutoff ()
               (if (= (length ranked) 16)
                   (min limit (first (car (last ranked))))
                   limit))
             (visit (remaining cost indices local-costs)
               (if (endp remaining)
                   (when (or (< (length ranked) 16)
                             (< cost (first (car (last ranked)))))
                     (setf ranked (insert (if with-search-costs
                                              (list cost (reverse indices)
                                                    (reverse local-costs))
                                              (list cost (reverse indices)))
                                          ranked))
                     (when (> (length ranked) 16)
                       (setf ranked (butlast ranked))))
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous
                                          (aref *search-intervals* index))
                           for local = (many-voice-local-cost
                                        state note pitch voice num-parts species
                                        (- (cutoff) cost) crossing-policy)
                           for new-cost = (+ cost local)
                           do (set-pitch state note voice pitch)
                              (when (< new-cost (cutoff))
                                (visit (cdr remaining) new-cost
                                       (cons (cons voice index) indices)
                                       (if with-search-costs
                                           (cons local local-costs)
                                           nil))))))))
      (visit notes 0 nil nil))
    ranked))

(defun many-voice-cadence-ficta (state num-parts)
  "C SaveResults, treating each voice's own penultimate note as cadence."
  (loop with mode = (aref state 1)
        for voice from 1 to num-parts
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
                      (when (loop for other from 0 to num-parts
                                  thereis (and (/= other voice)
                                               (= (mod (other-voice state i voice other)
                                                       12) 11)))
                        (return))
                      (when (member (- (pitch-at state (1- n) voice)
                                       (pitch-at state i voice)) '(1 3))
                        (incf (aref (aref state 3) i voice))))))
  state)

(defun many-voice-lines (state num-parts)
  (loop with base = (aref state 0)
        for voice from 1 to num-parts
        collect (loop for i from 1 to (total-notes state voice)
                      collect (+ base (pitch-at state i voice)))))

(defun generate-many-voice-counterpoint
    (cantus starts species &key (mode :dorian) (stream nil)
                            (seed 1) patterns inner-durations inner-half-notes
                            (integer-bounds nil)
                            (terminal-bound :best-fit)
                            (crossing-policy :candidate)
                            branch-limit with-search-costs)
  "C AnySpecies for 3..5 generated voices.
Earlier voices get first-species RULES; their rhythms may be supplied
separately from those rules. Return improving (COST VOICE1 ... VOICEN)
records. The C search configuration uses INTEGER-BOUNDS T,
TERMINAL-BOUND :MAX-PENALTY and CROSSING-POLICY :STORED."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Invalid terminal-bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Invalid crossing policy: ~S" crossing-policy))
  (let* ((num-parts (length starts))
         (state (make-many-voice-state
                 cantus starts mode species :seed seed :patterns patterns
                 :inner-durations inner-durations
                 :inner-half-notes inner-half-notes))
         (branch-limit (or branch-limit
                           (* 50 (- 6 num-parts) (- 6 species))))
         (total-time (aref state 2))
         (best 1000000)
         (max-penalty 1000000)
         (ratio (- 1 (/ (* species num-parts) 100)))
         (branches 0)
         (local-costs (when with-search-costs
                        (loop for voice from 1 to num-parts
                              collect (make-array (total-notes state voice)
                                                  :initial-element 0))))
         (results nil))
    (unless (and (integerp branch-limit) (plusp branch-limit))
      (error "Branch limit must be a positive integer: ~S" branch-limit))
    (labels ((bound (value)
               (if integer-bounds (truncate value) value))
             (save-result (cost)
               ;; Save the evaluated candidates before SaveResults changes
               ;; cadential pitches in the working state.
               (let ((raw (when with-search-costs
                            (many-voice-lines state num-parts)))
                     (costs (when with-search-costs
                              (loop for vector in local-costs
                                    collect (coerce vector 'list)))))
                 (when with-search-costs
                   (unless (= cost (reduce #'+ (mapcar (lambda (line)
                                                      (reduce #'+ line))
                                                    costs)))
                     (error "Search cost mismatch")))
                 (many-voice-cadence-ficta state num-parts)
                 (setf best cost
                       max-penalty (bound (min max-penalty (* ratio cost))))
                 (let* ((lines (many-voice-lines state num-parts))
                        (record (append (cons cost lines)
                                        (when with-search-costs
                                          (list :search-local-costs
                                                (cons nil costs)
                                                :raw-voices raw
                                                :ficta-changes
                                                (loop for before in raw
                                                      for after in lines
                                                      for voice from 1
                                                      append (loop for a in before
                                                                   for b in after
                                                                   for note from 1
                                                                   unless (= a b)
                                                                     collect (list voice note a b))))))))
                   (push record results)
                   (when stream
                     (format stream "~% [~D]~%" cost)
                     (dolist (line lines)
                       (format stream "~{~D ~}~%" line))))))
             (visit (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time
                          (many-voice-next-onset state time num-parts))
                        (notes (and next-time
                                    (many-voice-notes-at state next-time
                                                         num-parts))))
                   (when notes
                     (loop for tail on (many-voice-candidates
                                        state notes accumulated best
                                        num-parts species crossing-policy
                                        with-search-costs)
                           for option = (car tail)
                           for cost = (+ accumulated (first option))
                           do (when (>= cost
                                        (if (and (= next-time total-time)
                                                 (eq terminal-bound :best-fit))
                                            best max-penalty))
                                (return))
                              (dolist (choice (second option))
                                (let* ((voice (car choice))
                                       (note (cdr (assoc voice notes)))
                                       (previous (pitch-at state
                                                           (1- note) voice)))
                                  (set-pitch state note voice
                                             (+ previous
                                                (aref *search-intervals*
                                                      (cdr choice))))))
                              (when with-search-costs
                                (loop for choice in (second option)
                                      for local in (third option)
                                      for voice = (car choice)
                                      for note = (cdr (assoc voice notes))
                                      do (setf (aref (nth (1- voice)
                                                           local-costs)
                                                     (1- note))
                                               local)))
                              (if (< next-time total-time)
                                  (visit next-time cost)
                                  (save-result cost))
                              ;; C resets MaxPenalty only once it has
                              ;; advanced to another finite root candidate.
                              (when (and (zerop time) (cdr tail))
                                (setf max-penalty
                                      (bound (* ratio best))))))))))
      (visit 0 0)
      (nreverse results))))

(export 'generate-many-voice-counterpoint :fux)
