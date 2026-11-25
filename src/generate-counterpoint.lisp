;;;; generate-counterpoint: FUX implementation.
(in-package :fux)

(defun generate-counterpoint (cantus &key (species 1) starts
                                      (mode :dorian) (stream nil)
                                      (reference nil)
                                      (seed 1) patterns branch-limit with-rhythm
                                      with-search-costs
                                      inner-durations inner-half-notes)
  "Generate counterpoint for CANTUS using one to five starting pitches.
SPECIES is 1..5. All but the final generated voice use first-species
rules. INNER-DURATIONS or INNER-HALF-NOTES select earlier voices' rhythms
when there are three or more generated voices; they do not alter those
voices' rules. Return improving (COST VOICE...) records. REFERENCE selects
the tested C search settings. SEED and PATTERNS apply to species 5.
WITH-RHYTHM T adds onsets, durations and inferred ligatures."
  (unless (and (listp cantus) (>= (length cantus) 2)
               (every #'integerp cantus))
    (error "CANTUS must be a list of at least two integer pitches: ~S" cantus))
  (unless (member species '(1 2 3 4 5))
    (error "SPECIES must be 1 through 5: ~S" species))
  (unless (and (listp starts) (<= 1 (length starts) 5)
               (every #'integerp starts))
    (error "STARTS must contain one to five integer pitches: ~S" starts))
  (unless (assoc mode *modes*)
    (error "Unknown mode: ~S" mode))
  (when (and (/= species 5) patterns)
    (error "PATTERNS applies to fifth species only"))
  (when (and with-search-costs
             (not (or (and (= species 5) (= (length starts) 1))
                      (>= (length starts) 3))))
    (error "WITH-SEARCH-COSTS supports one fifth-species voice or 3..5 generated voices"))
  (when (and (< (length starts) 3)
             (or inner-durations inner-half-notes))
    (error "Inner-voice rhythms require at least three generated voices"))
  (when (and branch-limit (or (not (integerp branch-limit))
                             (not (plusp branch-limit))))
    (error "BRANCH-LIMIT must be a positive integer: ~S" branch-limit))
  (let* ((common (list :mode mode :stream stream
                       :integer-bounds (not (null reference))
                       :terminal-bound (if reference :max-penalty :best-fit)))
         (crossing (list :crossing-policy
                         (if reference :stored :candidate)))
         (rhythm (if (= species 5)
                     (list :seed seed :patterns patterns)
                     nil))
         (limits (if branch-limit (list :branch-limit branch-limit) nil))
         (args (append common rhythm limits)))
    (let ((records
            (cond
              ((= (length starts) 1)
               (apply (ecase species
                        (1 #'generate-first-species)
                        (2 #'generate-second-species)
                        (3 #'generate-third-species)
                        (4 #'generate-fourth-species)
                        (5 #'generate-fifth-species))
                      cantus (first starts)
                      (append args crossing
                              (when with-search-costs
                                (list :with-search-costs t)))))
              ((>= (length starts) 3)
               (apply #'generate-many-voice-counterpoint
                      cantus starts species
                      (append args crossing
                              (list :inner-durations inner-durations
                                    :inner-half-notes inner-half-notes
                                    :with-search-costs with-search-costs))))
              ((= species 1)
               ;; Historical poly search does not take CROSSING-POLICY.
               (apply #'generate-poly-first-species cantus starts args))
              ((= species 2)
               (apply #'generate-mixed-second-species cantus starts
                      (append args crossing)))
              (t
               (apply #'generate-mixed-species cantus starts species
                      (append args crossing))))))
      (if with-rhythm
          (counterpoint-records-with-rhythm records cantus starts species
                                            mode seed patterns
                                            :inner-durations inner-durations
                                            :inner-half-notes inner-half-notes)
          records))))

(defun best-counterpoint (records)
  "Return the final improvement record, or NIL when no result was found."
  (car (last records)))

(export '(generate-counterpoint best-counterpoint) :fux)
