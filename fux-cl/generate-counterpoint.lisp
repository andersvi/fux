;;;; Unified entry point for the FUX-CL generators.
;;;; No musical rules or search behavior are changed here.
(in-package :fux)

(defun generate-counterpoint (cantus &key (species 1) starts
                                      (mode :dorian) (stream nil)
                                      (reference nil)
                                      (seed 1) patterns branch-limit with-rhythm)
  "Generate counterpoint for CANTUS using one or two starting pitches.
SPECIES is 1..5. With two STARTS, voice 1 uses species 1 and voice 2
uses SPECIES. Return the existing improvement records: (COST VOICE...).
REFERENCE selects the tested historical C search configuration. SEED and
PATTERNS apply to species 5 only. BRANCH-LIMIT, when supplied, overrides
the underlying generator's own default. WITH-RHYTHM T returns each
improvement with pitches, onsets, durations and inferred ligatures."
  (unless (and (listp cantus) (>= (length cantus) 2)
               (every #'integerp cantus))
    (error "CANTUS must be a list of at least two integer pitches: ~S" cantus))
  (unless (member species '(1 2 3 4 5))
    (error "SPECIES must be 1 through 5: ~S" species))
  (unless (and (listp starts) (member (length starts) '(1 2))
               (every #'integerp starts))
    (error "STARTS must contain one or two integer pitches: ~S" starts))
  (unless (assoc mode *modes*)
    (error "Unknown mode: ~S" mode))
  (when (and (/= species 5) patterns)
    (error "PATTERNS applies to fifth species only"))
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
                      cantus (first starts) (append args crossing)))
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
                                            mode seed patterns)
          records))))

(defun best-counterpoint (records)
  "Return the final improvement record, or NIL when no result was found."
  (car (last records)))

(defun run-generator-api-tests ()
  "Check that the wrapper dispatches to unchanged underlying generators."
  (let ((cantus '(50 53 55 52 50)))
    (assert (equal (generate-counterpoint cantus :species 1 :starts '(57))
                   (generate-first-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 2 :starts '(57))
                   (generate-second-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 3 :starts '(57))
                   (generate-third-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 4 :starts '(57))
                   (generate-fourth-species cantus 57)))
    (assert (equal (generate-counterpoint cantus :species 5 :starts '(57)
                                          :patterns '(3 4 7 9))
                   (generate-fifth-species cantus 57
                                           :patterns '(3 4 7 9))))
    (assert (equal (generate-counterpoint cantus :species 1
                                          :starts '(57 62) :reference t)
                   (generate-poly-first-species cantus '(57 62)
                                                :integer-bounds t
                                                :terminal-bound :max-penalty)))
    (assert (equal (generate-counterpoint cantus :species 2
                                          :starts '(57 62) :reference t)
                   (generate-mixed-second-species cantus '(57 62)
                                                  :integer-bounds t
                                                  :terminal-bound :max-penalty
                                                  :crossing-policy :stored)))
    (dolist (species '(3 4 5))
      (assert (equal (generate-counterpoint cantus :species species
                                            :starts '(57 62) :reference t)
                     (generate-mixed-species cantus '(57 62) species
                                             :integer-bounds t
                                             :terminal-bound :max-penalty
                                             :crossing-policy :stored))))
    t))

(export '(generate-counterpoint best-counterpoint run-generator-api-tests) :fux)
