;;;; Literal and ficta-aware rechecks of the ten first-species EXS cases.
;;;; Load after load-species.lisp and tests.lisp.
(in-package :fux)

(defun audit-first-species-post-ficta (&key (reference t)
                                          (crossing-policy :stored)
                                          (save-state-policy :historic)
                                          (stream *standard-output*))
  "Find saved lines with a million-penalty note after proven ficta is exempted.
REFERENCE selects C-style bounds; CROSSING-POLICY selects candidate versus
historic stored-cell crossing. SAVE-STATE-POLICY selects the work-array behavior. Return incident
records and the number of saved lines examined. This is a rule diagnostic,
not a complete judgment of Fux validity."
  (let ((incidents nil)
        (checked 0))
    (dolist (example *exs-fixtures*)
      (let* ((number (getf example :number))
             (cantus (getf example :cantus))
             (mode (getf example :mode))
             (records (generate-first-species
                       cantus (getf example :start)
                       :mode mode :stream nil
                       :integer-bounds reference
                       :terminal-bound (if reference
                                           :max-penalty :best-fit)
                       :crossing-policy crossing-policy
                       :save-state-policy save-state-policy
                       :result-format :with-ficta)))
        (loop for record in records for save-number from 1
              do (incf checked)
                 (multiple-value-bind (total locals exempted)
                     (score-first-species-post-ficta
                      cantus (getf record :raw-pitches)
                      (getf record :notated-pitches)
                      :mode mode
                      :provenance (getf record :ficta-provenance))
                   (let ((hard-notes
                           (loop for local in locals for note from 1
                                 when (>= local (penalty :infinity))
                                   collect (list note local))))
                     (when hard-notes
                       (let ((incident
                               (list :example number :save save-number
                                     :save-state-policy save-state-policy
                                     :crossing-policy crossing-policy
                                     :search-penalty
                                     (getf record :search-penalty)
                                     :literal-penalty
                                     (getf record :notated-penalty)
                                     :post-ficta-penalty total
                                     :exempted-notes exempted
                                     :hard-notes hard-notes
                                     :notated-pitches
                                     (getf record :notated-pitches))))
                         (push incident incidents)
                         (when stream
                           (format stream
                                   "~&EXS~2,'0D save=~D search=~D literal=~D post=~D exempt=~S hard=~S~%"
                                   number save-number
                                   (getf record :search-penalty)
                                   (getf record :notated-penalty)
                                   total exempted hard-notes)))))))))
    (when stream
      (format stream "~&Crossing ~S, save ~S: checked ~D saved lines; ~D still have hard rule penalties.~%"
              crossing-policy save-state-policy checked (length incidents)))
    (values (nreverse incidents) checked)))

(defun audit-first-species-raw-score (&key (reference t)
                                          (crossing-policy :candidate)
                                          (save-state-policy :restore)
                                          (stream *standard-output*))
  "Compare each search cost with a fresh candidate-consistent RAW line score.
The default combines candidate crossing and restoration after SaveResults.
Return discrepancies and checked count. A mismatch isolates another search
state/cost issue; an equality says nothing yet about musical validity of
post-ficta notation."
  (let ((discrepancies nil)
        (checked 0))
    (dolist (example *exs-fixtures*)
      (let* ((number (getf example :number))
             (cantus (getf example :cantus))
             (mode (getf example :mode))
             (records (generate-first-species
                       cantus (getf example :start)
                       :mode mode :stream nil
                       :integer-bounds reference
                       :terminal-bound (if reference
                                           :max-penalty :best-fit)
                       :crossing-policy crossing-policy
                       :save-state-policy save-state-policy
                       :result-format :with-ficta)))
        (loop for record in records for save-number from 1
              do (incf checked)
                 (multiple-value-bind (fresh locals)
                     (score-first-species-line
                      cantus (getf record :raw-pitches) :mode mode)
                   (unless (= fresh (getf record :search-penalty))
                     (let ((discrepancy
                             (list :example number :save save-number
                                   :search-penalty
                                   (getf record :search-penalty)
                                   :raw-penalty fresh
                                   :raw-local-penalties locals
                                   :raw-pitches (getf record :raw-pitches))))
                       (push discrepancy discrepancies)
                       (when stream
                         (format stream
                                 "~&EXS~2,'0D save=~D search=~D raw=~D~%"
                                 number save-number
                                 (getf record :search-penalty) fresh))))))))
    (when stream
      (format stream "~&Crossing ~S, save ~S: checked ~D raw lines; ~D score discrepancies.~%"
              crossing-policy save-state-policy checked
              (length discrepancies)))
    (values (nreverse discrepancies) checked)))
