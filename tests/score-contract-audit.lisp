;;;; Audit the contract between a scored candidate and a saved line.
;;;; Load after tests.lisp. This file does not change generation or penalties.
(in-package :fux)

(defun audit-first-species-score-contract (&key (reference nil) (stream *standard-output*))
  "Return discrepancies for EXS01..10. No discrepancy is silently excused.
PRELOADED scores each candidate with the complete line in the work array.
SEQUENTIAL scores it with only the prefix present, as during generation.
Both explicitly use the candidate-dependent crossing rule. SEARCH is the
historically accumulated cost, which may differ after SaveResults ficta."
  (let ((discrepancies nil)
        (checked 0))
    (dolist (example *exs-fixtures*)
      (let* ((number (getf example :number))
             (records (run-exs number :reference reference :stream nil
                               :crossing-policy :candidate)))
        (dolist (record records)
          (incf checked)
          (multiple-value-bind (preloaded output-a local-a)
              (score-exs-line number (second record)
                              :crossing-policy :candidate)
            (multiple-value-bind (sequential output-b local-b)
                (score-exs-line number (second record) :sequential t
                                :crossing-policy :candidate)
              (unless (and (equal output-a output-b)
                           (equal local-a local-b))
                (push (list :example number :line (second record)
                            :preloaded preloaded :sequential sequential
                            :preloaded-locals local-a :sequential-locals local-b)
                      discrepancies))
              (when (or (/= (first record) preloaded)
                        (/= preloaded sequential))
                (format stream
                        "~&EXS~2,'0D search=~D preloaded=~D sequential=~D~%"
                        number (first record) preloaded sequential)))))))
    (format stream "~&Checked ~D saved lines; ~D prefix-dependence discrepancies.~%"
            checked (length discrepancies))
    (nreverse discrepancies)))

(defun test-first-species-candidate-independence ()
  "A candidate's local cost must not depend on the stale current work cell."
  (let* ((state (make-first-species-cantus
                 '(48 52 58 53 57 48) 60 :dorian))
         (base (aref state 0))
         (prefix '(60 60 55 60 53))
         (note 6)
         (candidate (- 60 base)))
    (loop for absolute in prefix for i from 1
          do (set-pitch state i 1 (- absolute base)))
    (let ((costs (loop for stale in '(0 48 60 72)
                       do (set-pitch state note 1 (- stale base))
                       collect (check-first-species
                                state note candidate :limit 10000000
                                :crossing-policy :candidate))))
      (assert (every (lambda (cost) (= cost (first costs))) (rest costs))
              () "Candidate evaluation depends on a stale current cell: ~S" costs)
      t)))
