;;;;
;;;; Load tests against either src/ or the generated dist/fux.lisp.
;;;;

(let ((root (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (name '("embedded-tests" "tests" "score-contract-audit"
                  "save-contract-tests" "post-ficta-audit"
                  "mixed-advanced-tests" "many-voice-tests"
                  "six-voice-positive-test" "search-cost-tests"
                  "many-voice-search-cost-tests" "fux-rhythm-suite"
                  "figure-16-8" "figure-16-8-c-audit" "run-suite"))
    (load (compile-file (merge-pathnames (format nil "tests/~A.lisp" name) root)))))
