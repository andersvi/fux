;;;; Load editable FUX modules from this directory, independent of cwd.
(let ((root (make-pathname :name nil :type nil :defaults *load-truename*)))
  (dolist (name '("core" "first-species" "search"
                  "poly-first-species" "poly-search"
                  "second-species" "second-search"
                  "third-species" "third-search"
                  "fourth-species" "fourth-search"
                  "fifth-species" "fifth-search"
                  "mixed-second-species" "mixed-search"
                  "mixed-voice-rules" "mixed-third-species"
                  "mixed-fourth-species" "mixed-fifth-species"
                  "mixed-advanced-search" "many-voice-search"
                  "counterpoint-rhythm" "generate-counterpoint" "fux-cmn"))
    (load (merge-pathnames (format nil "src/~A.lisp" name) root))))
