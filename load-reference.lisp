;;;;
;;;; Load the source-based implementation and its regression/source checks.
;;;;
(let ((root (make-pathname :name nil :type nil :defaults *load-truename*)))
  (load (merge-pathnames "load-dev.lisp" root))
  (load (merge-pathnames "load-tests-only.lisp" root)))
