;;;; Om::Compile&Load previous verified sources and the new species 2 implementation.
(defun compile-and-load (file)
  (progn
    (compile-file file)
    (load file)))


(compile-and-load "fux.lisp")
(compile-and-load "first-species.lisp")
(compile-and-load "search.lisp")
(compile-and-load "tests.lisp")
(compile-and-load "poly-first-species.lisp")
(compile-and-load "poly-search.lisp")
(compile-and-load "second-species.lisp")
(compile-and-load "second-search.lisp")
