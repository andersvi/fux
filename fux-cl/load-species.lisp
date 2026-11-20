

(dolist (file '("fux.lisp"
                "first-species.lisp"
                "search.lisp"
                "poly-first-species.lisp"
                "poly-search.lisp"
                "second-species.lisp"
                "second-search.lisp"
                "third-species.lisp"
                "third-search.lisp"))
  (load (compile-file file)))
