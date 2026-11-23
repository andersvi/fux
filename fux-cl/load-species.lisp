

(dolist (file '("fux.lisp"
		"first-species.lisp"
		"search.lisp"
                "tests.lisp"
		"poly-first-species.lisp"
		"poly-search.lisp"
                "second-species.lisp"
		"second-search.lisp"
                "third-species.lisp"
		"third-search.lisp"
                "fourth-species.lisp"
		"fourth-search.lisp"
                "fifth-species.lisp"
		"fifth-search.lisp"
		"mixed-second-species.lisp"
		"mixed-search.lisp"
		))
  (load (compile-file file)))

(dolist (file '("mixed-advanced-search.lisp"
		"mixed-advanced-tests.lisp"
		"mixed-fifth-species.lisp"
		"mixed-fourth-species.lisp"
		"mixed-third-species.lisp"
		"mixed-voice-rules.lisp"
		"counterpoint-rhythm.lisp"
		"generate-counterpoint.lisp"))

  (load (compile-file file)))
