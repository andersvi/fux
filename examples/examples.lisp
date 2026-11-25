(in-package :fux)


;; Interactive examples (evaluate individual forms after loading fux.lisp):

(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :starts '(57) :species 5
                            :patterns '(3 4 7 9)
                            :with-rhythm t :with-search-costs t))

;; Six voices in Lydian mode, with per-note search costs.
(defparameter *lydian-example*
  (fux:best-counterpoint
   (fux:generate-counterpoint '(53 55 57 59 53 52 50 52 53)
                              :starts '(41 48 57 60 65)
                              :species 5 :mode :lydian
                              :patterns '(2 3 4 5 6 7 8 9)
                              :inner-half-notes t :reference t
                              :with-rhythm t :with-search-costs t)))

(with-open-file (out "lydian.cmn" :direction :output :if-exists :supersede)
  (fux:write-counterpoint-cmn *lydian-example* :stream out
                              :mode :lydian :score-notes :search
                              :staff-order '(5 4 3 0 2 1)))

;;; #'scratch-cmn from scratch-cmn.lisp, direct output to cmn files

(scratch-cmn
 (fux:generate-counterpoint
  '(50 53 55 52 50)
  :starts '(57) :species 5
  :patterns '(3 4 7 9)
  :reference t :with-rhythm t
  :with-search-costs t)
 "scratch-species5")


(scratch-cmn (fux:generate-counterpoint
	      '(50 53 55 52 50)
	      :starts '(57) :species 2
	      :reference t :with-rhythm t
	      )
	     "scratch-species2")

;; Én generert stemme, femte art med fast rytme:

(scratch-cmn (fux:generate-counterpoint
	      '(50 53 55 52 50)
	      :starts '(57) :species 5
	      :patterns '(3 4 7 9)
	      :with-search-costs t
	      :reference t :with-rhythm t)
	     "scratch-species5")


;; Tre stemmer totalt, blandet første og fjerde art:

(scratch-cmn (fux:generate-counterpoint
	      '(50 53 55 52 50)
	      :starts '(57 62) :species 4
	      :reference t :with-rhythm t
	      )
	     "scratch-mixed-1+4")

;; Fire stemmer totalt, siste stemme i femte art:

(scratch-cmn (fux:generate-counterpoint
	      '(50 53 55 52 50)
	      :starts '(38 57 62) :species 5
	      :patterns '(3 4 7 9)
	      :reference t :with-rhythm t
	      :inner-half-notes t
	      :with-search-costs t
	      )
	     "scratch-four-voices"
	     :staff-order '(3 2 0 1))

;; Fem stemmer totalt, med halvnoter i innstemmene:

(scratch-cmn (fux:generate-counterpoint
	      '(50 53 55 52 50)
	      :starts '(38 45 57 62) :species 5
	      :patterns '(3 4 7 9)
	      :with-search-costs t
	      :inner-half-notes t
	      :reference t :with-rhythm t)
	     "scratch-five-voices"
	     :staff-order '(4 3 0 2 1))


;; 6 voices



(scratch-cmn (fux:generate-counterpoint
	      '(50 53 50 53 50)
	      :starts '(38 45 53 57 62)
	      :species 5
	      :patterns '(3 4 7 9)
	      :mode :dorian
	      :reference t
	      :with-search-costs t
	      :with-rhythm t)
	     "scratch-six-voices-florid"
	     :staff-order '(5 4 3 0 2 1))


(scratch-cmn (fux:generate-counterpoint '(53 55 57 59 53)   ; F4 G4 A4 B4 F4
					:starts '(41 48 57 60 65) ; F3 C4 A4 C5 F5
					:species 5
					:mode :lydian
					:patterns '(3 4 7 9)
					:inner-half-notes t
					:reference t
					:with-search-costs t
					:with-rhythm t)
	     "scratch-six-voices-lydian"
	     :mode :lydian
	     :staff-order '(5 4 3 0 2 1))



(scratch-cmn (fux:generate-counterpoint '(53 59 57 53 52 53)
					:starts '(41 48 57 60 65) ; F3 C4 A4 C5 F5
					:species 5
					:patterns '(3 4 5 9 2)
					:inner-half-notes t
					:with-search-costs t
					:mode :lydian
					:reference t
					:with-rhythm t)
	     "scratch-six-voices-mixed-lydian"
	     :mode :lydian
	     :staff-order '(5 4 3 2 1 0))


;; longer 6 voice

;; (print fux::*fifth-rhythm-patterns*)
;; #(NIL (4 4) (4 2 2) (2 2 2 2) (2 2 4) (2 1 1 4) (2 1 1 2 2) (4 2 1 1) (2 1 1 2 1 1) (2 2 2 1 1))



(scratch-cmn (fux:generate-counterpoint '(53 55 57 59 53 52 50 52 53)   ; F4 G4 A4 B4 F4 E4 D4 E4 F4
					:starts '(41 48 57 60 65 ) ; F3 C4 A4 C5 F5
					:species 5
					:mode :lydian
					:patterns '(2 3 4 5 6 7 8 9)
					:inner-half-notes t
					:reference t
					:with-search-costs t
					:with-rhythm t)
	     "scratch-six-voices-mixed-lydian-longer"
	     :mode :lydian
	     :staff-order '(5 4 3 0 2 1))



|#
