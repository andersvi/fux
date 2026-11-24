;;;; FUX-CL: interactive rhythm / tie demos (evaluate forms individually).
;;;; After (load "load-species.lisp") and loading fux-rhythm-suite.lisp.
;;;; Durations/onsets use eighth-note units: 8=whole, 4=half, 2=quarter, 1=eighth.

;; Check all five species with 1 or 2 generated voices, including tie metadata.
(fux:run-fux-rhythm-suite)

;; Recheck under the CL-default search policy (may generate more improvements).
(fux:run-fux-rhythm-suite :reference nil)

;; First species: one note against each cantus note, no ties.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 1 :starts '(57) :with-rhythm t))

(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50 50 53 55 52 50 50 53 55 52 50 47)
                            :species 1 :starts '(57) :with-rhythm t))


;; First species, three voices total: cantus + two whole-note voices.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 1 :starts '(57 62) :with-rhythm t))

;; Second species: two half notes per bar, final whole note.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 2 :starts '(57) :with-rhythm t))

;; Mixed 1+2: first added voice uses whole notes, second uses half notes.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 2 :starts '(57 62) :with-rhythm t))

;; Third species: four quarters per bar, final whole note.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 3 :starts '(57) :with-rhythm t))

;; Mixed 1+3: simultaneous events are scheduled at quarter-note times.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 3 :starts '(57 62) :with-rhythm t))

;; Fourth species: syncopated half notes, inferred ties on matching barline pitches.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 4 :starts '(57) :with-rhythm t))

;; (:PENALTY 57 :TIME-UNIT :EIGHTH-NOTE
;;  :VOICES ((:SPECIES 0
;; 	   :PITCHES (50 53 55 52 50)
;; 	   :ONSETS (0 8 16 24 32)
;; 	   :DURATIONS (8 8 8 8 8)
;; 	   :TIES NIL)
;; 	  (:SPECIES 4
;; 	   :PITCHES (57 59 59 57 59 62 62 61 62)
;; 	   :ONSETS (0 4 8 12 16 20 24 28 32)
;; 	   :DURATIONS (4 4 4 4 4 4 4 4 8)
;; 	   :TIES ((2 . 3) (6 . 7)))
;; 	  ))

;; Mixed 1+4: look for tie pairs in the final voice, if this solution has any.
(let* ((best (fux:best-counterpoint
              (fux:generate-counterpoint '(50 53 55 52 50)
                                         :species 4 :starts '(57 62)
                                         :with-rhythm t)))
       (last-voice (car (last (getf best :voices)))))
  (list :penalty (getf best :penalty)
        :pitches (getf last-voice :pitches)
        :ties (getf last-voice :ties)))

;; (:PENALTY 61 :PITCHES (62 65 62 60 60 59 59 57 57) :TIES ((4 . 5) (6 . 7) (8 . 9)))

;; Fifth species: explicit alternating rhythm figures 3,4,7,9.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 5 :starts '(57)
                            :patterns '(3 4 7 9) :with-rhythm t))

;; (:PENALTY 36 :TIME-UNIT :EIGHTH-NOTE
;;  :VOICES ((:SPECIES 0
;; 	   :PITCHES (50 53 55 52 50)
;; 	   :ONSETS (0 8 16 24 32)
;; 	   :DURATIONS (8 8 8 8 8)
;; 	   :TIES NIL)
;; 	  (:SPECIES 5
;; 	   :PITCHES (57 53 55 57 57 62 60 60 59 64 62 61 59 61 59 61 62)
;; 	   :ONSETS (0 2 4 6 8 10 12 16 20 22 23 24 26 28 30 31 32)
;; 	   :DURATIONS (2 2 2 2 2 2 4 4 2 1 1 2 2 2 1 1 8)
;; 	   :TIES ((4 . 5) (7 . 8)))
;; 	  ))

;; Mixed 1+5: same rhythm figures with one extra whole-note voice.
(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50)
                            :species 5 :starts '(57 62)
                            :patterns '(3 4 7 9) :with-rhythm t))

(fux:best-counterpoint
 (fux:generate-counterpoint '(50 53 55 52 50 59)
                            :species 5 :starts '(57 62)
                            :patterns '(3 4 7 9 4) :with-rhythm t))

;; simple demo 3rd species

(fux:best-counterpoint
 (fux:generate-counterpoint
  '(50 53 55 52 50)
  :species 3
  :starts '(57)
  :mode :dorian
  :with-rhythm t))

;; multi-voice 5th species

(fux:best-counterpoint
 (fux:generate-counterpoint
  '(50 53 55 52 50)
  :species 5
  :starts '(57 62)
  :mode :dorian
  :patterns '(3 4 7 9)
  :with-rhythm t))

;; Ønsket API – ennå ikke implementert
(fux:generate-counterpoint
 '(50 53 55 52 50)
 :species 5
 :starts '(57 62 65))




;; durations and onsets only
(let* ((best (fux:best-counterpoint
              (fux:generate-counterpoint '(50 53 55 52 50)
                                         :species 5 :starts '(57 62)
                                         :patterns '(3 4 7 9)
                                         :with-rhythm t)))
       (voice (car (last (getf best :voices)))))
  (list :onsets (getf voice :onsets)
        :durations (getf voice :durations)
        :ties (getf voice :ties)))

;; Positive tie example: same pitch repeated across the barline at onset 8.
(fux::counterpoint-ties '(60 62 62 64) '(0 4 8 10) '(4 4 2 2) 5)
;; => ((2 . 3))

;; Negative example: repeated pitch inside a bar is not inferred as tied.
(fux::counterpoint-ties '(60 60 62) '(0 2 4) '(2 2 8) 5)
;; => NIL

;; First / second / third species do not infer ties from pitch repetitions.
(fux::counterpoint-ties '(60 60 62) '(0 4 8) '(4 4 8) 2)
;; => NIL


;; (fux:generate-counterpoint '(50 53 55 52 50)
;;                            :species 5 :starts '(57 62)
;;                            :patterns '(3 4 7 9)
;;                            :with-rhythm t)

