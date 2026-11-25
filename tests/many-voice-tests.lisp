;;;; C reference cases for 3/4 generated voices (four/five total).
;;;; Extracted from the archived C driver output by multi-fixtures/extract.py
;;;; and extract-all.py. C changed only the historical SaveIndx allocation
;;;; safety margin and selected fifth-species patterns (3 4 7 9); the
;;;; inner-half-notes experiment also changes two earlier voice durations.
;;;; The pitches and all improvement costs are actual C output.
(in-package :fux)

(defparameter *many-voice-c-fixtures*
  '(
    (:label :poly-4-voices
     :cantus (50 53 55 52 50)
     :starts (38 57 62)
     :patterns (3 4 7 9)
     :inner-half-notes nil
     :penalty 843
     :voices (
       (:species 1
        :pitches (38 45 43 43 38)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 1
        :pitches (57 57 59 47 45)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 5
        :pitches (62 65 64 62 62 60 62 64 52 64 62 64 67 62 59 61 62)
        :onsets (0 2 4 6 8 10 12 16 20 22 23 24 26 28 30 31 32)
        :durations (2 2 2 2 2 2 4 4 2 1 1 2 2 2 1 1 8))
     ))
    (:label :poly-5-voices
     :cantus (50 53 55 52 50)
     :starts (38 45 57 62)
     :patterns (3 4 7 9)
     :inner-half-notes nil
     :penalty 1345
     :voices (
       (:species 1
        :pitches (38 45 43 43 38)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 1
        :pitches (45 48 47 55 57)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 1
        :pitches (57 57 52 47 45)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 5
        :pitches (62 59 60 62 62 60 62 64 59 67 69 71 64 62 59 61 62)
        :onsets (0 2 4 6 8 10 12 16 20 22 23 24 26 28 30 31 32)
        :durations (2 2 2 2 2 2 4 4 2 1 1 2 2 2 1 1 8))
     ))
    (:label :five-voices-half-rhythm
     :cantus (50 53 55 52 50)
     :starts (38 45 57 62)
     :patterns (3 4 7 9)
     :inner-half-notes t
     :penalty 2525
     :voices (
       (:species 1
        :pitches (38 33 35 31 26)
        :onsets (0 8 16 24 32)
        :durations (8 8 8 8 8))
       (:species 1
        :pitches (45 41 45 41 38 50 47 40 45)
        :onsets (0 4 8 12 16 20 24 28 32)
        :durations (4 4 4 4 4 4 4 4 8))
       (:species 1
        :pitches (57 62 60 48 50 43 43 47 45)
        :onsets (0 4 8 12 16 20 24 28 32)
        :durations (4 4 4 4 4 4 4 4 8))
       (:species 5
        :pitches (62 60 57 59 59 57 60 60 59 67 69 71 69 67 71 73 74)
        :onsets (0 2 4 6 8 10 12 16 20 22 23 24 26 28 30 31 32)
        :durations (2 2 2 2 2 2 4 4 2 1 1 2 2 2 1 1 8))
     ))
  ))

(defparameter *many-voice-c-improvements*
  '(
    (:label :poly-4-voices :records (
       (2143 (38 38 35 31 26) (57 50 50 47 45) (62 64 65 67 67 65 64 62 67 71 72 74 67 69 71 73 74))
       (1548 (38 38 35 31 26) (57 50 50 47 45) (62 64 65 67 67 65 64 62 67 71 72 74 67 64 59 61 62))
       (1303 (38 45 48 52 57) (57 57 52 55 57) (62 65 64 62 62 60 59 57 64 57 59 61 59 61 59 61 62))
       (843 (38 45 43 43 38) (57 57 59 47 45) (62 65 64 62 62 60 62 64 52 64 62 64 67 62 59 61 62))
     ))
    (:label :poly-5-voices :records (
       (2699 (38 38 31 31 26) (45 47 47 43 45) (57 50 52 47 45) (62 60 59 60 62 64 65 65 64 67 69 71 69 67 71 73 74))
       (1345 (38 45 43 43 38) (45 48 47 55 57) (57 57 52 47 45) (62 59 60 62 62 60 62 64 59 67 69 71 64 62 59 61 62))
     ))
    (:label :five-voices-half-rhythm :records (
       (2525 (38 33 35 31 26) (45 41 45 41 38 50 47 40 45) (57 62 60 48 50 43 43 47 45) (62 60 57 59 59 57 60 60 59 67 69 71 69 67 71 73 74))
     ))
  ))

;;;; C experiment: all improvements, species 1–4, 3 or 4 generated voices.
(defparameter *many-voice-c-species1-4*
  '(
    (:label :4-voices-species-1 :species 1 :starts (38 57 62) :records (
       (84 (38 45 43 45 50) (57 57 59 61 62) (62 60 64 57 57))
       (70 (38 45 43 45 50) (57 57 64 61 62) (62 60 59 57 57))
      ))
    (:label :4-voices-species-2 :species 2 :starts (38 57 62) :records (
       (240 (38 38 40 37 38) (57 50 48 52 45) (62 60 59 57 60 64 69 57 57))
       (222 (38 38 40 37 38) (57 50 48 52 45) (62 60 59 57 60 67 69 57 57))
       (166 (38 38 40 37 38) (57 50 47 40 45) (62 60 59 57 52 59 57 69 69))
       (145 (38 38 31 33 38) (57 50 47 49 50) (62 60 59 57 59 64 64 69 69))
       (124 (38 45 40 37 38) (57 57 60 64 69) (62 65 60 62 64 67 69 57 57))
      ))
    (:label :4-voices-species-3 :species 3 :starts (38 57 62) :records (
       (337 (38 38 35 37 38) (57 50 50 52 45) (62 64 65 67 69 67 65 64 62 60 59 67 69 57 69 57 57))
       (276 (38 38 35 43 38) (57 50 50 47 45) (62 64 65 67 69 67 65 64 62 64 65 67 55 57 59 61 62))
       (243 (38 45 43 43 38) (57 57 59 64 57) (62 69 67 65 60 65 67 69 64 65 67 64 59 55 59 61 62))
       (193 (38 45 47 52 57) (57 57 50 55 57) (62 69 67 65 60 65 67 69 67 65 64 62 60 64 62 61 62))
      ))
    (:label :4-voices-species-4 :species 4 :starts (38 57 62) :records (
       (135 (38 45 43 45 50) (57 57 59 61 62) (62 65 60 65 65 64 64 69 69))
      ))
    (:label :5-voices-species-1 :species 1 :starts (38 45 57 62) :records (
       (508 (38 45 43 55 50) (45 48 47 55 57) (57 57 52 64 57) (62 60 64 61 62))
       (284 (38 45 40 37 38) (45 48 52 45 45) (57 57 60 57 62) (62 60 60 64 57))
       (238 (38 45 40 37 38) (45 48 43 45 45) (57 57 60 57 62) (62 60 60 64 57))
       (209 (38 33 28 25 26) (45 48 52 40 45) (57 69 67 69 57) (62 60 60 57 62))
       (199 (38 38 35 37 38) (45 47 47 45 45) (57 50 50 57 62) (62 59 62 64 57))
      ))
    (:label :5-voices-species-2 :species 2 :starts (38 45 57 62) :records (
       (562 (38 38 40 52 57) (45 50 48 55 57) (57 65 64 67 62) (62 60 59 57 60 64 59 61 62))
       (471 (38 38 40 52 57) (45 50 48 55 57) (57 65 64 67 62) (62 60 59 57 52 61 59 61 62))
      ))
    (:label :5-voices-species-3 :species 3 :starts (38 45 57 62) :records (
       (835 (38 38 36 31 26) (45 41 48 47 45) (57 50 52 40 45) (62 64 65 67 69 67 65 69 64 65 67 64 67 69 71 73 74))
       (727 (38 45 43 43 38) (45 48 52 55 57) (57 57 59 64 57) (62 69 67 65 65 67 69 65 64 65 67 69 71 67 71 73 74))
       (395 (38 45 43 43 38) (45 48 52 55 57) (57 57 59 64 57) (62 69 67 65 65 67 69 65 64 65 67 69 71 64 59 61 62))
      ))
    (:label :5-voices-species-4 :species 4 :starts (38 45 57 62) :records (
       (1262 (38 45 40 52 57) (45 48 43 55 57) (57 57 60 67 62) (62 65 60 65 72 76 71 73 74))
       (649 (38 45 40 52 57) (45 48 52 55 62) (57 57 55 67 69) (62 65 60 65 65 64 59 61 62))
      ))
  ))

;;;; C max-capacity probes, five generated voices and one cantus.
(defparameter *many-voice-c-six-voices*
  '(
    (:label :6-voices-species-1 :species 1 :starts (38 45 53 57 62) :records (
      ))
    (:label :6-voices-species-5 :species 5 :starts (38 45 53 57 62) :records (
      ))
  ))

(defun run-many-voice-c-tests ()
  "Compare eleven positive C searches and two empty maximum-capacity probes.
The C fixtures are independent captured outputs; no numeric constants
are derived from the CL implementation. Run after all generator modules."
  (dolist (fixture *many-voice-c-fixtures*)
    (let* ((label (getf fixture :label))
           (cantus (getf fixture :cantus))
           (starts (getf fixture :starts))
           (patterns (getf fixture :patterns))
           (half (getf fixture :inner-half-notes))
           (record-set (find label *many-voice-c-improvements*
                             :key (lambda (x) (getf x :label))))
           (args (list :starts starts :species 5 :mode :dorian
                       :patterns patterns :inner-half-notes half
                       :reference t))
           (actual (apply #'generate-counterpoint cantus args))
           (expected (getf record-set :records))
           (rich (apply #'generate-counterpoint
                        cantus (append args '(:with-rhythm t)))))
      (unless (equal actual expected)
        (error "C many-voice improvements differ for ~S: expected ~S, got ~S"
               label expected actual))
      (unless (and (= (length rich) (length expected))
                   (= (getf (car (last rich)) :penalty)
                      (getf fixture :penalty)))
        (error "Improvement/rhythm record count or best penalty differs: ~S"
               label))
      (loop for voice in (rest (getf (car (last rich)) :voices))
            for c-voice in (getf fixture :voices)
            do (dolist (field '(:species :pitches :onsets :durations))
                 (unless (equal (getf voice field) (getf c-voice field))
                   (error "C ~S mismatch in ~S, voice ~D: ~S vs ~S"
                          field label (getf voice :species)
                          (getf voice field) (getf c-voice field)))))
      (format t "~&~A: ~D C improvements; best=~D; pitches and rhythm match.~%"
              label (length expected) (getf fixture :penalty))))
  (dolist (case *many-voice-c-species1-4*)
    (let* ((actual (generate-counterpoint
                    '(50 53 55 52 50)
                    :species (getf case :species)
                    :starts (getf case :starts) :reference t))
           (expected (getf case :records)))
      (unless (equal actual expected)
        (error "C ~S many-voice improvements differ: expected ~S, got ~S"
               (getf case :label) expected actual))
      (format t "~&~A: ~D C improvements; all pitch lines match.~%"
              (getf case :label) (length expected))))
  ;; C has six slots including cantus, but these short six-voice examples
  ;; have no saved lines. They establish only matching empty behavior and
  ;; correct state capacity, not positive six-voice musical parity.
  (dolist (case *many-voice-c-six-voices*)
    (let* ((species (getf case :species))
           (starts (getf case :starts))
           (patterns (and (= species 5) '(3 4 7 9)))
           (state (make-many-voice-state
                   '(50 53 55 52 50) starts :dorian species
                   :patterns patterns))
           (actual (generate-counterpoint
                    '(50 53 55 52 50) :species species
                    :starts starts :patterns patterns :reference t)))
      (unless (equal actual (getf case :records))
        (error "C six-voice empty probe differs: ~S returned ~S"
               (getf case :label) actual))
      (unless (and (= 5 (length starts))
                   (loop for voice from 1 to 4
                         always (= (total-notes state voice) 5))
                   (= (total-notes state 5)
                      (if (= species 5) 17 5)))
        (error "Six-voice state has wrong event counts: ~S"
               (getf case :label)))
      (format t "~&~A: six-voice state built; C and CL save no records.~%"
              (getf case :label))))
  t)

(export 'run-many-voice-c-tests :fux)
