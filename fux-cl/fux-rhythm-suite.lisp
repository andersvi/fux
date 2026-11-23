;;;; FUX: pitch/rhythm/tie consistency tests for all supported combinations.
;;;; Does not modify Check, candidate ranking, or the search algorithm.
(in-package :fux)

(defun fux-test-expected-durations (bars species patterns)
  "Independent reference rhythm in eighth-note units (whole=8)."
  (let ((whole '(8)))
    (case species
      ((0 1) (make-list bars :initial-element 8))
      ((2 4) (append (make-list (* 2 (1- bars)) :initial-element 4)
                     whole))
      (3 (append (make-list (* 4 (1- bars)) :initial-element 2)
                 whole))
      (5 (unless (and patterns (= (length patterns) (1- bars)))
           (error "Fifth species test needs ~D explicit patterns" (1- bars)))
         (let ((table '((1 4 4) (2 4 2 2) (3 2 2 2 2)
                        (4 2 2 4) (5 2 1 1 4) (6 2 1 1 2 2)
                        (7 4 2 1 1) (8 2 1 1 2 1 1)
                        (9 2 2 2 1 1))))
           (append (loop for p in patterns append
                         (let ((entry (assoc p table)))
                           (unless entry (error "Bad test pattern ~S" p))
                           (copy-list (cdr entry))))
                   whole)))
      (otherwise (error "Unknown species ~S" species)))))

(defun fux-test-onsets-from-durations (durations)
  (loop with time = 0
        for duration in durations
        collect time
        do (incf time duration)))

(defun fux-test-expected-ties (pitches onsets durations species)
  "Independent consistency oracle for the API's *inferred* ties.
This does not establish whether the original search intended a notated tie."
  (when (member species '(4 5))
    (loop for left in pitches
          for right in (rest pitches)
          for start in onsets
          for next in (rest onsets)
          for dur in durations
          for index from 1
          when (and (= left right)
                    (= (+ start dur) next)
                    (= (mod next 8) 0))
            collect (cons index (1+ index)))))

(defun fux-test-voice-record (voice pitches species bars patterns)
  (let* ((actual-pitches (getf voice :pitches))
         (onsets (getf voice :onsets))
         (durations (getf voice :durations))
         (ties (getf voice :ties))
         (expected-durations (fux-test-expected-durations
                              bars species patterns)))
    (unless (= species (getf voice :species))
      (error "Wrong species: ~S, wanted ~S" voice species))
    (unless (equal pitches actual-pitches)
      (error "Pitch mismatch in voice ~S" voice))
    (unless (equal durations expected-durations)
      (error "Duration mismatch for species ~D: ~S vs ~S"
             species durations expected-durations))
    (unless (equal onsets (fux-test-onsets-from-durations durations))
      (error "Onsets not contiguous in ~S" voice))
    (unless (and (every #'plusp durations)
                 (= (reduce #'+ durations) (* 8 bars))
                 (= (car (last durations)) 8))
      (error "Invalid rhythmic coverage: ~S" voice))
    (unless (equal ties (fux-test-expected-ties
                         pitches onsets durations species))
      (error "Tie metadata mismatch: ~S" voice))
    t))

(defun fux-test-rhythm-case (species starts &key (cantus '(50 53 55 52 50))
                                            (mode :dorian) patterns (seed 1)
                                            (reference t))
  "Check *every* improvement record, not only the final result."
  (let* ((opts (list :species species :starts starts :mode mode
                     :reference reference :seed seed :patterns patterns))
         (plain (apply #'generate-counterpoint cantus opts))
         (rich (apply #'generate-counterpoint cantus
                      (append opts '(:with-rhythm t))))
         (species-list (if (= 1 (length starts))
                           (list 0 species)
                           (list 0 1 species)))
         (bars (length cantus)))
    (unless (and plain (= (length plain) (length rich)))
      (error "No matching records for species ~D, starts ~S" species starts))
    (loop for original in plain
          for record in rich
          do (unless (and (eql (getf record :time-unit) :eighth-note)
                          (= (first original) (getf record :penalty)))
               (error "Penalty/unit mismatch: ~S" record))
             (let ((voices (getf record :voices))
                   (expected-pitches (cons cantus (rest original))))
               (unless (= (length voices) (length species-list))
                 (error "Wrong number of voices in ~S" record))
               (loop for voice in voices
                     for pitches in expected-pitches
                     for voice-species in species-list
                     do (fux-test-voice-record voice pitches voice-species
                                               bars (and (= voice-species 5)
                                                         patterns)))))
    (format t "~&OK species ~D, starts ~S: ~D records; pitches, rhythm, inferred ties~%"
            species starts (length rich))
    t))

(defun fux-test-tie-examples ()
  "Check positive and negative boundary cases, independent of generation."
  (let ((half-pitches '(60 62 62 64 64 65 67 69 70))
        (half-onsets '(0 4 8 12 16 20 24 28 32))
        (half-durations '(4 4 4 4 4 4 4 4 8)))
    (unless (equal (counterpoint-ties half-pitches half-onsets
                                      half-durations 4)
                   '((2 . 3) (4 . 5)))
      (error "Fourth-species tie detection failed"))
    (when (counterpoint-ties half-pitches half-onsets half-durations 2)
      (error "Second-species repetition incorrectly tied")))
  (unless (equal (counterpoint-ties '(60 62 62 64) '(0 4 8 10)
                                   '(4 4 2 2) 5)
                 '((2 . 3)))
    (error "Fifth-species barline tie detection failed"))
  (when (counterpoint-ties '(60 60 62) '(0 2 4) '(2 2 8) 5)
    (error "Intra-bar repetition incorrectly tied"))
  (when (counterpoint-ties '(60 60 62) '(0 4 8) '(4 4 8) 3)
    (error "Third-species repetition incorrectly tied"))
  t)

(defun run-fux-rhythm-suite (&key (reference t))
  "10 combinations: species 1..5 x one/two generated voices.
Checks all improvements, pitch identity, onsets, durations and inferred ties.
REFERENCE T selects tested historical C search settings." 
  (fux-test-tie-examples)
  (dolist (species '(1 2 3 4 5))
    (let ((patterns (and (= species 5) '(3 4 7 9))))
      (fux-test-rhythm-case species '(57) :patterns patterns
                            :reference reference)
      (fux-test-rhythm-case species '(57 62) :patterns patterns
                            :reference reference)))
  (format t "~&FUX rhythm suite: 10 combinations + tie edge cases OK.~%")
  t)

(export '(run-fux-rhythm-suite fux-test-rhythm-case
          fux-test-tie-examples) :fux)
