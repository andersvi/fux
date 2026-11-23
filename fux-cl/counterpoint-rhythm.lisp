;;;; Rhythm metadata for generated FUX lines. No change to Check or search.
;;;; Time unit follows C Onset/Dur: 8 = whole note, 1 = eighth note.
(in-package :fux)

(defun counterpoint-rhythm-state (cantus starts species mode seed patterns)
  "Build only the rhythmic state; no pitch search or scoring is performed."
  (if (= (length starts) 1)
      (let ((start (first starts)))
        (case species
          (1 (make-first-species-cantus cantus start mode))
          (2 (make-second-species-cantus cantus start mode))
          (3 (make-third-species-cantus cantus start mode))
          (4 (make-fourth-species-cantus cantus start mode))
          (5 (make-fifth-species-cantus cantus start mode
                                        :seed seed :patterns patterns))))
      (case species
        (1 (make-poly-first-species-state cantus starts mode))
        (2 (make-mixed-second-species-state cantus starts mode))
        (otherwise (make-mixed-advanced-state cantus starts mode species
                                              :seed seed :patterns patterns)))))

(defun counterpoint-ties (pitches onsets durations species)
  "List of (FROM . TO) 1-based note indices tied across cantus barlines.
In species 4/5 an equal pitch continued at an onset divisible by 8 is
interpreted as a ligature. Other repeated notes are not automatically tied."
  (when (member species '(4 5))
    (loop for (a b) on pitches
          for (start next-start) on onsets
          for duration in durations
          for i from 1
          while b
          when (and (= a b)
                    (= (+ start duration) next-start)
                    (zerop (mod next-start 8)))
            collect (cons i (1+ i)))))

(defun counterpoint-voice-rhythm (state pitches voice species)
  (let* ((n (total-notes state voice))
         (onsets (loop for i from 1 to n
                       collect (onset-at state i voice)))
         (durations (loop for i from 1 to n
                          collect (duration-at state i voice))))
    (unless (= (length pitches) n)
      (error "Voice ~D has ~D pitches but ~D rhythm events"
             voice (length pitches) n))
    (list :species species
          :pitches pitches
          :onsets onsets
          :durations durations
          :ties (counterpoint-ties pitches onsets durations species))))

(defun counterpoint-records-with-rhythm (records cantus starts species mode
                                         seed patterns)
  "Attach state-derived rhythm to existing improvement records.
The original records and search remain unchanged. Every record includes
cantus as voice 0, then the generated voices in their original order."
  (let* ((state (counterpoint-rhythm-state cantus starts species mode
                                            seed patterns))
         (species-by-voice (if (= (length starts) 1)
                               (list 0 species)
                               (list 0 1 species))))
    (loop for record in records
          collect (list :penalty (first record)
                        :time-unit :eighth-note
                        :voices
                        (loop for pitches in (cons cantus (rest record))
                              for voice from 0
                              for voice-species in species-by-voice
                              collect (counterpoint-voice-rhythm
                                       state pitches voice voice-species))))))

(defun run-generator-rhythm-tests ()
  "Check pitch identity, rhythmic coverage, syncopation and explicit patterns."
  (let ((cantus '(50 53 55 52 50)))
    (dolist (species '(1 2 3 4 5))
      (let* ((records (generate-counterpoint cantus :species species
                                              :starts '(57)
                                              :patterns (and (= species 5)
                                                             '(3 4 7 9))))
             (rich (counterpoint-records-with-rhythm records cantus '(57)
                                                     species :dorian 1
                                                     (and (= species 5)
                                                          '(3 4 7 9)))))
        (assert (= (length records) (length rich)))
        (loop for original in records
              for result in rich
              do (assert (= (first original) (getf result :penalty)))
                 (let* ((voices (getf result :voices))
                        (generated (second voices))
                        (onsets (getf generated :onsets))
                        (durations (getf generated :durations)))
                   (assert (equal (second original)
                                  (getf generated :pitches)))
                   (assert (equal (getf (first voices) :pitches) cantus))
                   (assert (= (reduce #'+ durations) (* 8 (length cantus))))
                   (assert (= (car onsets) 0))
                   (assert (= (car (last durations)) 8))
                   (loop for (a b) on onsets
                         for duration in durations
                         while b do (assert (= (+ a duration) b)))))))
    (let* ((durations (fifth-species-durations 5 :patterns '(3 4 7 9)))
           (state (counterpoint-rhythm-state cantus '(57 62) 5 :dorian
                                             1 '(3 4 7 9))))
      (assert (equal durations
                     (loop for i from 1 to (total-notes state 2)
                           collect (duration-at state i 2))))
      (assert (= (total-notes state 1) 5)))
    (assert (equal (counterpoint-ties '(60 60 62) '(4 8 12) '(4 4 8) 4)
                   '((1 . 2))))
    (assert (null (counterpoint-ties '(60 60 62) '(4 8 12) '(4 4 8) 2)))
    t))

(export '(counterpoint-records-with-rhythm run-generator-rhythm-tests) :fux)
