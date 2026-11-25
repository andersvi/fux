;;;; counterpoint-rhythm: FUX implementation.
(in-package :fux)

(defun counterpoint-rhythm-state (cantus starts species mode seed patterns
                                  &key inner-durations inner-half-notes)
  "Build only the rhythmic state; no pitch search or scoring is performed."
  (cond
    ((= (length starts) 1)
      (let ((start (first starts)))
        (case species
          (1 (make-first-species-cantus cantus start mode))
          (2 (make-second-species-cantus cantus start mode))
          (3 (make-third-species-cantus cantus start mode))
          (4 (make-fourth-species-cantus cantus start mode))
          (5 (make-fifth-species-cantus cantus start mode
                                        :seed seed :patterns patterns)))))
    ((= (length starts) 2)
     (case species
        (1 (make-poly-first-species-state cantus starts mode))
        (2 (make-mixed-second-species-state cantus starts mode))
        (otherwise (make-mixed-advanced-state cantus starts mode species
                                              :seed seed :patterns patterns))))
    (t (make-many-voice-state cantus starts mode species
                              :seed seed :patterns patterns
                              :inner-durations inner-durations
                              :inner-half-notes inner-half-notes))))

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
                                         seed patterns
                                         &key inner-durations inner-half-notes)
  "Attach state-derived rhythm to existing improvement records.
The original records and search remain unchanged. Every record includes
cantus as voice 0, then the generated voices in their original order."
  (let* ((state (counterpoint-rhythm-state
                 cantus starts species mode seed patterns
                 :inner-durations inner-durations
                 :inner-half-notes inner-half-notes))
         (species-by-voice
           (append (list 0)
                   (make-list (1- (length starts)) :initial-element 1)
                   (list species))))
    (loop for record in records
          collect (progn
                    (unless (>= (length record) (1+ (length starts)))
                      (error "Expected ~D generated voices in record ~S"
                             (length starts) record))
                    (append (list :penalty (first record)
                        :time-unit :eighth-note
                        :voices
                        (loop for pitches in (cons cantus
                                                   (subseq record 1
                                                           (1+ (length starts))))
                              for voice from 0
                              for voice-species in species-by-voice
                              collect (counterpoint-voice-rhythm
                                       state pitches voice voice-species)))
                            (nthcdr (1+ (length starts)) record))))))

(export '(counterpoint-records-with-rhythm) :fux)
