;;;; Readable study data for Bill Schottstaedt's five-voice example.
;;;; Standalone Common Lisp: no FUX or CMN dependency, and no search is run.
;;;;
;;;; Sources: May 1984 report p. 45 (PDF 49); chapter pp. 212-213, Figure 16.8;
;;;; and the user's fux.cmn notation file. All 104 pitches and durations
;;;; were independently read from the score and match fux.cmn exactly.
;;;; Penalty numbers below are CMN literals. Three clear scan variants are
;;;; recorded separately; their provenance must not be lost in rescoring.
;;;;
;;;; Bill's pitch zero is low C around 16 Hz. In ordinary MIDI numbering,
;;;; add 12: the cantus starts at Bill 50 = D4 = MIDI 62.
;;;; Durations/onsets use eighth-note units: 8=whole, 4=half, 2=quarter, 1=eighth.
;;;; Returned ties use the FUX API convention: (FROM . TO), 1-based indices.
;;;; Species per voice is intentionally unspecified: notation does not
;;;; establish which rule parameter was used to generate each inner voice.

(defpackage :fux-history
  (:use :cl)
  (:export :figure-16-8
           :figure-16-8-midi
           :bill-pitch-to-midi
           :check-figure-16-8))

(in-package :fux-history)

(defparameter *figure-16-8-bars*
  '(:source (:chapter-pages (212 213) :figure "16.8"
           :report-page 45 :report-pdf-page 49)
  :pitch-origin :low-c-16-hz
  :time-unit :eighth-note
  :voices
  ((:staff 1 :role :generated :visible-texture :florid
    :cmn-penalties
    (0 1 213 13 1 105 15 5 316 3 218 15 19 119 140 16 6 115 7
     218 9 1 253 10 2 318 2 220 112 3 338 30 107 115 129 32 5 16 4 122)
    :bars
    (((62 2) (64 2) (65 2) (69 1) (67 1))
     ((65 4) (69 2) (67 2))
     ((65 4) (67 4))
     ((69 4) (65 2) (62 1) (69 1))
     ((72 2) (69 1) (71 1) (72 2) (71 2))
     ((69 2) (62 1) (64 1) (65 2) (62 1) (64 1))
     ((65 4) (67 4))
     ((69 2) (64 1) (65 1) (67 4))
     ((69 4) (62 2) (60 1) (62 1))
     ((59 4) (64 2) (59 1) (61 1))
     ((62 8))))
   (:staff 2 :role :generated :visible-texture :half-notes-with-two-ties
    :cmn-penalties
    (0 17 3 133 17 13 7 11 117 21 4 32 10 19 7 9 3 5 11 105 7)
    :bars
    (((57 4) (62 4)) ((60 4) (48 4)) ((48 4) (45 4))
     ((45 4) (57 4)) ((52 4) (48 4)) ((50 4) (48 4))
     ((45 4) (53 4)) ((52 4) (64 4)) ((62 4) (57 4))
     ((55 4) (52 4)) ((57 8)))
    :ties (((2 2) (3 1)) ((3 2) (4 1))))
   (:staff 3 :role :cantus-firmus :visible-texture :whole-notes
    :bars (((50 8)) ((53 8)) ((52 8)) ((50 8)) ((55 8))
           ((53 8)) ((57 8)) ((55 8)) ((53 8)) ((52 8)) ((50 8))))
   (:staff 4 :role :generated :visible-texture :half-notes
    :cmn-penalties
    (0 2 11 12 3 21 125 112 2 8 13 21 10 3 2 19 5 6 110 2 10)
    :bars
    (((45 4) (41 4)) ((45 4) (41 4)) ((36 4) (48 4))
     ((53 4) (50 4)) ((52 4) (40 4)) ((45 4) (41 4))
     ((36 4) (38 4)) ((40 4) (48 4)) ((45 4) (50 4))
     ((52 4) (47 4)) ((45 8))))
   (:staff 5 :role :generated-bass :visible-texture :whole-notes
    :cmn-penalties (0 1 6 3 13 17 5 6 20 3 11)
    :bars (((38 8)) ((33 8)) ((33 8)) ((29 8)) ((24 8))
           ((29 8)) ((29 8)) ((36 8)) ((41 8)) ((43 8)) ((38 8)))))))


(defparameter *figure-16-8-source-variants*
  '((:staff 1 :note 13 :onset 30 :cmn 19 :scan 10
     :chapter-page 212 :report-page 45)
    (:staff 1 :note 30 :onset 59 :cmn 3 :scan 4
     :chapter-page 213 :report-page 45)
    (:staff 4 :note 5 :onset 16 :cmn 3 :scan 8
     :chapter-page 212 :report-page 45
     :report-legibility :blurred)))

(defun note-onsets (durations)
  (loop with onset = 0
        for duration in durations
        collect onset
        do (incf onset duration)))

(defun bar-note-index (position bars)
  ;; Convert (bar note) to the 1-based note index used by FUX rhythm output.
  (+ (second position)
     (loop for bar in bars
           for i from 1 below (first position)
           sum (length bar))))

(defun voice-with-rhythm (source)
  (let* ((bars (copy-tree (getf source :bars)))
         (notes (loop for bar in bars append bar))
         (pitches (mapcar #'first notes))
         (durations (mapcar #'second notes)))
    (list :staff (getf source :staff)
          :role (getf source :role)
          :visible-texture (getf source :visible-texture)
          :pitches pitches
          :onsets (note-onsets durations)
          :durations durations
          :ties (loop for (from to) in (getf source :ties)
                      collect (cons (bar-note-index from bars)
                                    (bar-note-index to bars)))
          :cmn-penalties (copy-list (getf source :cmn-penalties))
          :bars bars)))

(defun figure-16-8 ()
  "Return a fresh copy of the five-voice historical score, in staff order.
No generation or rescoring occurs. CMN-PENALTY-SUM sums literal notation
labels; it is not a penalty claimed for the current FUX implementation."
  (let* ((voices (mapcar #'voice-with-rhythm
                        (getf *figure-16-8-bars* :voices)))
         (sum (loop for voice in voices
                    sum (reduce #'+ (getf voice :cmn-penalties)
                                :initial-value 0))))
    (list :source (copy-tree (getf *figure-16-8-bars* :source))
          :pitch-origin :bill-low-c
          :time-unit :eighth-note
          :cmn-penalty-sum sum
          :source-variants (copy-tree *figure-16-8-source-variants*)
          :voices voices)))

(defun bill-pitch-to-midi (pitch)
  "Convert Bill's semitone number above low C (~16 Hz) to a MIDI number."
  (+ pitch 12))

(defun figure-16-8-midi ()
  "Return the same score with pitch numbers converted to ordinary MIDI.
The conversion changes both PITCHES and the editable barwise representation."
  (let ((score (figure-16-8)))
    (setf (getf score :pitch-origin) :midi)
    (dolist (voice (getf score :voices))
      (setf (getf voice :pitches)
            (mapcar #'bill-pitch-to-midi (getf voice :pitches)))
      (dolist (bar (getf voice :bars))
        (dolist (note bar)
          (incf (first note) 12))))
    score))

(defun check-figure-16-8 ()
  "Check transcription structure, rhythm, ties and half-note consonances.
This verifies the historical fixture, not FUX search or its complete rules."
  (let* ((score (figure-16-8))
         (voices (getf score :voices))
         (bass (getf (fifth voices) :pitches)))
    (assert (= (length voices) 5))
    (assert (equal (mapcar (lambda (voice)
                            (length (getf voice :pitches)))
                          voices)
                   '(40 21 11 21 11)))
    (assert (= (loop for voice in voices
                    sum (length (getf voice :pitches)))
               104))
    (assert (= (getf score :cmn-penalty-sum) 4506))
    (assert (equal (loop for voice in voices
                        collect (reduce #'+ (getf voice :cmn-penalties)
                                        :initial-value 0))
                   '(3373 551 0 497 85)))
    (assert (equal (getf (third voices) :pitches)
                   '(50 53 52 50 55 53 57 55 53 52 50)))
    (assert (eq (getf (third voices) :role) :cantus-firmus))
    (assert (equal (getf (second voices) :ties) '((4 . 5) (6 . 7))))
    (loop for voice in voices
          for staff from 1
          for pitches = (getf voice :pitches)
          for durations = (getf voice :durations)
          for onsets = (getf voice :onsets)
          do (assert (= (length pitches) (length durations) (length onsets)))
             (assert (= (reduce #'+ durations) 88))
             (assert (= (first onsets) 0))
             (assert (= (car (last onsets)) 80))
             (assert (= (car (last durations)) 8))
             (unless (= staff 3)
               (assert (= (length pitches)
                          (length (getf voice :cmn-penalties)))))
             (loop for bar in (getf voice :bars)
                   do (assert (= (reduce #'+ bar :key #'second) 8)))
             (loop for (a b) on onsets
                   for duration in durations
                   while b
                   do (assert (= (+ a duration) b)))
             (dolist (tie (getf voice :ties))
               (let ((from (1- (car tie)))
                     (to (1- (cdr tie))))
                 (assert (= (nth from pitches) (nth to pitches)))
                 (assert (= (+ (nth from onsets) (nth from durations))
                            (nth to onsets))))))
    ;; This limited musical check is the evidence for distinguishing a
    ;; half-note rhythm from a claim about second-species rule assignment.
    (dolist (voice (list (second voices) (fourth voices)))
      (loop for pitch in (getf voice :pitches)
            for onset in (getf voice :onsets)
            for bass-pitch = (nth (floor onset 8) bass)
            do (assert (>= pitch bass-pitch))
               (assert (member (mod (- pitch bass-pitch) 12)
                               '(0 3 4 7 8 9)))))
    (let ((midi (figure-16-8-midi)))
      (assert (eq (getf midi :pitch-origin) :midi))
      (loop for original in voices
            for converted in (getf midi :voices)
            do (assert (equal (mapcar #'bill-pitch-to-midi
                                     (getf original :pitches))
                              (getf converted :pitches)))
               (assert (equal (getf original :durations)
                              (getf converted :durations)))
               (assert (equal (getf original :ties)
                              (getf converted :ties)))))
    t))

;;; Scratch examples: evaluate one form at a time.
;;;
;;; Compile and load this standalone study file, then verify its data.
;;; (load (compile-file "figure-16-8.lisp"))
;;; (fux-history:check-figure-16-8) ; => T
;;;
;;; All five voices with onsets, durations, explicit ties and source labels.
;;; (fux-history:figure-16-8)
;;;
;;; Inspect the actual cantus (third staff), then the half-note staff's ties.
;;; (getf (third (getf (fux-history:figure-16-8) :voices)) :pitches)
;;; (getf (second (getf (fux-history:figure-16-8) :voices)) :ties)
;;;
;;; MIDI version for later playback/notation; this does not run the generator.
;;; (fux-history:figure-16-8-midi)
;;; (fux-history:bill-pitch-to-midi 50) ; => 62, D4
