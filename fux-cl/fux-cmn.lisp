;;;; Small, independent CMN writer for GENERATE-COUNTERPOINT :WITH-RHYTHM T.
;;;; Load after load-species.lisp. Bill's pitch 0 is low C; 62 is D5 in CMN.
(in-package :fux)

(defun cmn-pitch-name (pitch)
  (format nil "~A~D"
          (aref #("c" "cs" "d" "ef" "e" "f" "fs" "g" "af" "a" "bf" "b")
                (mod pitch 12))
          (floor pitch 12)))

(defun cmn-duration-name (eighths)
  (or (cdr (assoc eighths '((1 . "e") (2 . "q") (4 . "h") (8 . "w"))))
      (error "CMN writer: unsupported duration ~S eighth notes" eighths)))

(defun counterpoint-cmn-note-costs (record mode crossing-policy)
  "Literally rescore a finished line, returning per-voice per-note costs.
The fixed first note and cantus have no local search cost. Ficta or saved
state may make the sum differ from the original search penalty."
  (let* ((voices (getf record :voices))
         (cantus (getf (first voices) :pitches))
         (parts (1- (length voices)))
         (species (getf (car (last voices)) :species))
         (state (make-first-species-cantus
                 cantus (first (getf (second voices) :pitches)) mode))
         (base (aref state 0)))
    (loop for voice from 1 to parts
          for data in (rest voices)
          do (set-total-notes state voice (length (getf data :pitches)))
             (loop for pitch in (getf data :pitches)
                   for onset in (getf data :onsets)
                   for duration in (getf data :durations)
                   for note from 1
                   do (set-onset state note voice onset)
                      (set-duration state note voice duration)
                      (set-pitch state note voice (- pitch base))))
    (cons nil
          (loop for voice from 1 to parts
                collect (loop for note from 1 to (total-notes state voice)
                              collect (if (= note 1) 0
                                          (let ((pitch (pitch-at state note voice)))
                                            (if (= parts 1)
                                                (funcall
                                                 (ecase species
                                                   (1 #'check-first-species)
                                                   (2 #'check-second-species)
                                                   (3 #'check-third-species)
                                                   (4 #'check-fourth-species)
                                                   (5 #'check-fifth-species))
                                                 state note pitch
                                                 :limit 10000000
                                                 :crossing-policy crossing-policy)
                                                (many-voice-local-cost
                                                 state note pitch voice parts
                                                 species 10000000 crossing-policy)))))))))

(defun write-counterpoint-cmn (record &key (stream *standard-output*)
                                          (output-file "counterpoint.eps")
					  (size 20)
                                          staff-order (score-notes nil)
                                          (mode :dorian)
                                          (crossing-policy :stored) note-costs)
  "Write a CMN form from one :WITH-RHYTHM T improvement RECORD.
STAFF-ORDER lists zero-based indices into :VOICES, from top to bottom;
default is reverse generator order. SCORE-NOTES T rescoring the finished
line with MODE and CROSSING-POLICY adds (fingering N) to generated notes.
Alternatively NOTE-COSTS supplies lists parallel to :VOICES (NIL for
the cantus). The search penalty is printed separately as a comment."
  (let* ((voices (getf record :voices))
         (order (or staff-order (reverse (loop for v in voices for i from 0 collect i)))))
    (unless (and voices (eql (getf record :time-unit) :eighth-note)
                 (= (length order) (length voices))
                 (equal (sort (copy-list order) #'<)
                        (loop for i below (length voices) collect i)))
      (error "CMN writer: expected a complete :WITH-RHYTHM T record and staff order"))
    (when (and score-notes note-costs)
      (error "Use SCORE-NOTES or NOTE-COSTS, not both"))
    (when score-notes
      (setf note-costs (counterpoint-cmn-note-costs record mode crossing-policy)))
    (format stream "(in-package :cmn)~%;;; FUX search penalty: ~D~%"
            (getf record :penalty))
    (format stream "(cmn (output-file ~S) (size ~D) ~%" output-file size)
    (dolist (index order)
      (let* ((voice (nth index voices))
             (pitches (getf voice :pitches))
             (durations (getf voice :durations))
             (ties (getf voice :ties))
             (costs (and note-costs (nth index note-costs))))
        (unless (and (= (length pitches) (length durations))
                     (or (null costs) (= (length costs) (length pitches))))
          (error "CMN writer: pitch, duration, or note-cost count differs in voice ~D" index))
        (format stream "  (staff bar ~A (meter 4 4 (meter-size 0))~%    "
                (if (>= (first pitches) 48) "treble" "bass"))
        (loop for pitch in pitches for duration in durations for n from 1
              for cost in (or costs (make-list (length pitches)))
              do (format stream "(~A ~A~A~A~A) "
                         (cmn-pitch-name pitch) (cmn-duration-name duration)
                         (if (find n ties :key #'car) " begin-tie" "")
                         (if (find n ties :key #'cdr) " end-tie" "")
                         (if cost (format nil " (fingering ~D)" cost) "")))
        (format stream "bar)~%")))
    (format stream ")~%")
    record))

(export 'write-counterpoint-cmn :fux)

;;; Usage (run from directory containing load-species.lisp):
;;; (load "load-species.lisp")
;;; (load "fux-cmn.lisp")
;;; (let* ((records (fux:generate-counterpoint '(50 53 55 52 50)
;;;                       :starts '(38 45 57 62) :species 5 :reference t
;;;                       :patterns '(3 4 7 9) :inner-half-notes t
;;;                       :with-rhythm t))
;;;        (best (fux:best-counterpoint records)))
;;;   (with-open-file (s "counterpoint.cmn" :direction :output
;;;                      :if-exists :supersede)
;;;     (fux:write-counterpoint-cmn best :stream s
;;;                                 :staff-order '(4 3 0 2 1)
;;;                                 :score-notes t :mode :dorian)))
;;;
;;; Other small examples (the writer accepts the final record from each):
;;; (fux:best-counterpoint
;;;   (fux:generate-counterpoint '(50 53 55 52 50)
;;;      :starts '(57) :species 2 :reference t :with-rhythm t))
;;; (fux:best-counterpoint
;;;   (fux:generate-counterpoint '(50 53 55 52 50)
;;;      :starts '(57 62) :species 4 :reference t :with-rhythm t))
;;; (fux:best-counterpoint
;;;   (fux:generate-counterpoint '(50 53 55 52 50)
;;;      :starts '(38 57 62) :species 5 :reference t
;;;      :patterns '(3 4 7 9) :with-rhythm t))
