(load "load-species.lisp")

(defun figure-cmn-pitch (pitch)
  (format nil "~A~D"
          (aref #("c" "cs" "d" "ef" "e" "f" "fs" "g" "af" "a" "bf" "b")
                (mod pitch 12))
          (floor pitch 12)))

(defun figure-cmn-duration (duration)
  (case duration (8 "w") (4 "h") (2 "q") (1 "e")
        (otherwise (error "Unknown duration ~S" duration))))

(defun write-figure-search-cmn (cantus best patterns)
  "Make an unannotated CMN score; the original figure's costs do not apply."
  (let ((pitches (list (fifth best) (fourth best) cantus
                       (third best) (second best)))
        (durations (list (fux::fifth-species-durations 11 :patterns patterns)
                         (append (make-list 20 :initial-element 4) '(8))
                         (make-list 11 :initial-element 8)
                         (append (make-list 20 :initial-element 4) '(8))
                         (make-list 11 :initial-element 8))))
    (with-open-file (s "figure-16-8-search-output.cmn"
                       :direction :output :if-exists :supersede)
      (format s "(in-package :cmn)~%")
      (format s ";;; FUX-CL search with Figure 16.8 cantus, starts and rhythms.~%")
      (format s ";;; Computed pitches; printed historical penalties do not apply.~%")
      (format s "(cmn (output-file ~S) (size 16) (automatic-line-breaks nil)~%"
              "figure-16-8-search-output.eps")
      (loop for voice in pitches
            for ds in durations
            for staff from 1
            do (assert (= (length voice) (length ds)))
               (format s "  (staff bar ~A (meter 4 4 (meter-size 0))~%    "
                       (if (<= staff 3) "treble" "bass"))
               (loop for pitch in voice
                     for duration in ds
                     for note from 1
                     do (format s "(~A ~A~A) "
                                (figure-cmn-pitch pitch)
                                (figure-cmn-duration duration)
                                (if (= staff 2)
                                    (case note
                                      ((4 6) " begin-tie")
                                      ((5 7) " end-tie")
                                      (otherwise ""))
                                    "")))
               (format s "bar)~%"))
      (format s ")~%"))))

(let* ((figure (fux-history:figure-16-8))
       (voices (getf figure :voices))
       (staff (lambda (number)
                (find number voices :key (lambda (v) (getf v :staff)))))
       (cantus (getf (funcall staff 3) :pitches))
       (starts (mapcar (lambda (number) (first (getf (funcall staff number) :pitches)))
                       '(5 4 2 1)))
       (patterns '(9 2 1 7 6 8 1 5 7 7))
       (top-durations (getf (funcall staff 1) :durations))
       (start-time (get-internal-real-time)))
  (assert (equal top-durations (fux::fifth-species-durations 11 :patterns patterns)))
  (format t "~&FIGURE INPUT: cantus=~S starts=~S patterns=~S~%" cantus starts patterns)
  (finish-output)
  (let* ((records (fux:generate-counterpoint cantus :starts starts :species 5
                                             :mode :dorian :reference t
                                             :patterns patterns
                                             :inner-half-notes t :stream nil))
         (best (fux:best-counterpoint records)))
    (format t "~&FIGURE RESULT: improvements=~D elapsed=~,2F seconds~%"
            (length records)
            (/ (- (get-internal-real-time) start-time)
               internal-time-units-per-second))
    (format t "BEST: ~S~%" best)
    (with-open-file (s "figure-16-8-search-results.sexp"
                       :direction :output :if-exists :supersede)
      (write records :stream s :pretty t))
    (when best (write-figure-search-cmn cantus best patterns))
    (finish-output)))
