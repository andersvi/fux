;;;; Independent local-score regression for Schottstaedt's Figure 16.8.
;;;; The notation is in figure-16-8.lisp (staff order). C costs here were
;;;; captured from the unmodified fux.c Check, with CM selected and a
;;;; generous CurLim=10000000. See figure-16-8-audit.md for provenance.
(in-package :fux)

(defparameter *figure-16-8-original-c-costs*
  '((5 1 6 3 13 17 5 6 20 3 11)
    (4 2 11 12 8 21 122 112 2 8 13 21 10 3 2 22 5 6 110 2 10)
    (2 17 3 133 17 13 7 11 117 21 4 32 10 19 7 9 3 5 11 105 7)
    (1 1 213 13 1 105 15 5 316 3 219 15 10 119 140 16 6 115 7
       219 9 1 253 10 2 318 2 221 112 4 338 31 1000006 115 1000109
       125 5 16 4 122)))

(defun figure-16-8-audit-rows ()
  "Return (STAFF NOTE ONSET PITCH C-COST CL-COST PRINTED-COST) rows.
The first generated note in each voice is fixed and is not a Check call."
  (let* ((figure (fux-history:figure-16-8))
         (by-staff (getf figure :voices))
         (base-pitch (mod (car (last (getf (find 3 by-staff
                                               :key (lambda (v) (getf v :staff)))
                                         :pitches))) 12))
         (state (make-fux-state :mode :dorian :base-pitch base-pitch
                                :total-time 80))
         (rows nil))
    (loop for staff in '(3 5 4 2 1)
          for voice from 0
          for score = (find staff by-staff :key (lambda (v) (getf v :staff)))
          do (set-total-notes state voice (length (getf score :pitches)))
             (loop for pitch in (getf score :pitches)
                   for duration in (getf score :durations)
                   for onset in (getf score :onsets)
                   for note from 1
                   do (set-pitch state note voice (- pitch base-pitch))
                      (set-duration state note voice duration)
                      (set-onset state note voice onset)))
    (loop for staff in '(5 4 2 1)
          for voice from 1
          for score = (find staff by-staff :key (lambda (v) (getf v :staff)))
          for costs in *figure-16-8-original-c-costs*
          do (assert (= (length costs) (total-notes state voice)))
             (loop for note from 2 to (total-notes state voice)
                   for c-cost in (rest costs)
                   for pitch = (nth (1- note) (getf score :pitches))
                   for onset = (nth (1- note) (getf score :onsets))
                   for printed = (or (getf (find-if
                                             (lambda (variant)
                                               (and (= staff (getf variant :staff))
                                                    (= note (getf variant :note))))
                                             (getf figure :source-variants))
                                           :scan)
                                     (nth (1- note) (getf score :cmn-penalties)))
                   for cl-cost = (if (= voice 4)
                                     (check-mixed-fifth-species
                                      state note (- pitch base-pitch)
                                      :voice voice :num-parts 4 :limit 10000000
                                      :crossing-policy :stored)
                                     (check-poly-first-species
                                      state note (- pitch base-pitch)
                                      :voice voice :num-parts 4 :limit 10000000
                                      :crossing-policy :stored))
                   do (push (list staff note onset pitch c-cost cl-cost printed)
                            rows)))
    (nreverse rows)))

(defun run-figure-16-8-c-audit (&key (stream *standard-output*))
  "Compare the CL checker with the independent original C per-note costs.
Return T when all 89 match; report the nine published-notation differences."
  (let* ((rows (figure-16-8-audit-rows))
         (differences (remove-if (lambda (row) (= (fifth row) (sixth row))) rows))
         (printed (count-if (lambda (row) (= (fifth row) (seventh row))) rows)))
    (when stream
      (format stream "~&Figure 16.8: CL/C ~D/~D; C/chapter ~D/~D.~%"
              (- (length rows) (length differences)) (length rows)
              printed (length rows))
      (dolist (row differences)
        (format stream "  staff ~D note ~D onset ~D pitch ~D: C ~D CL ~D~%"
                (first row) (second row) (third row) (fourth row)
                (fifth row) (sixth row))))
    (assert (= 89 (length rows)))
    (assert (= 80 printed))
    (null differences)))

(export 'run-figure-16-8-c-audit :fux)
