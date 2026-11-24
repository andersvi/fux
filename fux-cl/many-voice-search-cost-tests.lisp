;;;; Search costs belong to candidate attacks, before SaveResults alters pitches.
;;;; Load after load-species.lisp and fux-cmn.lisp.
(in-package :fux)

(defun test-many-voice-search-cost-capture ()
  (dolist (case '((:cantus (50 53 50) :starts (38 45 53 57 62)
                   :species 1 :mode :dorian :reference t)
                  (:cantus (50 53 55 52 50) :starts (38 57 62)
                   :species 5 :patterns (3 4 7 9)
                   :mode :dorian :reference t)
                  (:cantus (50 53 55 52 50) :starts (38 45 57 62)
                   :species 5 :patterns (3 4 7 9)
                   :inner-half-notes t :mode :dorian :reference t)))
    (let* ((cantus (getf case :cantus))
           (args (loop for (key val) on case by #'cddr
                       unless (eq key :cantus) append (list key val)))
           (plain (apply #'generate-counterpoint cantus args))
           (traced (apply #'generate-counterpoint cantus
                          (append args '(:with-rhythm t
                                         :with-search-costs t)))))
      (assert (= (length plain) (length traced)))
      (assert plain)
      (loop for old in plain
            for record in traced
            for voices = (getf record :voices)
            for costs = (getf record :search-local-costs)
            for raw = (getf record :raw-voices)
            do (assert (= (first old) (getf record :penalty)))
               (assert (equal (rest old)
                              (mapcar (lambda (v) (getf v :pitches))
                                      (rest voices))))
               (assert (= (length voices) (length costs)))
               (assert (null (first costs)))
               (assert (= (length raw) (length (rest voices))))
               (loop for data in (rest voices)
                     for line in (rest costs)
                     for before in raw
                     do (assert (= (length line)
                                   (length before)
                                   (length (getf data :pitches))))
                        (assert (zerop (first line)))
                        (assert (every #'numberp line)))
               (assert (= (getf record :penalty)
                          (loop for line in (rest costs)
                                sum (reduce #'+ line))))
               (assert (equal (getf record :ficta-changes)
                              (loop for before in raw
                                    for after in (mapcar (lambda (v)
                                                         (getf v :pitches))
                                                       (rest voices))
                                    for voice from 1
                                    append (loop for a in before
                                                 for b in after
                                                 for note from 1
                                                 unless (= a b)
                                                   collect (list voice note a b)))))
               (let ((cmn (with-output-to-string (stream)
                            (write-counterpoint-cmn record :stream stream
                                                    :score-notes :search))))
                 (assert (= (loop for line in (rest costs)
                                  sum (length line))
                            (loop with pos = 0
                                  for hit = (search "(fingering " cmn :start2 pos)
                                  while hit
                                  count hit
                                  do (setf pos (1+ hit)))))))))
  t)

(export 'test-many-voice-search-cost-capture :fux)

;; (fux:test-many-voice-search-cost-capture)
