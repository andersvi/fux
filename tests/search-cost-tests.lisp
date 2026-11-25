;;;; Load after load-species.lisp and fux-cmn.lisp.
(in-package :fux)

(defun test-fifth-search-cost-capture ()
  (let* ((cantus '(50 53 55 52 50))
         (options '(:starts (57) :species 5 :patterns (3 4 7 9)
                    :reference t))
         (plain (apply #'generate-counterpoint cantus options))
         (traced (apply #'generate-counterpoint cantus
                        (append options '(:with-rhythm t
                                          :with-search-costs t)))))
    (assert (= (length plain) (length traced)))
    (loop for old in plain for new in traced
          for costs = (getf new :search-local-costs)
          for raw = (getf new :raw-pitches)
          for notated = (getf (second (getf new :voices)) :pitches)
          do (assert (= (first old) (getf new :penalty)
                        (reduce #'+ costs)))
             (assert (equal (second old) notated))
             (assert (= (length costs) (length raw) (length notated)))
             (assert (zerop (first costs)))
             (assert (equal (getf new :ficta-changes)
                            (loop for a in raw for b in notated for i from 1
                                  unless (= a b) collect (list i a b)))))
    (let* ((best (best-counterpoint traced))
           (cmn (with-output-to-string (stream)
                  (write-counterpoint-cmn best :stream stream
                                          :score-notes :search))))
      (assert (search "(fingering 0)" cmn))
      (assert (search (format nil "FUX search penalty: ~D"
                              (getf best :penalty)) cmn)))
    t))

(export 'test-fifth-search-cost-capture :fux)
