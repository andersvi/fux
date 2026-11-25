;;;; Positive C fixture at the six-voice capacity boundary.
;;;; Load after load-species.lisp; independent of the CMN writer.
(in-package :fux)

(defparameter *six-voice-positive-c-reference*
  '((106
     (38 37 38)
     (45 45 45)
     (53 57 62)
     (57 52 50)
     (62 64 57))))

(defun test-six-voice-positive-c-reference ()
  (let* ((cantus '(50 53 50))
         (starts '(38 45 53 57 62))
         (records (generate-counterpoint
                   cantus :starts starts :species 1 :mode :dorian
                   :reference t :stream nil))
         (rich (generate-counterpoint
                cantus :starts starts :species 1 :mode :dorian
                :reference t :stream nil :with-rhythm t)))
    (unless (equal records *six-voice-positive-c-reference*)
      (error "Six-voice CL/C mismatch:~%C: ~S~%CL: ~S"
             *six-voice-positive-c-reference* records))
    (assert (= (length rich) 1))
    (assert (= (getf (first rich) :penalty) 106))
    (assert (= (length (getf (first rich) :voices)) 6))
    (loop for voice in (getf (first rich) :voices)
          do (assert (equal (getf voice :onsets) '(0 8 16)))
             (assert (equal (getf voice :durations) '(8 8 8))))
    (values t records)))

(export 'test-six-voice-positive-c-reference :fux)
