;;;; Regression fixtures for cantus + species 1 + species 3, 4, 5.
;;;; All references are outputs of the original C code with fixed input.
(in-package :fux)

(defparameter *mixed-advanced-cases*
  '((:short (50 53 55 52 50))
    (:medium (50 53 52 50 55 53 50))))

(defun mixed-advanced-cantus (case)
  (or (second (assoc case *mixed-advanced-cases*))
      (error "Unknown mixed advanced case: ~S" case)))

(defparameter *mixed-3-short-reference*
  '(
    (131 (57 50 47 52 57)
         (62 64 65 67 69 67 65 64 62 60 59 57 55 57 59 61 62))
    (121 (57 50 47 49 50)
         (62 64 65 67 69 67 65 64 62 60 59 62 57 64 69 57 57))
    (108 (57 50 47 55 50)
         (62 64 65 67 69 67 65 64 62 60 59 62 60 64 62 61 62))
    (92 (57 50 52 55 57)
         (62 64 65 67 69 67 65 69 64 65 67 60 60 64 62 61 62))
    (78 (57 50 52 55 57)
         (62 64 65 67 69 67 65 69 64 62 60 59 60 64 62 61 62))
    ))
(defparameter *mixed-3-medium-reference*
  '(
    (324 (57 50 48 53 55 57 57)
         (62 64 65 67 69 67 65 67 69 57 59 60 59 60 62 60 59 64 59 61 62 61 62 61 62))
    (243 (57 50 48 53 52 49 50)
         (62 64 65 67 69 67 65 67 69 57 59 60 59 60 62 57 60 59 57 55 57 52 57 52 57))
    (224 (57 50 48 53 52 50 57)
         (62 64 65 67 69 67 65 67 69 57 59 60 59 60 62 57 60 59 57 55 57 62 59 61 62))
    (126 (57 50 48 53 52 57 57)
         (62 64 65 67 69 67 65 67 69 57 59 60 59 60 62 57 60 59 60 59 62 61 62 61 62))
    (115 (57 50 48 53 52 49 50)
         (62 64 65 67 69 67 65 67 69 57 59 60 59 60 62 57 60 62 64 60 64 62 60 59 57))
    ))
(defparameter *mixed-4-short-reference*
  '(
    (244 (57 57 59 61 62)
         (62 65 62 60 60 59 55 67 62))
    (220 (57 57 59 61 62)
         (62 65 62 60 60 59 64 67 69))
    (103 (57 57 52 49 50)
         (62 65 62 60 60 67 69 57 57))
    (61 (57 57 52 49 50)
         (62 65 62 60 60 59 59 57 57))
    ))
(defparameter *mixed-4-medium-reference*
  '(
    (117 (57 57 55 59 59 61 62)
         (62 65 62 60 60 67 67 65 65 64 64 62 69))
    ))
(defparameter *mixed-5-short-reference*
  '(
    (1323 (57 57 59 55 57)
         (62 60 59 71 72 74 62 69 69 67 64 65 67 60 62 64 59 61 62))
    (1144 (57 57 59 55 57)
         (62 60 59 71 72 74 62 69 69 67 64 62 60 59 60 64 59 61 62))
    (1010 (57 50 48 55 57)
         (62 64 65 53 55 57 55 53 53 52 57 59 60 59 57 55 59 61 62))
    (782 (57 50 48 52 57)
         (62 64 65 53 55 57 55 53 53 52 57 59 60 59 57 55 59 61 62))
    (495 (57 50 52 52 57)
         (62 64 65 53 55 57 59 57 60 59 60 59 60 59 57 55 59 61 62))
    ))
(defparameter *mixed-5-medium-reference*
  '(
    (2675 (57 57 45 41 40 45 50)
         (62 60 59 71 72 74 62 64 65 60 65 67 69 62 64 65 69 71 72 67 69 71 72 72 76 73 74))
    (2173 (57 57 45 41 40 45 50)
         (62 60 59 71 72 74 62 64 65 60 65 67 69 62 64 65 69 71 72 67 69 71 72 69 64 61 62))
    (1338 (57 50 52 47 47 45 57)
         (62 64 65 53 55 57 55 53 60 55 60 59 67 62 64 65 67 69 71 69 67 62 64 65 64 61 62))
    (1203 (57 50 52 47 47 49 50)
         (62 64 65 53 55 57 55 53 60 55 60 59 67 62 64 65 67 69 71 67 62 59 62 57 69 57 57))
    ))

(defun mixed-advanced-reference (species case)
  (ecase species
    (3 (ecase case (:short *mixed-3-short-reference*)
                     (:medium *mixed-3-medium-reference*)))
    (4 (ecase case (:short *mixed-4-short-reference*)
                     (:medium *mixed-4-medium-reference*)))
    (5 (ecase case (:short *mixed-5-short-reference*)
                     (:medium *mixed-5-medium-reference*)))))

(defun run-mixed-advanced (&key (species 3) (case :short)
                                (reference nil) (stream t)
                                (seed 1) patterns)
  (generate-mixed-species
   (mixed-advanced-cantus case) '(57 62) species
   :mode :dorian :stream stream :seed seed :patterns patterns
   :integer-bounds reference
   :terminal-bound (if reference :max-penalty :best-fit)
   :crossing-policy (if reference :stored :candidate)))

(defun compare-mixed-advanced (&key (species 3) (case :short))
  "Compare the complete improvement sequence with the C baseline."
  (let* ((expected (mixed-advanced-reference species case))
         (actual (run-mixed-advanced :species species :case case
                                     :reference t :stream nil)))
    (format t "~&Mixed species 1+~D (~A): C ~D, CL ~D records. Match: ~A~%"
            species case (length expected) (length actual)
            (if (equal actual expected) "YES" "NO"))
    (unless (equal actual expected)
      (format t "C: ~S~%CL: ~S~%" expected actual))
    actual))

(defun run-mixed-advanced-tests (&key (full nil))
  "Smoke-test rhythmic alignment; FULL also runs all six search comparisons."
  (loop for species in '(3 4 5)
        for state = (make-mixed-advanced-state
                     (mixed-advanced-cantus :short) '(57 62) :dorian species)
        do (assert (= (total-notes state 1) 5))
           (assert (= (onset-at state 2 1) 8))
           (assert (= (onset-at state (total-notes state 2) 2) 32))
           (assert (= (duration-at state (total-notes state 2) 2) 8))
           (assert (member '(1 . 2) (mixed-notes-at state 8) :test #'equal))
           (assert (integerp
                    (check-mixed-advanced
                     state 2 (pitch-at state 1 2) species)))
           (assert (= (total-notes state 2)
                      (case species (3 17) (4 9) (5 19)))))
  (when full
    (loop for species in '(3 4 5)
          do (dolist (case '(:short :medium))
               (assert (equal (compare-mixed-advanced
                               :species species :case case)
                              (mixed-advanced-reference species case))))))
  t)
