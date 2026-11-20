;;;; fux.lisp -- shared helpers for the Schottstaedt counterpoint port.
;;;; Source: https://github.com/namin/metasolfeggio/blob/master/classics/fux.c
;;;; Historical rule semantics are intentionally unchanged.
;;;; The implemented generator is first species, one generated voice;
;;;; see first-species.lisp and search.lisp for evaluation and search.

(defpackage :fux
  (:use :cl)
  (:export :make-fux-state :in-mode :bad-melody-p :motion-type
           :direct-motion-to-perfect-consonance-p
           :consecutive-skips-in-same-direction-p :out-of-range-p
           :extreme-range-p :total-range :cantus :other-voice :bass
           :pitch-repeats :too-much-of-interval-p :dissonance-p
           :set-pitch :pitch-at :set-onset :set-duration :set-total-notes
           :run-basic-tests))
(in-package :fux)

;;; fux.c: interval and mode definitions, InMode, BadMelody (lines 26-83)
;;; Pitch units are the same integer semitones as in the C version.
(defparameter *perfect-consonances* '#(t nil nil nil nil nil nil t nil nil nil nil t))
(defparameter *imperfect-consonances* '#(nil nil nil t t nil nil nil t t nil nil nil))
(defparameter *dissonances* '#(nil t t nil nil t t nil nil nil t t nil))
(defparameter *bad-melodic-intervals* '#(nil nil nil nil nil nil t nil nil t t t nil))

(defparameter *modes*
  '((:ionian     . #(1 0 1 0 1 1 0 1 0 1 0 1))
    (:dorian     . #(1 0 1 1 0 1 0 1 0 1 1 0))
    (:phrygian   . #(1 1 0 1 0 1 0 1 1 0 1 0))
    (:lydian     . #(1 0 1 0 1 0 1 1 0 1 0 1))
    (:mixolydian . #(1 0 1 0 1 1 0 1 0 1 1 0))
    (:aeolian    . #(1 0 1 1 0 1 0 1 0 0 1 0))
    (:locrian    . #(1 1 0 1 0 1 1 0 1 0 1 0))))

(defun in-mode (pitch mode)
  "C: InMode. MODE is a keyword, e.g. :dorian."
  (let ((scale (cdr (assoc mode *modes*))))
    (unless scale (error "Unknown mode: ~S" mode))
    (not (zerop (aref scale (mod pitch 12))))))

(defun bad-melody-p (interval)
  "C: BadMelody. Preserve the special downward minor-sixth case."
  (let ((n (abs interval)))
    (or (> n 12)
        (aref *bad-melodic-intervals* n)
        (= interval -8))))

(defun skip-p (interval) (> (abs interval) 2))
(defun step-p (interval) (member (abs interval) '(1 2) :test #'=))
;; Note: C's AThird and ASeventh check *positive* intervals only.
(defun third-p (interval) (member interval '(3 4) :test #'=))
(defun seventh-p (interval) (member interval '(10 11) :test #'=))
(defun octave-p (interval) (and (/= interval 0) (zerop (mod (abs interval) 12))))
(defun tenth-p (interval) (and (> (abs interval) 14)
                                 (third-p (mod (abs interval) 12))))

(defun motion-type (p1 p2 p3 p4)
  "C: MotionType. Argument order is first/next for each voice."
  (cond ((and (= p1 p2) (= p3 p4)) :none)
        ((or (= p1 p2) (= p3 p4)) :oblique)
        ((plusp (* (- p2 p1) (- p4 p3))) :direct)
        (t :contrary)))

(defun direct-motion-to-perfect-consonance-p (p1 p2 p3 p4)
  "C: DirectMotionToPerfectConsonance. Note C's p4-p2 interval."
  (and (aref *perfect-consonances* (mod (abs (- p4 p2)) 12))
       (eq (motion-type p1 p2 p3 p4) :direct)))

(defun consecutive-skips-in-same-direction-p (p1 p2 p3)
  (and (or (< p1 p2 p3) (> p1 p2 p3))
       (skip-p (- p2 p1)) (skip-p (- p3 p2))))

(defun out-of-range-p (pitch) (or (> pitch 72) (< pitch 24)))
(defun extreme-range-p (pitch) (or (> pitch 69) (< pitch 27)))

;;; fux.c: Ctrpt/Onset/Dur/TotalNotes through TooMuchOfInterval
;;; In place of four C global arrays, state travels explicitly.
;;; State slots are internal; no CLOS or custom macros are needed.
;;; Historical indexing is retained: cantus voice 0, notes start at 1.
(defun make-fux-state (&key (mode :dorian) (base-pitch 0) (total-time 0))
  (vector base-pitch mode total-time
          (make-array '(128 6) :initial-element 0)
          (make-array '(128 6) :initial-element 0)
          (make-array '(128 6) :initial-element 0)
          (make-array 6 :initial-element 0)))

(defun pitch-at (state note voice) (aref (aref state 3) note voice))
(defun (setf pitch-at) (pitch state note voice)
  (setf (aref (aref state 3) note voice) pitch))
(defun set-pitch (state note voice pitch)
  (setf (pitch-at state note voice) pitch))
(defun onset-at (state note voice) (aref (aref state 4) note voice))
(defun set-onset (state note voice onset)
  (setf (aref (aref state 4) note voice) onset))
(defun duration-at (state note voice) (aref (aref state 5) note voice))
(defun set-duration (state note voice duration)
  (setf (aref (aref state 5) note voice) duration))
(defun total-notes (state voice) (aref (aref state 6) voice))
(defun set-total-notes (state voice count)
  (setf (aref (aref state 6) voice) count))

(defun last-note-p (state n voice) (= n (total-notes state voice)))
(defun first-note-p (n) (= n 1))
(defun next-to-last-note-p (state n voice) (= n (1- (total-notes state voice))))

(defun total-range (state current-note candidate voice)
  "C: TotalRange. Candidate plus pitches of notes 1..current-note-1."
  (loop with lo = candidate with hi = candidate
        for i from 1 below current-note
        for pitch = (pitch-at state i voice)
        do (setf lo (min lo pitch) hi (max hi pitch))
        finally (return (- hi lo))))

(defun cantus (state note voice)
  "C: Cantus; onset >> 3 translates to floor division by 8."
  (pitch-at state (1+ (floor (onset-at state note voice) 8)) 0))

(defun voice-index-at (state time voice)
  "C: VIndex; return last index when no earlier note spans TIME."
  (loop for i from 1 below (total-notes state voice)
        when (and (<= (onset-at state i voice) time)
                  (> (+ (onset-at state i voice)
                        (duration-at state i voice)) time))
          return i
        finally (return (total-notes state voice))))

(defun other-voice (state note voice other-voice-number)
  "C: Other."
  (pitch-at state (voice-index-at state (onset-at state note voice)
                                 other-voice-number)
            other-voice-number))

(defun bass (state note voice)
  "C: Bass: cantus and previously constructed counterpoint voices."
  (loop with lowest = (cantus state note voice)
        for j from 1 below voice
        do (setf lowest (min lowest (other-voice state note voice j)))
        finally (return lowest)))

(defun beat8 (time) (mod time 8))
(defun downbeat-p (state note voice) (zerop (beat8 (onset-at state note voice))))
(defun upbeat-p (state note voice) (not (downbeat-p state note voice)))

(defun pitch-repeats (state current-note candidate voice)
  (loop for i from 1 below current-note
        count (= candidate (pitch-at state i voice))))

(defun interval-size (interval)
  "C: Size. C leaves unsupported intervals uninitialized; here signal error."
  (let* ((n (abs interval))
         (size (case n (0 0) ((1 2) 2) ((3 4) 3) (5 4) (7 5)
                      (8 6) (12 8))))
    (unless size (error "C Size undefined for semitone interval ~D" interval))
    (if (plusp interval) size (- size))))

(defun too-much-of-interval-p (state current-note candidate voice)
  "C: TooMuchOfInterval. Ints[k] excludes candidate; tie-breaking retained."
  (let ((counts (make-array 17 :initial-element 0)))
    (loop for i from 2 below current-note
          for k = (+ 8 (interval-size (- (pitch-at state i voice)
                                         (pitch-at state (1- i) voice))))
          do (incf (aref counts k)))
    (let ((k (+ 8 (interval-size (- candidate
                                    (pitch-at state (1- current-note) voice)))))
          (min-index 0))
      (loop for i from 1 below 17
            when (and (/= i k) (> (aref counts i) (aref counts min-index)))
              do (setf min-index i))
      (> (aref counts k) (+ (aref counts min-index) 6)))))

;;; fux.c: ADissonance. Dissonance depends on species and rhythmic location.
;;; C returns 0/1; Lisp uses NIL/T.
(defun dissonance-p (state interval current-note candidate voice species)
  (let* ((is-dissonance (aref *dissonances* interval))
         (duration (duration-at state current-note voice))
         (previous (and (> current-note 1)
                        (pitch-at state (1- current-note) voice))))
    (cond ((or (= species 1) (= duration 8)) is-dissonance)
          ((= species 2)
           (and is-dissonance
                (or (downbeat-p state current-note voice)
                    (not (step-p (- candidate previous))))))
          ((= species 3)
           (and is-dissonance
                (or (downbeat-p state current-note voice)
                    (first-note-p current-note)
                    (last-note-p state current-note voice)
                    (not (step-p (- candidate previous))))))
          ((= species 4)
           (and is-dissonance
                (or (upbeat-p state current-note voice)
                    (first-note-p current-note)
                    (last-note-p state current-note voice)
                    (/= candidate previous))))
          ((= species 5)
           (and is-dissonance
                (if (downbeat-p state current-note voice)
                    (/= candidate previous)
                    (not (step-p (- candidate previous))))))
          (t nil))))

(defun run-basic-tests ()
  "Sanity checks for the transcribed primitives (NOT the 11 exs tests)."
  (assert (in-mode 50 :dorian))
  (assert (not (in-mode 49 :dorian)))
  (assert (bad-melody-p -8))
  (assert (not (bad-melody-p 8)))
  (assert (eq :direct (motion-type 40 42 50 55)))
  (assert (eq :contrary (motion-type 40 42 50 48)))
  (assert (eq :oblique (motion-type 40 40 50 48)))
  (assert (eq :none (motion-type 40 40 50 50)))
  (assert (direct-motion-to-perfect-consonance-p 40 41 47 48))
  (assert (consecutive-skips-in-same-direction-p 40 43 47))
  (assert (not (consecutive-skips-in-same-direction-p 40 42 45)))
  (assert (out-of-range-p 73))
  (assert (not (out-of-range-p 72)))
  (let ((state (make-fux-state)))
    (set-total-notes state 0 3)
    (set-total-notes state 1 3)
    (set-pitch state 1 0 50)
    (set-pitch state 2 0 52)
    (set-pitch state 1 1 38)
    (set-onset state 1 1 0)
    (set-onset state 2 1 8)
    (set-duration state 1 1 8)
    (assert (= (cantus state 2 1) 52))
    (assert (= (pitch-repeats state 2 38 1) 1))
    (assert (= (total-range state 2 45 1) 7))
    (assert (dissonance-p state 1 2 40 1 1)))
  t)
