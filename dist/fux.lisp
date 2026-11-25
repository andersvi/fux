;;;; FUX Common Lisp counterpoint generator.
;;;; Generated from src/ by tools/build.py; edit src/ instead.
;;;; See examples/ for interactive examples.
;;;; Sources: Bill Schottstaedt, Automatic Species Counterpoint,
;;;; CCRMA report STAN-M-19 (1984); Automatic Counterpoint,
;;;; in Current Directions in Computer Music Research, pp. 199-214
;;;; (MIT Press, 1989). See README.md for bibliographic details.


;;;; fux: FUX implementation.

(defpackage :fux
  (:use :cl)
  (:export :make-fux-state :in-mode :bad-melody-p :motion-type
           :direct-motion-to-perfect-consonance-p
           :consecutive-skips-in-same-direction-p :out-of-range-p
           :extreme-range-p :total-range :cantus :other-voice :bass
           :pitch-repeats :too-much-of-interval-p :dissonance-p
           :set-pitch :pitch-at :set-onset :set-duration :set-total-notes
          ))

(in-package :fux)

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


;;;; first-species: FUX implementation.

(in-package :fux)

(defparameter *first-species-penalties*
  '((:infinity . 1000000) (:bad . 100) (:real-bad . 200)
    (:out-of-range . 200) (:extreme-range . 5) (:cross-above-cantus . 1000000)
    (:out-of-mode . 1000000) (:bad-cadence . 1000000)
    (:doubled-leading-tone . 1000000) (:no-leading-tone . 1000000)
    (:dissonance . 1000000) (:direct-to-octave . 200)
    (:direct-to-fifth . 200) (:parallel-fifth . 1000000)
    (:parallel-unison . 1000000) (:no-motion-against-octave . 34)
    (:bad-melody . 1000000) (:end-on-perfect . 1000000)
    (:direct-motion . 1) (:compound . 1) (:two-skips . 1)
    (:two-skips-not-in-triad . 3) (:skip-to-octave . 8)
    (:skip-from-unison . 4) (:skip-preceded-same-dir . 1)
    (:fifth-preceded-same-dir . 3) (:sixth-preceded-same-dir . 8)
    (:skip-followed-same-dir . 3) (:fifth-followed-same-dir . 8)
    (:sixth-followed-same-dir . 34) (:melodic-boredom . 1)
    (:melodic-tritone . 8) (:tenth-to-octave . 8)
    (:over-octave . 100) (:over-twelfth . 1000000)
    (:two-repeated-notes . 2) (:three-repeated-notes . 4)
    (:four-repeated-notes . 7) (:unresolved-leading-tone . 1000000)
    (:perfect-consonance . 2) (:unison . 100)
    (:octave-leap . 5) (:sixth-leap . 2)
    (:upper-neighbor . 1) (:lower-neighbor . 1)
    (:three-skips . 3) (:leap-at-cadence . 13)
    (:repetition-upbeat . 100) (:lydian-cadential-tritone . 13)
    (:vertical-tritone . 2)))

(defvar *fux-penalties* nil)

(setf *fux-penalties*
      (append (copy-list *first-species-penalties*)
              (remove-if (lambda (entry)
                           (assoc (car entry) *first-species-penalties*))
                         *fux-penalties*)))

(defun penalty (name)
  (let ((entry (assoc name *fux-penalties*)))
    (if entry (cdr entry) (error "Unknown penalty ~S" name))))

(defun doubled-p (state pitch-class note voice)
  "C: Doubled. Compare the preceding voices at the candidate's onset."
  (loop for other-voice-number below voice
        thereis (= (mod (other-voice state note voice other-voice-number) 12)
                  pitch-class)))

(defun check-first-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate)
                                                   (ficta-mode-exemption nil))
  "C: Check for first species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
Only VOICE=1, NUM-PARTS=1 are supported in this phase.
FICTA-MODE-EXEMPTION bypasses only the ordinary non-cadential mode gate.
It is for a separately verified SaveResults alteration, not search."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (= voice 1) (= num-parts 1))
    (error "Only first species / two voices are implemented"))
  (let* ((other0 (cantus state note voice))
         (other1 (cantus state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (or ficta-mode-exemption (in-mode pitch mode))
           (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-first-species value))
      (when (dissonance-p state int-class note candidate voice 1)
        (add :dissonance))
      (when (cutoff-p) (return-from check-first-species value))
      
      
      (when (= note 1) (return-from check-first-species value))
      (when (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0)
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-first-species value))
      (when (and (= int-class last-int-class) (= mel-int 0))
        (add :no-motion-against-octave))
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-first-species value))
      (when (and last-p (/= int-class 0)) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-first-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-first-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (= interval 0) (add :unison))
      (when (cutoff-p) (return-from check-first-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2)))))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; search: FUX implementation.

(in-package :fux)

(defparameter *search-intervals*
  #(0 1 -1 2 -2 3 -3 0 4 -4 5 7 -5 8 12 -7 -12))

(defun make-first-species-cantus (cantus start mode)
  "C: AnySpecies, restricted to one generated voice, first species.
CANTUS and START are absolute semitone pitches."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (<= 2 n 127)
      (error "Cantus length must be between 2 and 127"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 n)
    (loop for pitch in cantus
          for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8)
             (set-onset state i 1 (* 8 (1- i)))
             (set-duration state i 1 8))
    (set-pitch state 1 1 (- start base))
    state))

(defun first-species-candidates (state note current-penalty best-penalty
                                            crossing-policy &optional provenance)
  "C: Look plus SaveIndx (one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-first-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) 
             (when provenance (setf (aref provenance note) nil))
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun first-species-cadence-ficta (state)
  "C: SaveResults. Mutates the candidate line before copying it to output.
The change is intentionally *not* undone by the search: C does not undo it."
  (let* ((n (total-notes state 1))
         (last-pitch (mod (pitch-at state (1- n) 1) 12))
         (mode (aref state 1)))
    (unless (in-mode last-pitch mode)
      (loop for k from 2 below (1- n)
            for i = (- n k)
            for pitch = (mod (pitch-at state i 1) 12)
            for next-diff = (- (pitch-at state (1+ i) 1)
                               (pitch-at state i 1))
            for prior-diff = (abs (- (pitch-at state i 1)
                                     (pitch-at state (1- i) 1)))
            do (when (or (and (< pitch 8) (/= pitch 0))
                         (skip-p next-diff)
                         (member prior-diff '(0 5 7 12)))
                 (return))
               
               
               (when (= (mod (other-voice state i 1 0) 12) 11)
                 (return))
               (when (member (- (pitch-at state (1- n) 1)
                                (pitch-at state i 1)) '(1 3))
                 (incf (aref (aref state 3) i 1))))))
  state)

(defun line-from-state (state)
  (loop with base = (aref state 0)
        for i from 1 to (total-notes state 1)
        collect (+ base (pitch-at state i 1))))

(defun %score-first-species-line (cantus pitches mode ficta-positions)
  "Evaluate pitches sequentially; only listed notes bypass the mode gate."
  (unless (= (length cantus) (length pitches))
    (error "Cantus and counterpoint must have the same length"))
  (let* ((state (make-first-species-cantus cantus (first pitches) mode))
         (base (aref state 0))
         (locals (list 0)))
    (loop for pitch in (rest pitches)
          for note from 2
          for candidate = (- pitch base)
          do (push (check-first-species state note candidate
                                        :limit 10000000
                                        :crossing-policy :candidate
                                        :ficta-mode-exemption
                                        (not (null (member note ficta-positions))))
                   locals)
             (set-pitch state note 1 candidate))
    (setf locals (nreverse locals))
    (values (reduce #'+ locals) locals)))

(defun score-first-species-line (cantus pitches &key (mode :dorian))
  "Fresh literal Check of an absolute-pitch first-species line.
Return total and local penalties, with zero for the fixed first note.
Evaluate against the installed prefix using candidate crossing; do not
apply or exempt ficta. A large result is not a musical validity verdict."
  (%score-first-species-line cantus pitches mode nil))

(defun score-first-species-post-ficta (cantus raw notated
                                      &key (mode :dorian) provenance)
  "Recheck NOTATED with a narrow exemption for this save's proven ficta.
RAW must reproduce NOTATED by one call to SaveResults' ficta pass.
Return total, local penalties, and the one-based positions exempted from
the ordinary mode gate. PROVENANCE is a list of (note before after) from
the generator; earlier inherited alterations require that extra history.
Other melodic and harmonic checks still use the notated pitches."
  (unless (and (= (length cantus) (length raw))
               (= (length raw) (length notated)))
    (error "Cantus, raw and notated lines must have the same length"))
  (let* ((state (make-first-species-cantus cantus (first raw) mode))
         (base (aref state 0)))
    (loop for pitch in (rest raw)
          for note from 2
          do (set-pitch state note 1 (- pitch base)))
    (first-species-cadence-ficta state)
    (unless (equal (line-from-state state) notated)
      (error "Notated line does not follow from one SaveResults ficta pass"))
    (let* ((raised (loop for before in raw for after in notated
                         for note from 1
                         unless (= before after) collect note))
           (exemptions (copy-list raised)))
      (dolist (entry provenance)
        (unless (and (listp entry) (= (length entry) 3)
                     (integerp (first entry))
                     (<= 2 (first entry) (length raw))
                     (integerp (second entry)) (integerp (third entry))
                     (= (third entry) (1+ (second entry)))
                     (= (nth (1- (first entry)) notated) (third entry))
                     (or (member (first entry) raised)
                         (= (nth (1- (first entry)) raw) (third entry))))
          (error "Invalid ficta provenance: ~S" entry))
        (pushnew (first entry) exemptions))
      (setf exemptions (sort exemptions #'<))
      (multiple-value-bind (total locals)
          (%score-first-species-line cantus notated mode exemptions)
        (values total locals exemptions)))))

(defun generate-first-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 1250)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate)
                                             (save-state-policy :historic)
                                             (result-format :legacy))
  "Port of C BestFitFirst/Look/SaveResults for one voice in first species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
SAVE-STATE-POLICY :RESTORE removes SaveResults' work-array mutation after
the notated result is copied; :HISTORIC keeps it for later search branches.
No recomputation of a global optimum is implied."
  (unless (member result-format '(:legacy :with-ficta))
    (error "Unknown result format: ~S" result-format))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (unless (member save-state-policy '(:historic :restore))
    (error "Unknown save-state policy: ~S" save-state-policy))
  (let* ((state (make-first-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 99/100)
         (branches 0)
         (provenance (make-array (1+ total) :initial-element nil))
         (results nil))
    
    
    
    
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (let* ((detailed (eq result-format :with-ficta))
                      (raw (when (or detailed
                                     (eq save-state-policy :restore))
                            (line-from-state state))))
                 (first-species-cadence-ficta state)
                 (setf best-penalty cost
                       max-penalty (bound (min (* best-penalty penalty-ratio)
                                               max-penalty)))
                 (let* ((line (line-from-state state))
                        (changes (when raw
                                   (loop for before in raw for after in line
                                         for index from 1
                                         unless (= before after)
                                           collect (list index before after))))
                        (notated-penalty nil)
                        (notated-locals nil))
                   (dolist (entry changes)
                     (setf (aref provenance (first entry)) entry))
                   (when detailed
                     (multiple-value-setq (notated-penalty notated-locals)
                       (score-first-species-line cantus line :mode mode)))
                   (push (if detailed
                             (list :search-penalty cost
                                   :raw-pitches raw
                                   :notated-pitches line
                                   :ficta-changes changes
                                   :ficta-provenance
                                   (loop for note from 1 to total
                                         when (aref provenance note)
                                           collect (copy-list
                                                    (aref provenance note)))
                                   :notated-penalty notated-penalty
                                   :notated-local-penalties notated-locals)
                             (list cost line))
                         results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))
                 (when (eq save-state-policy :restore)
                   (loop for pitch in raw for note from 1
                         do (set-pitch state note 1
                                       (- pitch (aref state 0))))
                   (fill provenance nil)))))
             (visit-node (note accumulated)
               
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (first-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy provenance)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       
                       
                       
                       (when (< total-cost
                                (if (and (= note total)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         
                         
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (setf (aref provenance note) nil)
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       
                       
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; poly-first-species: FUX implementation.

(in-package :fux)

(defparameter *poly-penalties*
  '((:cross-below-bass . 1000000)
    (:augmented-interval . 1000000)
    (:upper-voices-too-far-apart . 1)
    (:inner-voices-direct-to-perfect . 21)
    (:inner-voices-direct-to-tritone . 13)
    (:not-contrary-to-others . 1)
    (:third-doubled . 5) (:sixth-doubled . 5)
    (:tripled-bass . 3) (:fifth-doubled . 3)
    (:not-triad . 34) (:all-voices-skip . 8)
    (:six-five-chord . 1000000)))

(dolist (entry *poly-penalties*)
  (unless (assoc (car entry) *fux-penalties*)
    (setf *fux-penalties* (append *fux-penalties* (list entry)))))

(defun chord-interval-class (difference)
  "C: AddInterval; classify pitch above the bass."
  (case (mod difference 12)
    (0 0) ((1 2) 2) ((3 4) 3) ((5 6) 4) (7 5)
    ((8 9) 6) ((10 11) 7)))

(defun other-voice-check-first-species (state note candidate voice num-parts limit)
  "C: OtherVoiceCheck, restricted to first species."
  (let* ((low (bass state note voice))
         (interval (mod (- candidate low) 12))
         (last-cp (pitch-at state (1- note) voice))
         (actual-pitch (mod candidate 12))
         (counts (make-array 8 :initial-element 0))
         (value 0)
         (all-skip (skip-p (- candidate last-cp))))
    (labels ((add (name) (incf value (penalty name)))
             (count-interval (difference)
               (incf (aref counts (chord-interval-class difference)))))
      (when (<= candidate low) (add :cross-below-bass))
      (when (and (= interval 4) (not (in-mode low (aref state 1))))
        (add :augmented-interval))
      (when (>= value limit) (return-from other-voice-check-first-species value))
      (count-interval interval)
      (loop for k below voice
            for other0 = (other-voice state note voice k)
            for other1 = (other-voice state (1- note) voice k)
            for last-int = (mod (abs (- other1 last-cp)) 12)
            for this-int = (mod (abs (- other0 candidate)) 12)
            do (unless (skip-p (- other0 other1)) (setf all-skip nil))
               (count-interval (- other0 low))
               (when (and (not (last-note-p state note voice))
                          (= other0 candidate))
                 (add :unison))
               (when (and (/= other0 low) (>= (abs (- candidate other0)) 19))
                 (add :upper-voices-too-far-apart))
               (when (= last-int this-int)
                 (cond ((= this-int 0) (add :parallel-unison))
                       ((= this-int 7) (add :parallel-fifth))))
               (when (and (> note 2) (= this-int 0)
                          (zerop (mod (abs (- (pitch-at state (- note 2) voice)
                                               (other-voice state (- note 2) voice k))) 12)))
                 (add :parallel-unison))
               (when (>= value limit) (return-from other-voice-check-first-species value))
               (when (= this-int 6) (add :vertical-tritone))
               (when (and (not (last-note-p state note voice))
                          (direct-motion-to-perfect-consonance-p
                           last-cp candidate other1 other0))
                 (add :inner-voices-direct-to-perfect))
               (when (and (= actual-pitch 10) (= (mod other0 12) 11))
                 (add :doubled-leading-tone))
               (when (and (eq (motion-type last-cp candidate other1 other0) :direct)
                          (= this-int 6))
                 (add :inner-voices-direct-to-tritone))
               (when (and (= actual-pitch 3) (= (mod other0 12) 11))
                 (add :augmented-interval))
               (unless (eq (motion-type last-cp candidate other1 other0) :contrary)
                 (add :not-contrary-to-others)))
      (when (> (aref counts 3) 1) (add :third-doubled))
      (when (and (zerop (aref counts 3)) (> (aref counts 6) 1))
        (add :sixth-doubled))
      (when (> (aref counts 0) 2) (add :tripled-bass))
      (when (> (aref counts 5) 1) (add :fifth-doubled))
      (when (and (= voice num-parts)
                 (not (last-note-p state note voice))
                 (zerop (aref counts 3)) (zerop (aref counts 6)))
        (add :not-triad))
      (when (and (= voice num-parts) all-skip) (add :all-voices-skip))
      (when (and (plusp (aref counts 5)) (plusp (aref counts 6)))
        (add :six-five-chord))
      value)))

(defun check-poly-first-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check, first species, including OtherVoiceCheck for VOICE>1.
Returns the local penalty, preserving early cutoff order."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (integerp num-parts) (<= 2 num-parts 5)
               (integerp voice) (<= 1 voice num-parts))
    (error "Expected 2..5 generated voices and a voice within that range"))
  (let* ((other0 (if (= voice 1) (cantus state note voice)
                      (bass state note voice)))
         (other1 (if (= voice 1) (cantus state (1- note) voice)
                      (bass state (1- note) voice)))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (= num-parts 1)
                 (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (when (= voice num-parts) (add :no-leading-tone))))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (when (dissonance-p state int-class note candidate voice 1)
        (add :dissonance))
      (when (cutoff-p) (return-from check-poly-first-species value))
      
      (when (> voice 1)
        (incf value (other-voice-check-first-species
                     state note candidate voice num-parts limit)))
      (when (= note 1) (return-from check-poly-first-species value))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (when (and (or (not last-p) (= num-parts 1))
                 (direct-motion-to-perfect-consonance-p
                  last-cp candidate other1 other0))
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (when (and (= num-parts 1) (= int-class last-int-class) (= mel-int 0))
        (add :no-motion-against-octave))
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (when (and last-p (/= int-class 0)
                 (or (= num-parts 1) (minusp interval)
                     (not (member int-class '(4 7)))))
        (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (= num-parts 1) (tenth-p (- other1 last-cp))
                 (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-poly-first-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (and (= num-parts 1) (= interval 0)) (add :unison))
      (when (cutoff-p) (return-from check-poly-first-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (= num-parts 1) (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (when (= num-parts 1)
        (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
          (incf value (max 0 (* 3 (- cross 2))))))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; poly-search: FUX implementation.

(in-package :fux)

(defun make-poly-first-species-state (cantus starts mode)
  "As in C AnySpecies, STARTS contains one absolute pitch per generated voice."
  (unless (= (length starts) 2)
    (error "EXS11 needs exactly two starting pitches"))
  (let ((state (make-first-species-cantus cantus (first starts) mode))
        (n (length cantus)))
    (set-total-notes state 2 n)
    (loop for i from 1 to n
          do (set-duration state i 2 8)
             (set-onset state i 2 (* 8 (1- i))))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun poly-first-species-candidates (state note accumulated best-penalty)
  "C: Look/SaveIndx for two simultaneous voices. Return top 16 triples
(COST INDEX-VOICE-1 INDEX-VOICE-2), keeping discovery order on ties."
  (let ((limit (- best-penalty accumulated))
        (options nil)
        (previous1 (pitch-at state (1- note) 1))
        (previous2 (pitch-at state (1- note) 2)))
    (loop for i from 1 to 16
          for pitch1 = (+ previous1 (aref *search-intervals* i))
          for cost1 = (check-poly-first-species state note pitch1
                                                :voice 1 :num-parts 2
                                                :limit limit)
          do (set-pitch state note 1 pitch1) 
             (when (< cost1 limit)
               (loop for j from 1 to 16
                     for pitch2 = (+ previous2 (aref *search-intervals* j))
                     for cost2 = (check-poly-first-species
                                  state note pitch2 :voice 2 :num-parts 2
                                  :limit (- limit cost1))
                     for cost = (+ cost1 cost2)
                     do (set-pitch state note 2 pitch2)
                        (when (< cost limit)
                          (push (list cost i j) options)))))
    
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun poly-first-species-cadence-ficta (state)
  "C: SaveResults for all generated voices. Historically mutates work state."
  (let ((mode (aref state 1)))
    (loop for voice from 1 to 2
          for n = (total-notes state voice)
          do (unless (in-mode (mod (pitch-at state (1- n) voice) 12) mode)
               (loop for k from 2 below (1- n)
                     for i = (- n k)
                     for pitch = (mod (pitch-at state i voice) 12)
                     for next-diff = (- (pitch-at state (1+ i) voice)
                                        (pitch-at state i voice))
                     for previous-diff = (abs (- (pitch-at state i voice)
                                                 (pitch-at state (1- i) voice)))
                     do (when (or (and (< pitch 8) (/= pitch 0))
                                  (skip-p next-diff)
                                  (member previous-diff '(0 5 7 12)))
                          (return))
                        (when (loop for other from 0 to 2
                                      thereis (and (/= other voice)
                                                   (= (mod (other-voice state i voice other) 12)
                                                      11)))
                          (return))
                        (when (member (- (pitch-at state (1- n) voice)
                                         (pitch-at state i voice)) '(1 3))
                          (incf (aref (aref state 3) i voice))))))
    state))

(defun poly-lines-from-state (state)
  (loop with base = (aref state 0)
        for voice from 1 to 2
        collect (loop for i from 1 to (total-notes state voice)
                      collect (+ base (pitch-at state i voice)))))

(defun generate-poly-first-species (cantus starts
                                    &key (mode :dorian) (stream nil)
                                      (branch-limit 1000)
                                      (integer-bounds nil)
                                      (terminal-bound :best-fit))
  "EXS11-style search. Results: (COST (VOICE-1...) (VOICE-2...)).
TERMINAL-BOUND :MAX-PENALTY and INTEGER-BOUNDS T approach historical C.
The default retains the CL search improvement policy."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Invalid terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-poly-first-species-state cantus starts mode))
         (n (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 1000000)
         (penalty-ratio 98/100)
         (branches 0)
         (results nil))
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (poly-first-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* cost penalty-ratio)
                                             max-penalty)))
               (let* ((lines (poly-lines-from-state state))
                      (record (cons cost lines)))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] " cost)
                   (dolist (line lines)
                     (format stream "~{~D ~}~%" line)))))
             (visit-node (note accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (poly-first-species-candidates
                                 state note accumulated best-penalty)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (first option))))
                       (when (< total-cost
                                (if (and (= note n)
                                         (eq terminal-bound :best-fit))
                                    best-penalty max-penalty))
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (second option))))
                         (set-pitch state note 2
                                    (+ (pitch-at state (1- note) 2)
                                       (aref *search-intervals* (third option))))
                         (if (< note n)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       (when (= note 2)
                         (setf max-penalty
                               (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; second-species: FUX implementation.

(in-package :fux)

(defparameter *second-species-penalties*
  '((:unison-downbeat . 3)
    (:dissonance-not-filling-third . 1000000)
    (:direct-perfect-on-downbeat . 1000000)))

(dolist (entry *second-species-penalties*)
  (unless (assoc (car entry) *fux-penalties*)
    (push entry *fux-penalties*)))

(defun check-second-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for second species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
Only VOICE=1, NUM-PARTS=1 are supported in this phase."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (= voice 1) (= num-parts 1))
    (error "Only second species / two voices are implemented"))
  (let* ((other0 (cantus state note voice))
         (other1 (cantus state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (cantus state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         
         (unless (and (= note (- (total-notes state voice) 2))
                      (eq mode :aeolian)
                      (> candidate other0)
                      (= int-class 7))
           (unless (in-mode pitch mode) (add :out-of-mode))))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-second-species value))
      (when (dissonance-p state int-class note candidate voice 2)
        (add :dissonance))
      (when (cutoff-p) (return-from check-second-species value))
      
      (when (and penultimate-p (member pitch '(10 11)))
        (if (or (not (eq mode :phrygian)) (>= interval 0))
            (unless (= last-int-class 7) (add :bad-cadence))
            (unless (= last-int-class 8) (add :bad-cadence))))
      
      (when (= note 1) (return-from check-second-species value))
      (when (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0)
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-second-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-second-species value))
      (when (and last-p (/= int-class 0)) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-second-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-second-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (= interval 0) (add :unison))
      (when (cutoff-p) (return-from check-second-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2)))))
      
      (when (downbeat-p state note voice)
        (when (and (zerop mel-int) (not last-p))
          (add :unison-downbeat))
        (when (and (aref *dissonances* last-int-class)
                   (or (not (step-p mel-int)) (not same-dir)))
          (add :dissonance-not-filling-third))
        (when (and (> note 2)
                   (direct-motion-to-perfect-consonance-p
                    last-cp2 candidate other2 other0)
                   (< (abs last-mel-int) 5))
          (add :direct-perfect-on-downbeat)))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; second-search: FUX implementation.

(in-package :fux)

(defun make-second-species-cantus (cantus start mode)
  "C AnySpecies: exactly 2*N-1 counterpoint notes, last duration 8."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (count (1- (* 2 n)))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (<= 2 n 64)
      (error "Cantus length must be 2..64 for this state representation"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 count)
    (loop for pitch in cantus for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8))
    (loop for i from 1 to count
          do (set-onset state i 1 (* 4 (1- i)))
             (set-duration state i 1 (if (= i count) 8 4)))
    (set-pitch state 1 1 (- start base))
    state))

(defun second-species-candidates (state note current-penalty best-penalty
                                            crossing-policy)
  "C: Look plus SaveIndx (second species, one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-second-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) 
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun second-species-cadence-ficta (state)
  "C: SaveResults. Mutates the candidate line before copying it to output.
The change is intentionally *not* undone by the search: C does not undo it."
  (let* ((n (total-notes state 1))
         (last-pitch (mod (pitch-at state (1- n) 1) 12))
         (mode (aref state 1)))
    (unless (in-mode last-pitch mode)
      (loop for k from 2 below (1- n)
            for i = (- n k)
            for pitch = (mod (pitch-at state i 1) 12)
            for next-diff = (- (pitch-at state (1+ i) 1)
                               (pitch-at state i 1))
            for prior-diff = (abs (- (pitch-at state i 1)
                                     (pitch-at state (1- i) 1)))
            do (when (or (and (< pitch 8) (/= pitch 0))
                         (skip-p next-diff)
                         (member prior-diff '(0 5 7 12)))
                 (return))
               
               
               (when (= (mod (other-voice state i 1 0) 12) 11)
                 (return))
               (when (member (- (pitch-at state (1- n) 1)
                                (pitch-at state i 1)) '(1 3))
                 (incf (aref (aref state 3) i 1))))))
  state)

(defun second-species-line-from-state (state)
  (loop with base = (aref state 0)
        for i from 1 to (total-notes state 1)
        collect (+ base (pitch-at state i 1))))

(defun generate-second-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 1000)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate))
  "Port of C BestFitFirst/Look/SaveResults for one voice in second species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
No recomputation of a global optimum is implied."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-second-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 98/100)
         (branches 0)
         (results nil))
    
    
    
    
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (second-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* best-penalty penalty-ratio)
                                             max-penalty)))
               (let ((line (second-species-line-from-state state)))
                 (push (list cost line) results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))))
             (visit-node (note accumulated)
               
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (second-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       
                       
                       
                       (when (< total-cost
                                (if (and (= note total)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         
                         
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       
                       
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; third-species: FUX implementation.

(in-package :fux)

(defparameter *third-species-penalties*
  '((:unison-on-beat4 . 3)
    (:skip-to-downbeat . 1)
    (:downbeat-unison . 100)
    (:nota-cambiata . 1000000)
    (:unison-downbeat . 3)
    (:dissonance-not-filling-third . 1000000)
    (:direct-perfect-on-downbeat . 1000000)))

(dolist (entry *third-species-penalties*)
  (unless (assoc (car entry) *fux-penalties*)
    (push entry *fux-penalties*)))

(defun check-third-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for third species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
Only VOICE=1, NUM-PARTS=1 are supported in this phase."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (= voice 1) (= num-parts 1))
    (error "Only third species / two voices are implemented"))
  (let* ((other0 (cantus state note voice))
         (other1 (cantus state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (cantus state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-third-species value))
      (when (dissonance-p state int-class note candidate voice 3)
        (add :dissonance))
      (when (cutoff-p) (return-from check-third-species value))
      
      
      (when (and (= (beat8 (onset-at state note voice)) 6)
                 (= candidate last-cp))
        (add :unison-on-beat4))
      (when (downbeat-p state note voice)
        (when (skip-p mel-int) (add :skip-to-downbeat))
        (when (and (> note 2) (member int-class '(0 7))
                   (= (mod (abs (- last-cp4
                                    (cantus state (- note 4) voice))) 12)
                      int-class))
          (add :downbeat-unison)))
      
      (when (and (= (beat8 (onset-at state note voice)) 6)
                 (member (abs last-mel-int) '(3 4))
                 (aref *dissonances*
                       (mod (abs (- last-cp2 other2)) 12))
                 (or (minusp mel-int)
                     (not (member (abs mel-int) '(1 2)))))
        (add :nota-cambiata))
      (when (cutoff-p) (return-from check-third-species value))
      
      (when (and (> note 1) (aref *dissonances* last-int-class))
        (case (beat8 (onset-at state note voice))
          ((0 6)
           (when (or (not (step-p mel-int))
                     (not (step-p last-mel-int))
                     (minusp (* mel-int last-mel-int)))
             (add :dissonance)))
          (2 (add :dissonance))
          (4
           (if (or (not (step-p last-mel-int))
                   (> (abs mel-int) 4) (zerop mel-int)
                   (minusp (* last-mel-int mel-int)))
               (add :dissonance)
               (unless (step-p mel-int)
                 (if (>= interval 0)
                     (unless (member last-int-class '(10 11))
                       (add :dissonance))
                     (unless (= last-int-class 5)
                       (add :dissonance))))))))
      
      
      (when (= note 1) (return-from check-third-species value))
      (when (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0)
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-third-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-third-species value))
      (when (and last-p (/= int-class 0)) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-third-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-third-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (= interval 0) (add :unison))
      (when (cutoff-p) (return-from check-third-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2)))))
      
      (when (downbeat-p state note voice)
        (when (and (zerop mel-int) (not last-p))
          (add :unison-downbeat))
        (when (and (aref *dissonances* last-int-class)
                   (or (not (step-p mel-int)) (not same-dir)))
          (add :dissonance-not-filling-third))
        (when (and (> note 2)
                   (direct-motion-to-perfect-consonance-p
                    last-cp2 candidate other2 other0)
                   (< (abs last-mel-int) 5))
          (add :direct-perfect-on-downbeat)))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; third-search: FUX implementation.

(in-package :fux)

(defun make-third-species-cantus (cantus start mode)
  "C AnySpecies: exactly 4*N-3 counterpoint notes, last duration 8."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (count (- (* 4 n) 3))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (<= 2 n 32)
      (error "Cantus length must be 2..32 for this state representation"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 count)
    (loop for pitch in cantus for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8))
    (loop for i from 1 to count
          do (set-onset state i 1 (* 2 (1- i)))
             (set-duration state i 1 (if (= i count) 8 2)))
    (set-pitch state 1 1 (- start base))
    state))

(defun third-species-candidates (state note current-penalty best-penalty
                                            crossing-policy)
  "C: Look plus SaveIndx (third species, one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-third-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) 
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun third-species-cadence-ficta (state)
  "C: SaveResults. Mutates the candidate line before copying it to output.
The change is intentionally *not* undone by the search: C does not undo it."
  (let* ((n (total-notes state 1))
         (last-pitch (mod (pitch-at state (1- n) 1) 12))
         (mode (aref state 1)))
    (unless (in-mode last-pitch mode)
      (loop for k from 2 below (1- n)
            for i = (- n k)
            for pitch = (mod (pitch-at state i 1) 12)
            for next-diff = (- (pitch-at state (1+ i) 1)
                               (pitch-at state i 1))
            for prior-diff = (abs (- (pitch-at state i 1)
                                     (pitch-at state (1- i) 1)))
            do (when (or (and (< pitch 8) (/= pitch 0))
                         (skip-p next-diff)
                         (member prior-diff '(0 5 7 12)))
                 (return))
               
               
               (when (= (mod (other-voice state i 1 0) 12) 11)
                 (return))
               (when (member (- (pitch-at state (1- n) 1)
                                (pitch-at state i 1)) '(1 3))
                 (incf (aref (aref state 3) i 1))))))
  state)

(defun third-species-line-from-state (state)
  (loop with base = (aref state 0)
        for i from 1 to (total-notes state 1)
        collect (+ base (pitch-at state i 1))))

(defun generate-third-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 750)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate))
  "Port of C BestFitFirst/Look/SaveResults for one voice in third species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
No recomputation of a global optimum is implied."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-third-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 97/100)
         (branches 0)
         (results nil))
    
    
    
    
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (third-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* best-penalty penalty-ratio)
                                             max-penalty)))
               (let ((line (third-species-line-from-state state)))
                 (push (list cost line) results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))))
             (visit-node (note accumulated)
               
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (third-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       
                       
                       
                       (when (< total-cost
                                (if (and (= note total)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         
                         
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       
                       
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; fourth-species: FUX implementation.

(in-package :fux)

(defparameter *fourth-species-penalties*
  '((:nota-ligature . 21)
    (:unresolved-ligature . 1000000)
    (:no-time-for-a-ligature . 1000000)
    (:direct-perfect-on-downbeat . 1000000)))

(dolist (entry *fourth-species-penalties*)
  (unless (assoc (car entry) *fux-penalties*)
    (push entry *fux-penalties*)))

(defun check-fourth-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for fourth species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
Only VOICE=1, NUM-PARTS=1 are supported in this phase."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (= voice 1) (= num-parts 1))
    (error "Only fourth species / two voices are implemented"))
  (let* ((other0 (cantus state note voice))
         (other1 (cantus state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (cantus state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-fourth-species value))
      (when (dissonance-p state int-class note candidate voice 4)
        (add :dissonance))
      (when (cutoff-p) (return-from check-fourth-species value))
      
      
      (when (and (downbeat-p state note voice) (not (zerop mel-int)))
        (add :nota-ligature))
      (when (and (upbeat-p state note voice)
                 (aref *dissonances* last-int-class))
        (unless (member mel-int '(-1 -2))
          (add :unresolved-ligature))
        
        
        (when (and (zerop int-class)
                   (or (minusp interval)
                       (zerop (mod (abs (- (pitch-at state (- note 2) voice)
                                                other2)) 12))))
          (add :no-time-for-a-ligature))
        (when (member int-class '(7 6))
          (add :no-time-for-a-ligature)))
      
      (when (= note 1) (return-from check-fourth-species value))
      (when (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0)
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-fourth-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-fourth-species value))
      (when (and last-p (/= int-class 0)) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-fourth-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-fourth-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (= interval 0) (add :unison))
      (when (cutoff-p) (return-from check-fourth-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2)))))
      
      
      (when (and (downbeat-p state note voice)
                 (> note 2)
                 (direct-motion-to-perfect-consonance-p
                  last-cp2 candidate other2 other0)
                 (< (abs last-mel-int) 5))
        (add :direct-perfect-on-downbeat))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; fourth-search: FUX implementation.

(in-package :fux)

(defun make-fourth-species-cantus (cantus start mode)
  "C AnySpecies: same half-note rhythm as second species, but with ties
  and suspensions evaluated according to the fourth-species rules."
  (make-second-species-cantus cantus start mode))

(defun fourth-species-candidates (state note current-penalty best-penalty
                                            crossing-policy)
  "C: Look plus SaveIndx (fourth species, one voice). Return up to 16 (cost . interval-index)
records, sorted by increasing cost, retaining original order for ties."
  (let ((previous (pitch-at state (1- note) 1))
        (candidates nil))
    (loop for i from 1 to 16
          for pitch = (+ previous (aref *search-intervals* i))
          for local = (check-fourth-species state note pitch
                                           :limit (- best-penalty
                                                     current-penalty)
                                           :crossing-policy crossing-policy)
          for cost = local
          do (set-pitch state note 1 pitch) 
             (when (< cost (- best-penalty current-penalty))
               (push (cons cost i) candidates)))
    (stable-sort (nreverse candidates) #'< :key #'car)))

(defun fourth-species-cadence-ficta (state)
  "C SaveResults: same ficta logic for all one-voice species."
  (second-species-cadence-ficta state))

(defun fourth-species-line-from-state (state)
  (second-species-line-from-state state))

(defun fourth-species-ties (pitches)
  "Return (from-note . to-note) ties in a complete pitch line.
  In this species, equal notes at an odd numbered note index (3,5,...)
  represent a half-note continued across the cantus downbeat."
  (loop for previous in pitches
        for current in (cdr pitches)
        for note from 2
        when (and (oddp note) (= previous current))
          collect (cons (1- note) note)))

(defun generate-fourth-species (cantus start &key (mode :dorian)
                                             (stream nil)
                                             (branch-limit 500)
                                             (integer-bounds nil)
                                             (terminal-bound :best-fit)
                                             (crossing-policy :candidate))
  "Port of C BestFitFirst/Look/SaveResults for one voice in fourth species.
Returns a list of (penalty pitches) in the order improvements were found.
STREAM may be T (standard output) or a stream to print C-style results.
TERMINAL-BOUND is :BEST-FIT to accept any complete improvement, or
:MAX-PENALTY to apply the historical search bound at the final note too.
INTEGER-BOUNDS truncates each bound update; NIL retains exact rationals.
No recomputation of a global optimum is implied."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (let* ((state (make-fourth-species-cantus cantus start mode))
         (total (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 96/100)
         (branches 0)
         (results nil))
    
    
    
    
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (fourth-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min (* best-penalty penalty-ratio)
                                             max-penalty)))
               (let ((line (fourth-species-line-from-state state)))
                 (push (list cost line) results)
                 (when stream
                   (format stream "~% [~D] ~{~D ~}~%" cost line))))
             (visit-node (note accumulated)
               
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (fourth-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       
                       
                       
                       (when (< total-cost
                                (if (and (= note total)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         
                         
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (if (< note total)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       
                       
                       (when (= note 2)
                         (setf max-penalty (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; fifth-species: FUX implementation.

(in-package :fux)

(defparameter *fifth-species-penalties*
  '((:lesser-ligature . 8)
    (:half-untied . 13)
    (:eighth-jump . 100)
    (:unison-upbeat . 21)))

(dolist (entry *fifth-species-penalties*)
  (unless (assoc (car entry) *fux-penalties*)
    (push entry *fux-penalties*)))

(defun check-fifth-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for fifth species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
Only VOICE=1, NUM-PARTS=1 are supported in this phase."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (= voice 1) (= num-parts 1))
    (error "Only fifth species / two voices are implemented"))
  (let* ((other0 (cantus state note voice))
         (other1 (cantus state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (cantus state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-fifth-species value))
      (when (dissonance-p state int-class note candidate voice 5)
        (add :dissonance))
      (when (cutoff-p) (return-from check-fifth-species value))
      
      (let ((beat (beat8 (onset-at state note voice))))
        (when (and (member beat '(6 7)) (= candidate last-cp))
          (add :unison-on-beat4))
        (when (zerop beat)
          (when (skip-p mel-int) (add :skip-to-downbeat))
          (when (and (> note 2) (member int-class '(0 7)))
            
            
            (let ((previous-downbeat
                    (loop for i downfrom (1- note) to 1
                          when (zerop (beat8 (onset-at state i voice)))
                            return i)))
              (when (and previous-downbeat
                         (= (mod (abs (- (pitch-at state previous-downbeat voice)
                                         (cantus state previous-downbeat voice)))
                                 12)
                            int-class))
                (add :downbeat-unison)))))
        
        (when (and (= beat 6) (member (abs last-mel-int) '(3 4))
                   (aref *dissonances*
                         (mod (abs (- last-cp2 other2)) 12))
                   (or (minusp mel-int)
                       (not (step-p mel-int))))
          (add :nota-cambiata))
        (when (cutoff-p) (return-from check-fifth-species value))

        
        (when (and (> note 1) (zerop beat)
                   (/= candidate last-cp)
                   (<= (duration-at state note voice)
                       (duration-at state (1- note) voice)))
          (add :lesser-ligature))
        (when (and (> note 3)
                   (= (duration-at state note voice) 4)
                   (= beat 4)
                   (= (duration-at state (1- note) voice) 2)
                   (= (duration-at state (- note 2) voice) 2))
          (add :half-untied))
        (when (and (= (duration-at state note voice) 1)
                   (zerop beat)
                   (aref *dissonances* int-class))
          (add :dissonance))
        (when (cutoff-p) (return-from check-fifth-species value))

        
        (when (and (> note 1)
                   (aref *dissonances*
                         (mod (abs (- last-cp other1)) 12)))
          (let ((last-dis-int (mod (abs (- last-cp other1)) 12))
                (previous-beat (beat8 (onset-at state (1- note) voice)))
                (previous-duration (duration-at state (1- note) voice))
                (before-previous-duration
                  (duration-at state (- note 2) voice)))
            (case previous-beat
              ((4 6)
               (unless (and (= last-dis-int 5) (zerop mel-int)
                            (= other0 other1) (zerop beat))
                 (when (or (not (step-p mel-int))
                           (not (step-p last-mel-int))
                           (minusp (* mel-int last-mel-int))
                           (= previous-duration 1)
                           (and (= previous-duration 2)
                                (= before-previous-duration 4)))
                   (add :dissonance))))
              ((1 3 5 7)
               (when (or (not (step-p mel-int))
                         (not (step-p last-mel-int))
                         (minusp (* mel-int last-mel-int)))
                 (add :dissonance)))
              (0
               (when (or (= before-previous-duration 1)
                         (< before-previous-duration previous-duration))
                 (add :no-time-for-a-ligature))
               (unless (member mel-int '(-1 -2))
                 (add :unresolved-ligature))
               (when (member int-class '(5 6))
                 (add :no-time-for-a-ligature))
               (when (and (= int-class 7) (minusp interval))
                 (add :no-time-for-a-ligature))
               (when (and (zerop int-class)
                          (zerop (mod (abs (- last-cp2 other2)) 12)))
                 (add :no-time-for-a-ligature))
               (unless (zerop last-mel-int)
                 (add :dissonance)))
              (2
               (if (or (not (step-p last-mel-int))
                       (> (abs mel-int) 4)
                       (zerop mel-int)
                       (= previous-duration 1)
                       (minusp (* last-mel-int mel-int)))
                   (add :dissonance)
                   (unless (step-p mel-int)
                     (if (>= interval 0)
                         (unless (member last-int-class '(10 11))
                           (add :dissonance))
                         (unless (= last-int-class 5)
                           (add :dissonance)))))))))
        (when (and (> note 1)
                   (= (duration-at state (1- note) voice) 1)
                   (not (step-p mel-int)))
          (add :eighth-jump))
        (when (and (> note 1)
                   (= (duration-at state (1- note) voice) 4)
                   (= beat 4)
                   (zerop mel-int))
          (add :unison-upbeat)))

      
      (when (= note 1) (return-from check-fifth-species value))
      (when (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0)
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-fifth-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-fifth-species value))
      (when (and last-p (/= int-class 0)) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (and (> note 30)
                 (> (total-range state note candidate voice) 19))
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-fifth-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-fifth-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (= interval 0) (add :unison))
      (when (cutoff-p) (return-from check-fifth-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2)))))
      
      
      (when (and (downbeat-p state note voice) (> note 2)
                 (direct-motion-to-perfect-consonance-p
                  last-cp2 candidate other2 other0)
                 (< (abs last-mel-int) 5))
        (add :direct-perfect-on-downbeat))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; fifth-search: FUX implementation.

(in-package :fux)

(defparameter *fifth-rhythm-patterns*
  #(()
    (4 4)                   
    (4 2 2)                 
    (2 2 2 2)               
    (2 2 4)                 
    (2 1 1 4)               
    (2 1 1 2 2)             
    (4 2 1 1)               
    (2 1 1 2 1 1)           
    (2 2 2 1 1)))

(defun fifth-rhythm-pattern-indices (bars &key (seed 1))
  "C: GoodRhy, with a deterministic 31-bit LCG and usage counts.
RANDOM(10.0) can produce indexes as high as 19 in the C port even though
patterns 1..9 are valid. We clamp to [1,9] before reading a count;
see README-species5.md. All current reference runs use SEED=1."
  (unless (and (integerp bars) (<= 0 bars))
    (error "Invalid number of fifth-species bars: ~S" bars))
  (let ((rng seed)
        (used (make-array 10 :initial-element 0))
        (chosen nil))
    (dotimes (bar bars (nreverse chosen))
      (declare (ignore bar))
      
      (setf rng (mod (+ (* rng 1103515245) 12345) 2147483648))
      (let* ((bits (mod (floor rng 65536) 32768))
             (raw (floor (* 10 bits) 16384))
             (i (max 1 (min 9 raw)))
             (before (max 1 (1- i)))
             (after (min 9 (1+ i)))
             (choice (cond ((> (aref used i) (aref used before)) before)
                           ((<= (aref used i) (aref used after)) after)
                           (t i))))
        (incf (aref used choice))
        (push choice chosen)))))

(defun fifth-species-durations (cantus-length &key (seed 1) patterns)
  "Return durations (whole=8, half=4, quarter=2, eighth=1).
PATTERNS is an optional explicit list of per-bar indices 1..9."
  (let ((indices (or patterns
                     (fifth-rhythm-pattern-indices (1- cantus-length)
                                                   :seed seed))))
    (unless (= (length indices) (1- cantus-length))
      (error "Expected ~D rhythm patterns, got ~D"
             (1- cantus-length) (length indices)))
    (append (loop for i in indices
                  append (progn
                           (unless (and (integerp i) (<= 1 i 9))
                             (error "Invalid fifth-species pattern: ~S" i))
                           (copy-list (aref *fifth-rhythm-patterns* i))))
            '(8))))

(defun make-fifth-species-cantus (cantus start mode
                                    &key (seed 1) patterns)
  "C: AnySpecies, one moving voice in fifth species.
The rhythm is chosen once, before the pitch search, as in the C port."
  (let* ((n (length cantus))
         (base (mod (car (last cantus)) 12))
         (durs (fifth-species-durations n :seed seed :patterns patterns))
         (count (length durs))
         (state (make-fux-state :mode mode :base-pitch base
                                :total-time (* 8 (1- n)))))
    (unless (and (<= 2 n) (< count 128))
      (error "Cantus/rhythm exceeds the 128-note reference state"))
    (unless (= (reduce #'+ durs) (* 8 n))
      (error "Fifth-species rhythm does not span the cantus"))
    (set-total-notes state 0 n)
    (set-total-notes state 1 count)
    (loop for pitch in cantus for i from 1
          do (set-pitch state i 0 (- pitch base))
             (set-onset state i 0 (* 8 (1- i)))
             (set-duration state i 0 8))
    (loop with onset = 0
          for dur in durs for i from 1
          do (set-onset state i 1 onset)
             (set-duration state i 1 dur)
             (incf onset dur))
    (set-pitch state 1 1 (- start base))
    state))

(defun fifth-species-candidates (state note accumulated best crossing-policy)
  "C: Look + SaveIndx. Equal-cost candidates retain interval order."
  (let ((previous (pitch-at state (1- note) 1))
        (options nil))
    (loop for i from 1 to 16
          for candidate = (+ previous (aref *search-intervals* i))
          for cost = (check-fifth-species
                      state note candidate :limit (- best accumulated)
                      :crossing-policy crossing-policy)
          do (set-pitch state note 1 candidate) 
             (when (< cost (- best accumulated))
               (push (cons cost i) options)))
    (stable-sort (nreverse options) #'< :key #'car)))

(defun fifth-species-cadence-ficta (state)
  (third-species-cadence-ficta state))

(defun fifth-species-line-from-state (state)
  (third-species-line-from-state state))

(defun generate-fifth-species (cantus start
                               &key (mode :dorian) (stream nil)
                                 (seed 1) patterns
                                 (branch-limit 250)
                                 (integer-bounds nil)
                                 (terminal-bound :best-fit)
                                 (crossing-policy :candidate)
                                 (with-search-costs nil))
  "C: BestFitFirst for one generated voice in florid counterpoint.
Return ((penalty (pitches...)) ...). Rhythm is selectable by SEED or
PATTERNS, but fixed during a pitch search, as in C. The :REFERENCE
configuration corresponds to integer bounds, stored crossing and MaxPenalty."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal-bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-fifth-species-cantus cantus start mode
                                            :seed seed :patterns patterns))
         (last-note (total-notes state 1))
         (best-penalty 1000000)
         (max-penalty 400)
         (penalty-ratio 95/100)
         (branches 0)
         (results nil)
         (local-costs (when with-search-costs
                        (make-array 128 :initial-element 0))))
    (labels ((bound (number)
               (if integer-bounds (truncate number) number))
             (save-result (cost)
               
               (let ((raw (when with-search-costs
                            (fifth-species-line-from-state state)))
                     (costs (when with-search-costs
                              (loop for i from 1 to last-note
                                    collect (aref local-costs i)))))
                 (when with-search-costs
                   (unless (= cost (reduce #'+ costs))
                     (error "Search cost mismatch")))
                 (fifth-species-cadence-ficta state)
                 (setf best-penalty cost
                       max-penalty (bound (min (* cost penalty-ratio)
                                               max-penalty)))
                 (let ((line (fifth-species-line-from-state state)))
                   (push (if with-search-costs
                             (list cost line :search-local-costs costs
                                   :raw-pitches raw
                                   :ficta-changes
                                   (loop for before in raw for after in line
                                         for i from 1
                                         unless (= before after)
                                           collect (list i before after)))
                             (list cost line))
                         results)
                   (when stream
                     (format stream "~% [~D] ~{~D ~}~%" cost line)))))
             (visit-node (note accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* max-penalty penalty-ratio))
                         branches 0))
                 (let ((options (fifth-species-candidates
                                 state note accumulated best-penalty
                                 crossing-policy)))
                   (dolist (option options)
                     (let ((total-cost (+ accumulated (car option))))
                       (when (< total-cost
                                (if (and (= note last-note)
                                         (eq terminal-bound :best-fit))
                                    best-penalty
                                    max-penalty))
                         (set-pitch state note 1
                                    (+ (pitch-at state (1- note) 1)
                                       (aref *search-intervals* (cdr option))))
                         (when with-search-costs
                           (setf (aref local-costs note) (car option)))
                         (if (< note last-note)
                             (visit-node (1+ note) total-cost)
                             (save-result total-cost)))
                       (when (= note 2)
                         (setf max-penalty
                               (bound (* best-penalty penalty-ratio))))))))))
      (visit-node 2 0)
      (nreverse results))))


;;;; mixed-second-species: FUX implementation.

(in-package :fux)

(defun other-voice-check-mixed-second (state note candidate voice num-parts limit)
  "C: OtherVoiceCheck. Skip chord-tone checks for final-voice dissonances."
  (let* ((low (bass state note voice))
         (interval (mod (- candidate low) 12))
         (value 0))
    (when (<= candidate low)
      (incf value (penalty :cross-below-bass)))
    (when (and (= interval 4) (not (in-mode low (aref state 1))))
      (incf value (penalty :augmented-interval)))
    (if (or (>= value limit)
            (and (= voice num-parts) (aref *dissonances* interval)))
        value
        (other-voice-check-first-species
         state note candidate voice num-parts limit))))

(defun check-mixed-second-species (state note candidate &key (voice 2) (num-parts 2)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for second species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
For the final generated voice (VOICE=2), above a first-species voice."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (integerp num-parts) (<= 2 num-parts 5)
               (eql voice num-parts))
    (error "Second species must be the final voice among 2..5 generated voices"))
  (let* ((other0 (bass state note voice))
         (other1 (bass state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (bass state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (= num-parts 1)
                 (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         
         (unless (and (= note (- (total-notes state voice) 2))
                      (eq mode :aeolian)
                      (> candidate other0)
                      (= int-class 7))
           (unless (in-mode pitch mode) (add :out-of-mode))))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (when (= voice num-parts) (add :no-leading-tone))))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      (when (dissonance-p state int-class note candidate voice 2)
        (add :dissonance))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      
      (when (and penultimate-p (member pitch '(10 11)))
        (if (or (not (eq mode :phrygian)) (>= interval 0))
            (unless (= last-int-class 7) (add :bad-cadence))
            (unless (= last-int-class 8) (add :bad-cadence))))
      
      (incf value (other-voice-check-mixed-second
                   state note candidate voice num-parts limit))
      (when (= note 1) (return-from check-mixed-second-species value))
      (when (and (or (not last-p) (= num-parts 1))
                 (direct-motion-to-perfect-consonance-p
                  last-cp candidate other1 other0))
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      (when (and last-p (/= int-class 0)
                 (or (= num-parts 1) (minusp interval)
                     (not (member int-class '(4 7)))))
        (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (= num-parts 1) (tenth-p (- other1 last-cp))
                 (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (and (= num-parts 1) (= interval 0)) (add :unison))
      (when (cutoff-p) (return-from check-mixed-second-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (= num-parts 1)
                 (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (when (= num-parts 1)
        (let ((cross 0))
          (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
          (incf value (max 0 (* 3 (- cross 2))))))
      
      (when (downbeat-p state note voice)
        (when (and (zerop mel-int) (not last-p))
          (add :unison-downbeat))
        (when (and (aref *dissonances* last-int-class)
                   (or (not (step-p mel-int)) (not same-dir)))
          (add :dissonance-not-filling-third))
        (when (and (> note 2)
                   (direct-motion-to-perfect-consonance-p
                    last-cp2 candidate other2 other0)
                   (< (abs last-mel-int) 5))
          (add :direct-perfect-on-downbeat)))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; mixed-search: FUX implementation.

(in-package :fux)

(defun make-mixed-second-species-state (cantus starts mode)
  "Voice 1 uses whole notes; voice 2 uses half notes, then a whole note."
  (unless (and (<= 2 (length cantus) 32) (= (length starts) 2))
    (error "Need 2..32 cantus notes and two starting pitches"))
  (let* ((state (make-first-species-cantus cantus (first starts) mode))
         (count (1- (* 2 (length cantus)))))
    (set-total-notes state 2 count)
    (loop for note from 1 to count
          do (set-onset state note 2 (* 4 (1- note)))
             (set-duration state note 2 (if (= note count) 8 4)))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun next-mixed-onset (state time)
  "First future onset for either generated voice (C: VIndex/NextTime)."
  (let ((next nil))
    (loop for voice from 1 to 2
          do (loop for note from 2 to (total-notes state voice)
                   for onset = (onset-at state note voice)
                   when (> onset time)
                     do (setf next (if next (min next onset) onset))
                        (return)))
    next))

(defun mixed-notes-at (state time)
  "Return (VOICE . NOTE) for voices beginning new notes at TIME."
  (loop for voice from 1 to 2
        for note = (loop for i from 2 to (total-notes state voice)
                         when (= (onset-at state i voice) time)
                           return i)
        when note collect (cons voice note)))

(defun mixed-second-species-candidates (state notes accumulated best-penalty
                                      crossing-policy)
  "C Look/SaveIndx: at most 16 ranked combinations of moving voices.
Each result is (local-penalty (voice . interval-index)...)."
  (let ((limit (- best-penalty accumulated))
        (options nil))
    (labels ((visit (remaining local-cost indices)
               (if (endp remaining)
                   (push (list local-cost (reverse indices)) options)
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous (aref *search-intervals* index))
                           for local = (if (= voice 1)
                                           (check-poly-first-species
                                            state note pitch
                                            :voice 1 :num-parts 2
                                            :limit (- limit local-cost)
                                            :crossing-policy crossing-policy)
                                           (check-mixed-second-species
                                            state note pitch
                                            :voice 2 :num-parts 2
                                            :limit (- limit local-cost)
                                            :crossing-policy crossing-policy))
                           for cost = (+ local-cost local)
                           do (set-pitch state note voice pitch) 
                              (when (< cost limit)
                                (visit (cdr remaining) cost
                                       (cons (cons voice index) indices))))))))
      (visit notes 0 nil))
    
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun generate-mixed-second-species (cantus starts
                                      &key (mode :dorian) (stream nil)
                                        (integer-bounds nil)
                                        (terminal-bound :best-fit)
                                        (crossing-policy :candidate)
                                        (branch-limit 800))
  "Generate voice 1 in first species, voice 2 in second species.
Return improvements as (COST (VOICE-1...) (VOICE-2...)).
Use :INTEGER-BOUNDS T and :TERMINAL-BOUND :MAX-PENALTY to match
C's documented search-bound behavior."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-mixed-second-species-state cantus starts mode))
         (total-time (aref state 2))
         (best-penalty 1000000)
         (max-penalty 1000000)
         (ratio 24/25)  
         (branches 0)
         (results nil))
    (labels ((bound (value) (if integer-bounds (truncate value) value))
             (save-result (cost)
               (poly-first-species-cadence-ficta state)
               (setf best-penalty cost
                     max-penalty (bound (min max-penalty (* ratio cost))))
               (let ((record (cons cost (poly-lines-from-state state))))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] ~%" cost)
                   (dolist (line (cdr record))
                     (format stream "~{~D ~}~%" line)))))
             (visit-node (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time (next-mixed-onset state time))
                        (notes (and next-time (mixed-notes-at state next-time))))
                   (when notes
                     (dolist (option (mixed-second-species-candidates
                                      state notes accumulated best-penalty
                                      crossing-policy))
                       (let ((total-cost (+ accumulated (first option))))
                         (when (< total-cost
                                  (if (and (= next-time total-time)
                                           (eq terminal-bound :best-fit))
                                      best-penalty
                                      max-penalty))
                           (dolist (choice (second option))
                             (let* ((voice (car choice))
                                    (note (cdr (assoc voice notes)))
                                    (previous (pitch-at state (1- note) voice)))
                               (set-pitch state note voice
                                          (+ previous
                                             (aref *search-intervals* (cdr choice))))))
                           (if (< next-time total-time)
                               (visit-node next-time total-cost)
                               (save-result total-cost)))
                         (when (= time 0)
                           (setf max-penalty
                                 (bound (* ratio best-penalty)))))))))))
      (visit-node 0 0)
      (nreverse results))))


;;;; mixed-voice-rules: FUX implementation.

(in-package :fux)

(dolist (entry '((:unprepared-six-five . 100)
                 (:unresolved-six-five . 100)))
  (unless (assoc (car entry) *fux-penalties*)
    (push entry *fux-penalties*)))

(defun other-voice-check-mixed (state note candidate voice num-parts species limit)
  "C OtherVoiceCheck for an upper, final non-first-species voice."
  (let* ((low (bass state note voice))
         (interval (mod (- candidate low) 12))
         (last-cp (pitch-at state (1- note) voice))
         (actual-pitch (mod candidate 12))
         (counts (make-array 8 :initial-element 0))
         (value 0)
         (all-skip (skip-p (- candidate last-cp))))
    (labels ((add (name) (incf value (penalty name)))
             (count-interval (difference)
               (incf (aref counts (chord-interval-class difference)))))
      (when (<= candidate low) (add :cross-below-bass))
      (when (and (= interval 4) (not (in-mode low (aref state 1))))
        (add :augmented-interval))
      
      (when (or (>= value limit)
                (and (= voice num-parts) (aref *dissonances* interval)))
        (return-from other-voice-check-mixed value))
      (count-interval interval)
      (loop for k below voice
            for other0 = (other-voice state note voice k)
            for other1 = (other-voice state (1- note) voice k)
            for last-int = (mod (abs (- other1 last-cp)) 12)
            for this-int = (mod (abs (- other0 candidate)) 12)
            do (unless (skip-p (- other0 other1)) (setf all-skip nil))
               (count-interval (- other0 low))
               (when (and (not (last-note-p state note voice))
                          (= other0 candidate))
                 (add :unison))
               (when (and (/= other0 low) (>= (abs (- candidate other0)) 19))
                 (add :upper-voices-too-far-apart))
               (when (= last-int this-int)
                 (cond ((zerop this-int) (add :parallel-unison))
                       ((= this-int 7) (add :parallel-fifth))))
               (when (and (> note 2) (zerop this-int)
                          (zerop (mod (abs (- (pitch-at state (- note 2) voice)
                                               (other-voice state (- note 2) voice k))) 12)))
                 (add :parallel-unison))
               (when (>= value limit) (return-from other-voice-check-mixed value))
               (when (= this-int 6) (add :vertical-tritone))
               (when (= species 5)
                 
                 (when (and (aref *dissonances* last-int)
                            (/= last-int 5))
                   (let ((our-last-int
                           (mod (- last-cp (bass state (1- note) voice)) 12)))
                     (unless (zerop our-last-int)
                       (if (= our-last-int 7)
                           (when (or (skip-p (- candidate last-cp))
                                     (>= candidate last-cp))
                             (add :unresolved-six-five))
                           (when (or (skip-p (- other0 other1))
                                     (>= other0 other1))
                             (add :unresolved-six-five))))))
                 (when (and (aref *dissonances* this-int)
                            (/= this-int 5)
                            (not (zerop interval)))
                   (when (if (= interval 7)
                             (/= (- candidate last-cp) 0)
                             (/= (- other0 other1) 0))
                     (add :unprepared-six-five))))
               (when (and (not (last-note-p state note voice))
                          (direct-motion-to-perfect-consonance-p
                           last-cp candidate other1 other0))
                 (add :inner-voices-direct-to-perfect))
               (when (and (= actual-pitch 10) (= (mod other0 12) 11))
                 (add :doubled-leading-tone))
               (when (and (eq (motion-type last-cp candidate other1 other0) :direct)
                          (= this-int 6))
                 (add :inner-voices-direct-to-tritone))
               (when (and (= actual-pitch 3) (= (mod other0 12) 11))
                 (add :augmented-interval))
               (unless (eq (motion-type last-cp candidate other1 other0) :contrary)
                 (add :not-contrary-to-others)))
      (when (> (aref counts 3) 1) (add :third-doubled))
      (when (and (zerop (aref counts 3)) (> (aref counts 6) 1))
        (add :sixth-doubled))
      (when (> (aref counts 0) 2) (add :tripled-bass))
      (when (> (aref counts 5) 1) (add :fifth-doubled))
      (when (and (= voice num-parts)
                 (not (last-note-p state note voice))
                 (zerop (aref counts 3)) (zerop (aref counts 6)))
        (add :not-triad))
      (when (and (= voice num-parts) all-skip) (add :all-voices-skip))
      (when (and (plusp (aref counts 5)) (plusp (aref counts 6))
                 (/= species 5))
        (add :six-five-chord))
      value)))


;;;; mixed-third-species: FUX implementation.

(in-package :fux)

(defun check-mixed-third-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for third species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
For final VOICE=2 and NUM-PARTS=2."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (integerp num-parts) (<= 2 num-parts 5)
               (eql voice num-parts))
    (error "Third species must be the final voice among 2..5 generated voices"))
  (let* ((other0 (bass state note voice))
         (other1 (bass state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (bass state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (= num-parts 1) (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      (when (dissonance-p state int-class note candidate voice 3)
        (add :dissonance))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      
      
      (when (and (= (beat8 (onset-at state note voice)) 6)
                 (= candidate last-cp))
        (add :unison-on-beat4))
      (when (downbeat-p state note voice)
        (when (skip-p mel-int) (add :skip-to-downbeat))
        (when (and (> note 2) (member int-class '(0 7))
                   (= (mod (abs (- last-cp4
                                    (bass state (- note 4) voice))) 12)
                      int-class))
          (add :downbeat-unison)))
      
      (when (and (= (beat8 (onset-at state note voice)) 6)
                 (member (abs last-mel-int) '(3 4))
                 (aref *dissonances*
                       (mod (abs (- last-cp2 other2)) 12))
                 (or (minusp mel-int)
                     (not (member (abs mel-int) '(1 2)))))
        (add :nota-cambiata))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      
      (when (and (> note 1) (aref *dissonances* last-int-class))
        (case (beat8 (onset-at state note voice))
          ((0 6)
           (when (or (not (step-p mel-int))
                     (not (step-p last-mel-int))
                     (minusp (* mel-int last-mel-int)))
             (add :dissonance)))
          (2 (add :dissonance))
          (4
           (if (or (not (step-p last-mel-int))
                   (> (abs mel-int) 4) (zerop mel-int)
                   (minusp (* last-mel-int mel-int)))
               (add :dissonance)
               (unless (step-p mel-int)
                 (if (>= interval 0)
                     (unless (member last-int-class '(10 11))
                       (add :dissonance))
                     (unless (= last-int-class 5)
                       (add :dissonance))))))))
      
      
      (incf value (other-voice-check-mixed state note candidate
                                          voice num-parts 3
                                          limit))
      (when (= note 1) (return-from check-mixed-third-species value))
      (when (and (or (not last-p) (= num-parts 1))
                 (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0))
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      (when (and last-p (/= int-class 0)
                 (or (= num-parts 1) (minusp interval)
                     (not (member int-class '(4 7))))) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (= num-parts 1) (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (and (= num-parts 1) (= interval 0)) (add :unison))
      (when (cutoff-p) (return-from check-mixed-third-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (= num-parts 1) (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (when (= num-parts 1)
        (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2))))))
      
      (when (downbeat-p state note voice)
        (when (and (zerop mel-int) (not last-p))
          (add :unison-downbeat))
        (when (and (aref *dissonances* last-int-class)
                   (or (not (step-p mel-int)) (not same-dir)))
          (add :dissonance-not-filling-third))
        (when (and (> note 2)
                   (direct-motion-to-perfect-consonance-p
                    last-cp2 candidate other2 other0)
                   (< (abs last-mel-int) 5))
          (add :direct-perfect-on-downbeat)))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; mixed-fourth-species: FUX implementation.

(in-package :fux)

(defun check-mixed-fourth-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for fourth species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
For final VOICE=2 and NUM-PARTS=2."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (integerp num-parts) (<= 2 num-parts 5)
               (eql voice num-parts))
    (error "Fourth species must be the final voice among 2..5 generated voices"))
  (let* ((other0 (bass state note voice))
         (other1 (bass state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (bass state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (= num-parts 1) (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      (when (dissonance-p state int-class note candidate voice 4)
        (add :dissonance))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      
      
      (when (and (downbeat-p state note voice) (not (zerop mel-int)))
        (add :nota-ligature))
      (when (and (upbeat-p state note voice)
                 (aref *dissonances* last-int-class))
        (unless (member mel-int '(-1 -2))
          (add :unresolved-ligature))
        
        
        (when (and (zerop int-class)
                   (or (minusp interval)
                       (zerop (mod (abs (- (pitch-at state (- note 2) voice)
                                                other2)) 12))))
          (add :no-time-for-a-ligature))
        (when (member int-class '(7 6))
          (add :no-time-for-a-ligature)))
      
      (incf value (other-voice-check-mixed state note candidate
                                          voice num-parts 4
                                          limit))
      (when (= note 1) (return-from check-mixed-fourth-species value))
      (when (and (or (not last-p) (= num-parts 1))
                 (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0))
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      (when (and last-p (/= int-class 0)
                 (or (= num-parts 1) (minusp interval)
                     (not (member int-class '(4 7))))) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      (when (and (= num-parts 1) (tenth-p (- other1 last-cp)) (octave-p interval))
        (add :tenth-to-octave))
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (> (total-range state note candidate voice) 19)
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (and (= num-parts 1) (= interval 0)) (add :unison))
      (when (cutoff-p) (return-from check-mixed-fourth-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (= num-parts 1) (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (when (= num-parts 1)
        (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2))))))
      
      
      (when (and (downbeat-p state note voice)
                 (> note 2)
                 (direct-motion-to-perfect-consonance-p
                  last-cp2 candidate other2 other0)
                 (< (abs last-mel-int) 5))
        (add :direct-perfect-on-downbeat))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; mixed-fifth-species: FUX implementation.

(in-package :fux)

(defun check-mixed-fifth-species (state note candidate &key (voice 1) (num-parts 1)
                                                   (limit 1000000)
                                                   (crossing-policy :candidate))
  "C: Check for fifth species, two voices, in original rule order.
STATE must have all cantus pitches, onset/duration and total-notes set.
The result is the candidate's LOCAL penalty, not the cumulative score.
For final VOICE=2 and NUM-PARTS=2."
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (unless (and (integerp num-parts) (<= 2 num-parts 5)
               (eql voice num-parts))
    (error "Fifth species must be the final voice among 2..5 generated voices"))
  (let* ((other0 (bass state note voice))
         (other1 (bass state (1- note) voice))
         (last-cp (pitch-at state (1- note) voice))
         (interval (- candidate other0))
         (int-class (mod (abs interval) 12))
         (mel-int (- candidate last-cp))
         (pitch (mod candidate 12))
         (mode (aref state 1))
         (base-pitch (aref state 0))
         (last-p (last-note-p state note voice))
         (penultimate-p (next-to-last-note-p state note voice))
         (last-int-class (mod (abs (- last-cp other1)) 12))
         (other2 (if (> note 2) (bass state (- note 2) voice) 0))
         (last-cp2 (if (> note 2) (pitch-at state (- note 2) voice) 0))
         (last-cp3 (if (> note 3) (pitch-at state (- note 3) voice) 0))
         (last-cp4 (if (> note 4) (pitch-at state (- note 4) voice) 0))
         (last-mel-int (- last-cp last-cp2))
         (same-dir (>= (* mel-int last-mel-int) 0))
         (value 0))
    (labels ((add (name) (incf value (penalty name)))
             (cutoff-p () (>= value limit)))
      
      (when (out-of-range-p (+ candidate base-pitch)) (add :out-of-range))
      (when (extreme-range-p (+ candidate base-pitch)) (add :extreme-range))
      (when (and (= num-parts 1) (< (pitch-at state 1 voice) (cantus state 1 voice))
                 (> interval 0))
        (add :cross-above-cantus))
      (cond
        ((not penultimate-p)
         (unless (in-mode pitch mode) (add :out-of-mode)))
        ((or (= pitch 11) (and (= pitch 10) (eq mode :phrygian)))
         (when (doubled-p state pitch note voice) (add :doubled-leading-tone)))
        ((= pitch 10) (add :bad-cadence))
        ((not (in-mode pitch mode)) (add :out-of-mode))
        ((not (or (doubled-p state 11 note voice)
                  (doubled-p state 10 note voice)))
         (add :no-leading-tone)))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      (when (dissonance-p state int-class note candidate voice 5)
        (add :dissonance))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      
      (let ((beat (beat8 (onset-at state note voice))))
        (when (and (member beat '(6 7)) (= candidate last-cp))
          (add :unison-on-beat4))
        (when (zerop beat)
          (when (skip-p mel-int) (add :skip-to-downbeat))
          (when (and (> note 2) (member int-class '(0 7)))
            
            
            (let ((previous-downbeat
                    (loop for i downfrom (1- note) to 1
                          when (zerop (beat8 (onset-at state i voice)))
                            return i)))
              (when (and previous-downbeat
                         (= (mod (abs (- (pitch-at state previous-downbeat voice)
                                         (bass state previous-downbeat voice)))
                                 12)
                            int-class))
                (add :downbeat-unison)))))
        
        (when (and (= beat 6) (member (abs last-mel-int) '(3 4))
                   (aref *dissonances*
                         (mod (abs (- last-cp2 other2)) 12))
                   (or (minusp mel-int)
                       (not (step-p mel-int))))
          (add :nota-cambiata))
        (when (cutoff-p) (return-from check-mixed-fifth-species value))

        
        (when (and (> note 1) (zerop beat)
                   (/= candidate last-cp)
                   (<= (duration-at state note voice)
                       (duration-at state (1- note) voice)))
          (add :lesser-ligature))
        (when (and (> note 3)
                   (= (duration-at state note voice) 4)
                   (= beat 4)
                   (= (duration-at state (1- note) voice) 2)
                   (= (duration-at state (- note 2) voice) 2))
          (add :half-untied))
        (when (and (= (duration-at state note voice) 1)
                   (zerop beat)
                   (aref *dissonances* int-class))
          (add :dissonance))
        (when (cutoff-p) (return-from check-mixed-fifth-species value))

        
        (when (and (> note 1)
                   (aref *dissonances*
                         (mod (abs (- last-cp other1)) 12)))
          (let ((last-dis-int (mod (abs (- last-cp other1)) 12))
                (previous-beat (beat8 (onset-at state (1- note) voice)))
                (previous-duration (duration-at state (1- note) voice))
                (before-previous-duration
                  (duration-at state (- note 2) voice)))
            (case previous-beat
              ((4 6)
               (unless (and (= last-dis-int 5) (zerop mel-int)
                            (= other0 other1) (zerop beat))
                 (when (or (not (step-p mel-int))
                           (not (step-p last-mel-int))
                           (minusp (* mel-int last-mel-int))
                           (= previous-duration 1)
                           (and (= previous-duration 2)
                                (= before-previous-duration 4)))
                   (add :dissonance))))
              ((1 3 5 7)
               (when (or (not (step-p mel-int))
                         (not (step-p last-mel-int))
                         (minusp (* mel-int last-mel-int)))
                 (add :dissonance)))
              (0
               (when (or (= before-previous-duration 1)
                         (< before-previous-duration previous-duration))
                 (add :no-time-for-a-ligature))
               (unless (member mel-int '(-1 -2))
                 (add :unresolved-ligature))
               (when (member int-class '(5 6))
                 (add :no-time-for-a-ligature))
               (when (and (= int-class 7) (minusp interval))
                 (add :no-time-for-a-ligature))
               (when (and (zerop int-class)
                          (zerop (mod (abs (- last-cp2 other2)) 12)))
                 (add :no-time-for-a-ligature))
               (unless (zerop last-mel-int)
                 (add :dissonance)))
              (2
               (if (or (not (step-p last-mel-int))
                       (> (abs mel-int) 4)
                       (zerop mel-int)
                       (= previous-duration 1)
                       (minusp (* last-mel-int mel-int)))
                   (add :dissonance)
                   (unless (step-p mel-int)
                     (if (>= interval 0)
                         (unless (member last-int-class '(10 11))
                           (add :dissonance))
                         (unless (= last-int-class 5)
                           (add :dissonance)))))))))
        (when (and (> note 1)
                   (= (duration-at state (1- note) voice) 1)
                   (not (step-p mel-int)))
          (add :eighth-jump))
        (when (and (> note 1)
                   (= (duration-at state (1- note) voice) 4)
                   (= beat 4)
                   (zerop mel-int))
          (add :unison-upbeat)))

      
      (incf value (other-voice-check-mixed state note candidate
                                          voice num-parts 5
                                          limit))
      (when (= note 1) (return-from check-mixed-fifth-species value))
      (when (and (or (not last-p) (= num-parts 1))
                 (direct-motion-to-perfect-consonance-p
             last-cp candidate other1 other0))
        (add (if (= int-class 0) :direct-to-octave :direct-to-fifth)))
      (when (and (= int-class 7) (= last-int-class 7)) (add :parallel-fifth))
      (when (and (= int-class 0) (= last-int-class 0)) (add :parallel-unison))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      
      (when (bad-melody-p mel-int) (add :bad-melody))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      (when (and last-p (/= int-class 0)
                 (or (= num-parts 1) (minusp interval)
                     (not (member int-class '(4 7))))) (add :end-on-perfect))
      (when (eq (motion-type last-cp candidate other1 other0) :direct)
        (add :direct-motion)
        (when (= int-class 6) (add :direct-to-fifth)))
      (when (> (abs interval) 12) (add :compound))
      (when (and (> note 2)
                 (consecutive-skips-in-same-direction-p last-cp2 last-cp candidate))
        (add :two-skips)
        (let ((jump (abs (- candidate last-cp2))))
          (when (< 9 jump 12) (add :two-skips-not-in-triad))))
      (when (and (= int-class 0)
                 (or (skip-p mel-int) (skip-p (- other0 other1))))
        (add :skip-to-octave))
      (when (and (= other1 last-cp) (skip-p mel-int))
        (add :skip-from-unison))
      (when (and (> note 2) (skip-p mel-int) same-dir)
        (add (cond ((< (abs mel-int) 7) :skip-preceded-same-dir)
                   ((member (abs mel-int) '(7 12)) :fifth-preceded-same-dir)
                   (t :sixth-preceded-same-dir))))
      (when (and (> note 2) (skip-p last-mel-int) same-dir)
        (add (cond ((< (abs last-mel-int) 7) :skip-followed-same-dir)
                   ((member (abs last-mel-int) '(7 12)) :fifth-followed-same-dir)
                   (t :sixth-followed-same-dir))))
      (when (and (> note 4) (skip-p mel-int) (skip-p last-mel-int)
                 (skip-p (- last-cp2 last-cp3)))
        (add :melodic-boredom))
      (when (and (> note 4)
                 (member 6 (list (abs (- candidate last-cp2))
                                 (abs (- candidate last-cp3))
                                 (abs (- candidate last-cp4)))))
        (add :melodic-tritone))
      
      (when (and (> note 2) (> (abs (- candidate last-cp2)) 12))
        (add :over-octave))
      (when (and (> note 30)
                 (> (total-range state note candidate voice) 19))
        (add :over-twelfth))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      
      (when (and (> note 3) (= candidate last-cp2) (= last-cp last-cp3))
        (add :two-repeated-notes))
      (when (and (> note 5) (= candidate last-cp3)
                 (= last-cp last-cp4)
                 (= last-cp2 (pitch-at state (- note 5) voice)))
        (add :three-repeated-notes))
      (when (and (> note 6) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice)))
        (incf value (1- (penalty :three-repeated-notes))))
      (when (and (> note 7) (= candidate last-cp4)
                 (= last-cp (pitch-at state (- note 5) voice))
                 (= last-cp2 (pitch-at state (- note 6) voice))
                 (= last-cp3 (pitch-at state (- note 7) voice)))
        (add :four-repeated-notes))
      (when (and (> note 8) (= candidate (pitch-at state (- note 5) voice))
                 (= last-cp (pitch-at state (- note 6) voice))
                 (= last-cp2 (pitch-at state (- note 7) voice))
                 (= last-cp3 (pitch-at state (- note 8) voice)))
        (add :four-repeated-notes))
      (when (and last-p
                 (or (= (mod last-cp 12) 11)
                     (and (= (mod last-cp 12) 10) (eq mode :phrygian)))
                 (/= pitch 0))
        (add :unresolved-leading-tone))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      (when (aref *perfect-consonances* int-class)
        (add :perfect-consonance))
      (when (and (= num-parts 1) (= interval 0)) (add :unison))
      (when (cutoff-p) (return-from check-mixed-fifth-species value))
      (incf value (floor (pitch-repeats state note candidate voice) 2))
      (when (octave-p mel-int) (add :octave-leap))
      (when (= mel-int 8) (add :sixth-leap))
      (when (and (> note 2) (minusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :upper-neighbor))
      (when (and (> note 2) (plusp mel-int) (step-p mel-int)
                 (= candidate last-cp2))
        (add :lower-neighbor))
      (when (and (not (in-mode pitch mode))
                 (member mel-int '(1 8 -4)))
        (add :out-of-mode))
      (when (and (> note 2) (skip-p mel-int) (skip-p last-mel-int)
                 (not same-dir))
        (incf value (max 0 (- (+ (abs mel-int) (abs last-mel-int)) 8)))
        (when (and (> note 3) (skip-p (- last-cp2 last-cp3)))
          (add :three-skips)))
      (when (and (= num-parts 1) (>= note (- (total-notes state voice) 4))
                 (> (abs mel-int) 4))
        (add :leap-at-cadence))
      
      
      
      (when (= num-parts 1)
        (let ((cross 0))
        (loop for k from 4 to note
              for current = (if (and (= k note)
                                     (eq crossing-policy :candidate))
                                candidate
                                (pitch-at state k voice))
              when (minusp (* (- current (cantus state k voice))
                              (- (pitch-at state (1- k) voice)
                                 (cantus state (1- k) voice))))
                do (incf cross))
        (incf value (max 0 (* 3 (- cross 2))))))
      
      
      (when (and (downbeat-p state note voice) (> note 2)
                 (direct-motion-to-perfect-consonance-p
                  last-cp2 candidate other2 other0)
                 (< (abs last-mel-int) 5))
        (add :direct-perfect-on-downbeat))
      (when (and (upbeat-p state note voice) (= mel-int 0))
        (add :repetition-upbeat))
      (when (and (eq mode :lydian)
                 (> note (- (total-notes state voice) 4))
                 (= pitch 6))
        (add :lydian-cadential-tritone))
      (when (= int-class 6) (add :vertical-tritone))
      (when (and (> note 10) (too-much-of-interval-p state note candidate voice))
        (add :melodic-boredom))
      value)))


;;;; mixed-advanced-search: FUX implementation.

(in-package :fux)

(defun mixed-advanced-durations (cantus-length species &key (seed 1) patterns)
  "Onset durations for the final voice. Whole=8; a final whole is appended."
  (case species
    (3 (append (make-list (* 4 (1- cantus-length)) :initial-element 2) '(8)))
    (4 (append (make-list (* 2 (1- cantus-length)) :initial-element 4) '(8)))
    (5 (fifth-species-durations cantus-length :seed seed :patterns patterns))
    (otherwise (error "Expected species 3, 4 or 5; received ~S" species))))

(defun make-mixed-advanced-state (cantus starts mode species
                                  &key (seed 1) patterns)
  "Two generated voices: voice 1 whole notes, voice 2 selected species."
  (unless (and (<= 2 (length cantus) 32) (= (length starts) 2))
    (error "Need 2..32 cantus pitches and two starts"))
  (let* ((state (make-first-species-cantus cantus (first starts) mode))
         (durations (mixed-advanced-durations (length cantus) species
                                               :seed seed :patterns patterns)))
    (unless (< (length durations) 128)
      (error "Too many events for the original 128-note state"))
    (set-total-notes state 2 (length durations))
    (loop with onset = 0
          for dur in durations
          for i from 1
          do (set-onset state i 2 onset)
             (set-duration state i 2 dur)
             (incf onset dur))
    (set-pitch state 1 2 (- (second starts) (aref state 0)))
    state))

(defun check-mixed-advanced (state note candidate species &key
                              (limit 1000000) (crossing-policy :candidate))
  "C Check for the FINAL generated voice, with first-species accompaniment."
  (case species
    (3 (check-mixed-third-species state note candidate
                                  :voice 2 :num-parts 2 :limit limit
                                  :crossing-policy crossing-policy))
    (4 (check-mixed-fourth-species state note candidate
                                   :voice 2 :num-parts 2 :limit limit
                                   :crossing-policy crossing-policy))
    (5 (check-mixed-fifth-species state note candidate
                                  :voice 2 :num-parts 2 :limit limit
                                  :crossing-policy crossing-policy))
    (otherwise (error "Invalid mixed species ~S" species))))

(defun mixed-advanced-candidates (state notes accumulated best species
                                   crossing-policy)
  "C Look/SaveIndx: 16 best simultaneous candidate combinations."
  (let ((limit (- best accumulated))
        (options nil))
    (labels ((visit (remaining cost indices)
               (if (endp remaining)
                   (push (list cost (reverse indices)) options)
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous (aref *search-intervals* index))
                           for local = (if (= voice 1)
                                           (check-poly-first-species
                                            state note pitch :voice 1 :num-parts 2
                                            :limit (- limit cost)
                                            :crossing-policy crossing-policy)
                                           (check-mixed-advanced
                                            state note pitch species
                                            :limit (- limit cost)
                                            :crossing-policy crossing-policy))
                           for next-cost = (+ cost local)
                           do (set-pitch state note voice pitch) 
                              (when (< next-cost limit)
                                (visit (cdr remaining) next-cost
                                       (cons (cons voice index) indices))))))))
      (visit notes 0 nil))
    (let ((ranked (stable-sort (nreverse options) #'< :key #'first)))
      (subseq ranked 0 (min 16 (length ranked))))))

(defun mixed-advanced-cadence-ficta (state)
  "C SaveResults: treat each voice's own penultimate note as its cadence."
  (loop with mode = (aref state 1)
        for voice from 1 to 2
        for n = (total-notes state voice)
        do (unless (in-mode (mod (pitch-at state (1- n) voice) 12) mode)
             (loop for k from 2 below (1- n)
                   for i = (- n k)
                   for pitch = (mod (pitch-at state i voice) 12)
                   for next-diff = (- (pitch-at state (1+ i) voice)
                                      (pitch-at state i voice))
                   for previous-diff = (abs (- (pitch-at state i voice)
                                               (pitch-at state (1- i) voice)))
                   do (when (or (and (< pitch 8) (/= pitch 0))
                                (skip-p next-diff)
                                (member previous-diff '(0 5 7 12)))
                        (return))
                      (when (loop for other from 0 to 2
                                  thereis (and (/= other voice)
                                               (= (mod (other-voice state i voice other)
                                                       12)
                                                  11)))
                        (return))
                      (when (member (- (pitch-at state (1- n) voice)
                                       (pitch-at state i voice)) '(1 3))
                        (incf (aref (aref state 3) i voice))))))
  state)

(defun generate-mixed-species (cantus starts species
                               &key (mode :dorian) (stream nil)
                                 (seed 1) patterns
                                 (integer-bounds nil)
                                 (terminal-bound :best-fit)
                                 (crossing-policy :candidate)
                                 (branch-limit (* 200 (- 6 species))))
  "Generate two voices, first in species 1 and second in SPECIES 3/4/5.
Returns ((penalty (voice-1...) (voice-2...)) ...).
C reference policy: :INTEGER-BOUNDS T :TERMINAL-BOUND :MAX-PENALTY
:CROSSING-POLICY :STORED. Rhy patterns for species 5 may be explicit."
  (unless (member species '(3 4 5))
    (error "SPECIES must be 3, 4, or 5"))
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Unknown terminal bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Unknown crossing policy: ~S" crossing-policy))
  (let* ((state (make-mixed-advanced-state cantus starts mode species
                                           :seed seed :patterns patterns))
         (total-time (aref state 2))
         (best 1000000)
         (max-penalty 1000000)
         (ratio (- 1 (/ species 50))) 
         (branches 0)
         (results nil))
    (labels ((bound (x) (if integer-bounds (truncate x) x))
             (save-result (cost)
               (mixed-advanced-cadence-ficta state)
               (setf best cost
                     max-penalty (bound (min max-penalty (* ratio cost))))
               (let ((record (cons cost (poly-lines-from-state state))))
                 (push record results)
                 (when stream
                   (format stream "~% [~D] ~%" cost)
                   (dolist (line (cdr record))
                     (format stream "~{~D ~}~%" line)))))
             (visit (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time (next-mixed-onset state time))
                        (notes (and next-time (mixed-notes-at state next-time))))
                   (when notes
                     (dolist (option (mixed-advanced-candidates
                                      state notes accumulated best species
                                      crossing-policy))
                       (let ((cost (+ accumulated (first option))))
                         (when (< cost
                                  (if (and (= next-time total-time)
                                           (eq terminal-bound :best-fit))
                                      best max-penalty))
                           (dolist (choice (second option))
                             (let* ((voice (car choice))
                                    (note (cdr (assoc voice notes)))
                                    (previous (pitch-at state (1- note) voice)))
                               (set-pitch state note voice
                                          (+ previous (aref *search-intervals*
                                                            (cdr choice))))))
                           (if (< next-time total-time)
                               (visit next-time cost)
                               (save-result cost)))
                         (when (zerop time)
                           (setf max-penalty (bound (* ratio best)))))))))))
      (visit 0 0)
      (nreverse results))))


;;;; many-voice-search: FUX implementation.

(in-package :fux)

(defun many-voice-durations (bars species seed patterns)
  (case species
    (1 (make-list bars :initial-element 8))
    ((2 4) (append (make-list (* 2 (1- bars)) :initial-element 4)
                   '(8)))
    (3 (append (make-list (* 4 (1- bars)) :initial-element 2)
               '(8)))
    (5 (fifth-species-durations bars :seed seed :patterns patterns))
    (otherwise (error "Species must be 1..5: ~S" species))))

(defun make-many-voice-state (cantus starts mode species
                              &key (seed 1) patterns inner-durations
                                   inner-half-notes)
  "Build C AnySpecies state with 3..5 moving voices.
INNER-DURATIONS is a list of duration lists, one per earlier voice. These
voices still use first-species Check rules, regardless of their rhythm.
INNER-HALF-NOTES uses half notes for earlier voices 2..N-1, as in the
documented five-staff C experiment; voice 1 remains in whole notes."
  (let* ((n (length cantus))
         (num-parts (length starts)))
    (unless (and (listp cantus) (<= 2 n 127)
                 (every #'integerp cantus)
                 (listp starts) (<= 3 num-parts 5)
                 (every #'integerp starts)
                 (assoc mode *modes*)
                 (member species '(1 2 3 4 5)))
      (error "Expected cantus 2..127 notes, 3..5 starts, mode, species 1..5"))
    (when (and inner-durations inner-half-notes)
      (error "Specify either INNER-DURATIONS or INNER-HALF-NOTES"))
    (when (and patterns (/= species 5))
      (error "PATTERNS applies to fifth species only"))
    (when (and inner-durations
               (or (not (listp inner-durations))
                   (/= (length inner-durations) (1- num-parts))))
      (error "INNER-DURATIONS must contain one list for each earlier voice"))
    (let* ((state (make-first-species-cantus cantus (first starts) mode))
           (half (append (make-list (* 2 (1- n)) :initial-element 4) '(8)))
           (whole (make-list n :initial-element 8))
           (earlier (or inner-durations
                        (loop for voice from 1 below num-parts
                              collect (if (and inner-half-notes (> voice 1))
                                          half whole))))
           (durations (append earlier
                              (list (many-voice-durations n species
                                                          seed patterns))))
           (total (* 8 n)))
      (loop for ds in durations for voice from 1
            for start in starts
            do (unless (and (listp ds) (<= 2 (length ds) 127)
                            (every (lambda (x) (and (integerp x) (> x 0))) ds)
                            (= (reduce #'+ ds) total)
                            (= (car (last ds)) 8))
                 (error "Voice ~D needs 2..127 positive durations spanning ~D, last 8"
                        voice total))
               (set-total-notes state voice (length ds))
               (loop with onset = 0
                     for d in ds for i from 1
                     do (set-onset state i voice onset)
                        (set-duration state i voice d)
                        (incf onset d))
               (set-pitch state 1 voice (- start (aref state 0))))
      
      
      (let ((final-onsets (loop for i from 2 to (total-notes state num-parts)
                                collect (onset-at state i num-parts))))
        (loop for voice from 1 below num-parts
              do (loop for i from 2 to (total-notes state voice)
                       for time = (onset-at state i voice)
                       unless (member time final-onsets :test #'=)
                         do (error "Voice ~D has onset ~D without final-voice onset"
                                   voice time))))
      state)))

(defun many-voice-next-onset (state time num-parts)
  (let ((next nil))
    (loop for voice from 1 to num-parts
          do (loop for i from 2 to (total-notes state voice)
                   for onset = (onset-at state i voice)
                   when (> onset time)
                     do (setf next (if next (min next onset) onset))
                        (return)))
    next))

(defun many-voice-notes-at (state time num-parts)
  (loop for voice from 1 to num-parts
        for note = (loop for i from 2 to (total-notes state voice)
                         when (= (onset-at state i voice) time) return i)
        when note collect (cons voice note)))

(defun many-voice-local-cost (state note pitch voice num-parts species
                               limit crossing-policy)
  (if (< voice num-parts)
      (check-poly-first-species state note pitch
                                :voice voice :num-parts num-parts
                                :limit limit :crossing-policy crossing-policy)
      (case species
        (1 (check-poly-first-species state note pitch
                                     :voice voice :num-parts num-parts
                                     :limit limit :crossing-policy crossing-policy))
        (2 (check-mixed-second-species state note pitch
                                       :voice voice :num-parts num-parts
                                       :limit limit :crossing-policy crossing-policy))
        (3 (check-mixed-third-species state note pitch
                                      :voice voice :num-parts num-parts
                                      :limit limit :crossing-policy crossing-policy))
        (4 (check-mixed-fourth-species state note pitch
                                       :voice voice :num-parts num-parts
                                       :limit limit :crossing-policy crossing-policy))
        (5 (check-mixed-fifth-species state note pitch
                                      :voice voice :num-parts num-parts
                                      :limit limit :crossing-policy crossing-policy)))))

(defun many-voice-candidates (state notes accumulated best num-parts species
                                crossing-policy &optional with-search-costs)
  "Keep the lowest 16 combinations in discovery order for equal costs.
The active cutoff removes only combinations that cannot enter this set."
  (let ((ranked nil)
        (limit (- best accumulated)))
    (labels ((insert (option items)
               (cond ((endp items) (list option))
                     ((< (first option) (first (car items)))
                      (cons option items))
                     (t (cons (car items) (insert option (cdr items))))))
             (cutoff ()
               (if (= (length ranked) 16)
                   (min limit (first (car (last ranked))))
                   limit))
             (visit (remaining cost indices local-costs)
               (if (endp remaining)
                   (when (or (< (length ranked) 16)
                             (< cost (first (car (last ranked)))))
                     (setf ranked (insert (if with-search-costs
                                              (list cost (reverse indices)
                                                    (reverse local-costs))
                                              (list cost (reverse indices)))
                                          ranked))
                     (when (> (length ranked) 16)
                       (setf ranked (butlast ranked))))
                   (let* ((voice (caar remaining))
                          (note (cdar remaining))
                          (previous (pitch-at state (1- note) voice)))
                     (loop for index from 1 to 16
                           for pitch = (+ previous
                                          (aref *search-intervals* index))
                           for local = (many-voice-local-cost
                                        state note pitch voice num-parts species
                                        (- (cutoff) cost) crossing-policy)
                           for new-cost = (+ cost local)
                           do (set-pitch state note voice pitch)
                              (when (< new-cost (cutoff))
                                (visit (cdr remaining) new-cost
                                       (cons (cons voice index) indices)
                                       (if with-search-costs
                                           (cons local local-costs)
                                           nil))))))))
      (visit notes 0 nil nil))
    ranked))

(defun many-voice-cadence-ficta (state num-parts)
  "C SaveResults, treating each voice's own penultimate note as cadence."
  (loop with mode = (aref state 1)
        for voice from 1 to num-parts
        for n = (total-notes state voice)
        do (unless (in-mode (mod (pitch-at state (1- n) voice) 12) mode)
             (loop for k from 2 below (1- n)
                   for i = (- n k)
                   for pitch = (mod (pitch-at state i voice) 12)
                   for next-diff = (- (pitch-at state (1+ i) voice)
                                      (pitch-at state i voice))
                   for previous-diff = (abs (- (pitch-at state i voice)
                                               (pitch-at state (1- i) voice)))
                   do (when (or (and (< pitch 8) (/= pitch 0))
                                (skip-p next-diff)
                                (member previous-diff '(0 5 7 12)))
                        (return))
                      (when (loop for other from 0 to num-parts
                                  thereis (and (/= other voice)
                                               (= (mod (other-voice state i voice other)
                                                       12) 11)))
                        (return))
                      (when (member (- (pitch-at state (1- n) voice)
                                       (pitch-at state i voice)) '(1 3))
                        (incf (aref (aref state 3) i voice))))))
  state)

(defun many-voice-lines (state num-parts)
  (loop with base = (aref state 0)
        for voice from 1 to num-parts
        collect (loop for i from 1 to (total-notes state voice)
                      collect (+ base (pitch-at state i voice)))))

(defun generate-many-voice-counterpoint
    (cantus starts species &key (mode :dorian) (stream nil)
                            (seed 1) patterns inner-durations inner-half-notes
                            (integer-bounds nil)
                            (terminal-bound :best-fit)
                            (crossing-policy :candidate)
                            branch-limit with-search-costs)
  "C AnySpecies for 3..5 generated voices.
Earlier voices get first-species RULES; their rhythms may be supplied
separately from those rules. Return improving (COST VOICE1 ... VOICEN)
records. The C search configuration uses INTEGER-BOUNDS T,
TERMINAL-BOUND :MAX-PENALTY and CROSSING-POLICY :STORED."
  (unless (member terminal-bound '(:best-fit :max-penalty))
    (error "Invalid terminal-bound policy: ~S" terminal-bound))
  (unless (member crossing-policy '(:candidate :stored))
    (error "Invalid crossing policy: ~S" crossing-policy))
  (let* ((num-parts (length starts))
         (state (make-many-voice-state
                 cantus starts mode species :seed seed :patterns patterns
                 :inner-durations inner-durations
                 :inner-half-notes inner-half-notes))
         (branch-limit (or branch-limit
                           (* 50 (- 6 num-parts) (- 6 species))))
         (total-time (aref state 2))
         (best 1000000)
         (max-penalty 1000000)
         (ratio (- 1 (/ (* species num-parts) 100)))
         (branches 0)
         (local-costs (when with-search-costs
                        (loop for voice from 1 to num-parts
                              collect (make-array (total-notes state voice)
                                                  :initial-element 0))))
         (results nil))
    (unless (and (integerp branch-limit) (plusp branch-limit))
      (error "Branch limit must be a positive integer: ~S" branch-limit))
    (labels ((bound (value)
               (if integer-bounds (truncate value) value))
             (save-result (cost)
               
               
               (let ((raw (when with-search-costs
                            (many-voice-lines state num-parts)))
                     (costs (when with-search-costs
                              (loop for vector in local-costs
                                    collect (coerce vector 'list)))))
                 (when with-search-costs
                   (unless (= cost (reduce #'+ (mapcar (lambda (line)
                                                      (reduce #'+ line))
                                                    costs)))
                     (error "Search cost mismatch")))
                 (many-voice-cadence-ficta state num-parts)
                 (setf best cost
                       max-penalty (bound (min max-penalty (* ratio cost))))
                 (let* ((lines (many-voice-lines state num-parts))
                        (record (append (cons cost lines)
                                        (when with-search-costs
                                          (list :search-local-costs
                                                (cons nil costs)
                                                :raw-voices raw
                                                :ficta-changes
                                                (loop for before in raw
                                                      for after in lines
                                                      for voice from 1
                                                      append (loop for a in before
                                                                   for b in after
                                                                   for note from 1
                                                                   unless (= a b)
                                                                     collect (list voice note a b))))))))
                   (push record results)
                   (when stream
                     (format stream "~% [~D]~%" cost)
                     (dolist (line lines)
                       (format stream "~{~D ~}~%" line))))))
             (visit (time accumulated)
               (when (<= accumulated max-penalty)
                 (incf branches)
                 (when (= branches branch-limit)
                   (setf max-penalty (bound (* ratio max-penalty))
                         branches 0))
                 (let* ((next-time
                          (many-voice-next-onset state time num-parts))
                        (notes (and next-time
                                    (many-voice-notes-at state next-time
                                                         num-parts))))
                   (when notes
                     (loop for tail on (many-voice-candidates
                                        state notes accumulated best
                                        num-parts species crossing-policy
                                        with-search-costs)
                           for option = (car tail)
                           for cost = (+ accumulated (first option))
                           do (when (>= cost
                                        (if (and (= next-time total-time)
                                                 (eq terminal-bound :best-fit))
                                            best max-penalty))
                                (return))
                              (dolist (choice (second option))
                                (let* ((voice (car choice))
                                       (note (cdr (assoc voice notes)))
                                       (previous (pitch-at state
                                                           (1- note) voice)))
                                  (set-pitch state note voice
                                             (+ previous
                                                (aref *search-intervals*
                                                      (cdr choice))))))
                              (when with-search-costs
                                (loop for choice in (second option)
                                      for local in (third option)
                                      for voice = (car choice)
                                      for note = (cdr (assoc voice notes))
                                      do (setf (aref (nth (1- voice)
                                                           local-costs)
                                                     (1- note))
                                               local)))
                              (if (< next-time total-time)
                                  (visit next-time cost)
                                  (save-result cost))
                              
                              
                              (when (and (zerop time) (cdr tail))
                                (setf max-penalty
                                      (bound (* ratio best))))))))))
      (visit 0 0)
      (nreverse results))))

(export 'generate-many-voice-counterpoint :fux)


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


;;;; generate-counterpoint: FUX implementation.

(in-package :fux)

(defun generate-counterpoint (cantus &key (species 1) starts
                                      (mode :dorian) (stream nil)
                                      (reference nil)
                                      (seed 1) patterns branch-limit with-rhythm
                                      with-search-costs
                                      inner-durations inner-half-notes)
  "Generate counterpoint for CANTUS using one to five starting pitches.
SPECIES is 1..5. All but the final generated voice use first-species
rules. INNER-DURATIONS or INNER-HALF-NOTES select earlier voices' rhythms
when there are three or more generated voices; they do not alter those
voices' rules. Return improving (COST VOICE...) records. REFERENCE selects
the tested C search settings. SEED and PATTERNS apply to species 5.
WITH-RHYTHM T adds onsets, durations and inferred ligatures."
  (unless (and (listp cantus) (>= (length cantus) 2)
               (every #'integerp cantus))
    (error "CANTUS must be a list of at least two integer pitches: ~S" cantus))
  (unless (member species '(1 2 3 4 5))
    (error "SPECIES must be 1 through 5: ~S" species))
  (unless (and (listp starts) (<= 1 (length starts) 5)
               (every #'integerp starts))
    (error "STARTS must contain one to five integer pitches: ~S" starts))
  (unless (assoc mode *modes*)
    (error "Unknown mode: ~S" mode))
  (when (and (/= species 5) patterns)
    (error "PATTERNS applies to fifth species only"))
  (when (and with-search-costs
             (not (or (and (= species 5) (= (length starts) 1))
                      (>= (length starts) 3))))
    (error "WITH-SEARCH-COSTS supports one fifth-species voice or 3..5 generated voices"))
  (when (and (< (length starts) 3)
             (or inner-durations inner-half-notes))
    (error "Inner-voice rhythms require at least three generated voices"))
  (when (and branch-limit (or (not (integerp branch-limit))
                             (not (plusp branch-limit))))
    (error "BRANCH-LIMIT must be a positive integer: ~S" branch-limit))
  (let* ((common (list :mode mode :stream stream
                       :integer-bounds (not (null reference))
                       :terminal-bound (if reference :max-penalty :best-fit)))
         (crossing (list :crossing-policy
                         (if reference :stored :candidate)))
         (rhythm (if (= species 5)
                     (list :seed seed :patterns patterns)
                     nil))
         (limits (if branch-limit (list :branch-limit branch-limit) nil))
         (args (append common rhythm limits)))
    (let ((records
            (cond
              ((= (length starts) 1)
               (apply (ecase species
                        (1 #'generate-first-species)
                        (2 #'generate-second-species)
                        (3 #'generate-third-species)
                        (4 #'generate-fourth-species)
                        (5 #'generate-fifth-species))
                      cantus (first starts)
                      (append args crossing
                              (when with-search-costs
                                (list :with-search-costs t)))))
              ((>= (length starts) 3)
               (apply #'generate-many-voice-counterpoint
                      cantus starts species
                      (append args crossing
                              (list :inner-durations inner-durations
                                    :inner-half-notes inner-half-notes
                                    :with-search-costs with-search-costs))))
              ((= species 1)
               
               (apply #'generate-poly-first-species cantus starts args))
              ((= species 2)
               (apply #'generate-mixed-second-species cantus starts
                      (append args crossing)))
              (t
               (apply #'generate-mixed-species cantus starts species
                      (append args crossing))))))
      (if with-rhythm
          (counterpoint-records-with-rhythm records cantus starts species
                                            mode seed patterns
                                            :inner-durations inner-durations
                                            :inner-half-notes inner-half-notes)
          records))))

(defun best-counterpoint (records)
  "Return the final improvement record, or NIL when no result was found."
  (car (last records)))

(export '(generate-counterpoint best-counterpoint) :fux)


;;;; fux-cmn: FUX implementation.

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
                                          title
					  (output-file "counterpoint.eps")
					  (size 20)
                                          staff-order (score-notes nil)
                                          (mode :dorian)
                                          (crossing-policy :stored) note-costs)
  "Write a CMN form from one :WITH-RHYTHM T improvement RECORD.
STAFF-ORDER lists zero-based indices into :VOICES, from top to bottom;
default is reverse generator order. SCORE-NOTES :SEARCH uses captured
candidate costs before cadential pitch changes; it requires
:WITH-SEARCH-COSTS T during
generation. SCORE-NOTES T literally rescores the notated line with MODE
and CROSSING-POLICY; that can differ from the search cost after ficta.
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
      (setf note-costs
            (if (eq score-notes :search)
                (let* ((costs (getf record :search-local-costs))
                       (per-voice (if (= (length voices) 2)
                                      (list nil costs)
                                      costs)))
                  (unless (and per-voice
                               (= (length per-voice) (length voices))
                               (null (first per-voice))
                               (loop for data in (rest voices)
                                     for line in (rest per-voice)
                                     always (and (listp line)
                                                 (= (length line)
                                                    (length (getf data :pitches)))
                                                 (every #'numberp line)))
                               (= (loop for line in (rest per-voice)
                                        sum (reduce #'+ line))
                                  (getf record :penalty)))
                    (error "CMN writer: missing or inconsistent captured search costs"))
                  per-voice)
                (counterpoint-cmn-note-costs record mode crossing-policy))))
    (format stream "(in-package :cmn)~%;;; FUX search penalty: ~D~%"
            (getf record :penalty))
    (when (eq score-notes :search)
      (format stream ";;; Note fingerings are search costs before cadential pitch changes.~%")
      (when (getf record :ficta-changes)
        (format stream ";;; Cadential pitch changes (one voice: note/raw/notated; multiple voices: voice/note/raw/notated): ~S~%"
                (getf record :ficta-changes))))
    (format stream "(cmn (output-file ~S) (size ~D) (title ~S) ~%" output-file size title)
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
