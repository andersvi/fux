;;;; Source-level contract of the first-species SaveResults port.
(in-package :fux)

(defun run-first-species-save-contract-tests ()
  "Check optional detail without changing the historic legacy output."
  (let ((cantus '(43 48 47 43 48 52 50 55 52 48 50 47 45 43))
        (args (list :mode :mixolydian :stream nil
                    :integer-bounds t :terminal-bound :max-penalty
                    :crossing-policy :stored)))
    (dolist (case '((55 63) (43 41)))
      (let* ((start (first case))
             (legacy (apply #'generate-first-species cantus start args))
             (details (apply #'generate-first-species cantus start
                             :result-format :with-ficta args)))
        (assert (= (length details) (length legacy)))
        (loop for old in legacy for new in details
              do (assert (= (first old) (getf new :search-penalty)))
                 (assert (equal (second old) (getf new :notated-pitches)))
                 (assert (= (length cantus)
                            (length (getf new :notated-local-penalties))))
                 (assert (= (getf new :notated-penalty)
                            (reduce #'+ (getf new :notated-local-penalties))))
                 (multiple-value-bind (independent-cost independent-line locals)
                     (score-exs-line (if (= start 55) 7 8)
                                     (getf new :notated-pitches)
                                     :sequential t)
                   (assert (equal independent-line
                                  (getf new :notated-pitches)))
                   (assert (= independent-cost
                              (getf new :notated-penalty)))
                   (assert (equal locals
                                  (getf new :notated-local-penalties))))
                 (assert (equal (getf new :ficta-changes)
                                (loop for before in (getf new :raw-pitches)
                                      for after in (getf new :notated-pitches)
                                      for i from 1 unless (= before after)
                                        collect (list i before after)))))
        ;; C EXS07/08: note 11 is raised from 53 to 54 on first save.
        (assert (= (getf (first details) :search-penalty) (second case)))
        (assert (equal (getf (first details) :ficta-changes)
                       '((11 53 54))))
        (let ((first-save (first details)))
          (multiple-value-bind (total locals raised)
              (score-first-species-post-ficta
               cantus (getf first-save :raw-pitches)
               (getf first-save :notated-pitches) :mode :mixolydian
               :provenance (getf first-save :ficta-provenance))
            (assert (equal raised '(11)))
            (assert (equal (getf first-save :ficta-provenance)
                           '((11 53 54))))
            (assert (= total (reduce #'+ locals)))
            (assert (= (length locals) (length cantus)))
            (when (= start 55)
              (assert (= total 66))
              (assert (= (getf first-save :notated-penalty)
                         1000066))))
          ;; An arbitrary chromatic edit must not gain the exemption.
          (let ((edited (copy-list (getf first-save :notated-pitches))))
            (incf (nth 3 edited))
            (assert (handler-case
                        (progn (score-first-species-post-ficta
                                cantus (getf first-save :raw-pitches)
                                edited :mode :mixolydian)
                               nil)
                      (error () t)))))
        (when (= start 43)
          ;; In C EXS08 the [22] line already contains the earlier ficta
          ;; in its scored candidate. SaveResults raises nothing this time.
          ;; This guards the historical search-state side effect.
          (let ((inherited (find 22 details
                                 :key (lambda (x) (getf x :search-penalty)))))
            (assert inherited)
            (assert (null (getf inherited :ficta-changes)))
            (assert (equal (getf inherited :raw-pitches)
                           (getf inherited :notated-pitches)))
            (assert (= (nth 10 (getf inherited :raw-pitches)) 54))
            (assert (= (nth 11 (getf inherited :raw-pitches)) 51))
            ;; No new ficta in this save, but note 11 retains its
            ;; proven origin from an earlier SaveResults call.
            (assert (equal (getf inherited :ficta-provenance)
                           '((11 53 54))))
            (multiple-value-bind (total locals raised)
                (score-first-species-post-ficta
                 cantus (getf inherited :raw-pitches)
                 (getf inherited :notated-pitches) :mode :mixolydian
                 :provenance (getf inherited :ficta-provenance))
              (assert (equal raised '(11)))
              (assert (< total (getf inherited :notated-penalty)))
              ;; Note 12 is 51, a non-modal pitch produced when the
              ;; cached interval is applied after note 11 was raised.
              ;; Exempting note 11 must not excuse note 12.
              (assert (= (nth 11 (getf inherited :notated-pitches)) 51))
              (assert (>= (nth 11 locals) (penalty :out-of-mode))))))))
    ;; The C experiment restoring Ctrpt after SaveResults keeps the same
    ;; seven EXS08 costs; at [22] notes 11/12 become 53/50. Exercise the
    ;; separate CL policy without changing the historical default.
    (let* ((restored (generate-first-species
                      cantus 43 :mode :mixolydian :stream nil
                      :integer-bounds t :terminal-bound :max-penalty
                      :crossing-policy :stored :save-state-policy :restore
                      :result-format :with-ficta))
           (twenty-two (find 22 restored
                             :key (lambda (x) (getf x :search-penalty)))))
      (assert (= (length restored) 7))
      (assert twenty-two)
      (assert (equal (getf twenty-two :notated-pitches)
                     '(43 45 50 52 52 55 53 52 55 52 53 50 54 55)))
      (assert (null (getf twenty-two :ficta-provenance)))
      (multiple-value-bind (total locals exempted)
          (score-first-species-post-ficta
           cantus (getf twenty-two :raw-pitches)
           (getf twenty-two :notated-pitches) :mode :mixolydian
           :provenance (getf twenty-two :ficta-provenance))
        (declare (ignore total))
        (assert (null exempted))
        (assert (every (lambda (cost) (< cost (penalty :infinity)))
                       locals))))
    (let ((line '(57 57 55 59 59 62 60 64 62 61 62)))
      (multiple-value-bind (total locals)
          (score-first-species-line
           '(50 53 52 50 55 53 57 55 53 52 50) line)
        (assert (= total 11))
        (assert (= (length locals) (length line)))
        (assert (= (first locals) 0))
        (assert (equal line '(57 57 55 59 59 62 60 64 62 61 62)))))
    t))
