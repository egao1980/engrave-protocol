(in-package #:engrave-protocol/tests)

;;; timemap.lisp: SCORE-TIMEMAP and RENDER-TIMEMAP.

(defun tempo (bpm &optional (beat-unit 1/4))
  (mt:make-tempo :bpm bpm :beat-unit beat-unit))

(defun entry-rows (timemap)
  "TIMEMAP as (onset on off) rows."
  (mapcar (lambda (e) (list (timemap-entry-onset e) (timemap-entry-on e) (timemap-entry-off e)))
          timemap))

;;; sc0-key-change, by hand. Quarter = 100 (600 ms) in bars 1-3 and the first
;;; half of bar 4 (3/4); quarter = 80 (750 ms) from bar 4 + 1/2 (the text-only
;;; "poco rit." at bar 4 + 0 changes nothing); dotted quarter = 60 (1000 ms)
;;; from bar 5 (6/8). Bar starts: 0, 1, 2, 3, 15/4, 9/2 (whole notes).
(defparameter +key-change-timemap+
  '((0 ("e-f") ())                     ; 0
    (600 ("e-g") ("e-f"))              ; 1/4
    (1200 ("e-h") ("e-g"))             ; 1/2
    (1800 ("e-i") ("e-h"))             ; 3/4
    (2400 ("e-j") ("e-i"))             ; 1
    (3600 ("e-k") ("e-j"))             ; 3/2
    (4800 ("e-l") ("e-k"))             ; 2
    (5400 ("e-m") ("e-l"))             ; 9/4
    (6000 ("e-n") ("e-m"))             ; 5/2
    (7200 ("e-o") ("e-n"))             ; 3
    (7800 ("e-p") ("e-o"))             ; 13/4
    (8400 ("e-q") ("e-p"))             ; 7/2, tempo change: 7200 + 2 x 600
    (9150 ("e-r") ("e-q"))             ; 15/4: 8400 + 750
    (10150 ("e-s") ("e-r"))            ; 33/8: + 1000
    (11150 ("e-t") ("e-s"))            ; 9/2: + 1000
    (13150 () ("e-t"))))               ; 21/4: + 2 x 1000

(deftest timemap-by-hand
  (let ((tm (score-timemap (fixture-score "sc0-key-change"))))
    (ok (equal +key-change-timemap+ (entry-rows tm)))
    (ok (every #'timemap-entry-p tm))
    (ok (equalp (append (make-list 11 :initial-element (tempo 100))
                        (list (tempo 80))
                        (make-list 4 :initial-element (tempo 60 3/8)))
                (mapcar #'timemap-entry-tempo tm))
        "the tempo in force at each onset")
    (ok (every #'rationalp (mapcar #'timemap-entry-onset tm)) "exact")))

(deftest timemap-default-tempo
  (let ((tm (score-timemap (fixture-score "sc0-repeats-voltas"))))
    (ok (equalp +default-tempo+ (tempo 120)))
    (ok (every (lambda (e) (equalp (tempo 120) (timemap-entry-tempo e))) tm) "no tempo mark: 120")
    (ok (equal '(0 ("e-i") ()) (first (entry-rows tm))))
    (ok (equal '(500 ("e-j") ("e-i")) (second (entry-rows tm))) "a quarter is 500 ms")))

(defun constant-tempo-ms (score offset)
  "OFFSET (whole notes) in ms when SCORE has one tempo from its start, or the default."
  (let* ((marks (loop for m in (fset:convert 'list (sp:score-measure-map score))
                      append (remove-if-not #'sp:tempo-mark-tempo (sp:measure-info-tempo m))))
         (tempo (if marks (sp:tempo-mark-tempo (first marks)) (tempo 120))))
    (* offset (/ 1 (mt:tempo-beat-unit tempo)) (/ 60000 (mt:tempo-bpm tempo)))))

(defparameter +one-tempo-fixtures+
  '("sc0-carol-6-8" "sc0-chords-under-rests" "sc0-jumps" "sc0-keyboard" "sc0-omr-result"
    "sc0-pickup" "sc0-repeats-voltas" "sc0-satb" "sc0-three-verses" "sc1-open-score")
  "Fixtures with at most one tempo mark, at the start.")

(deftest-parametrize timemap-onsets-at-one-tempo
    ((name) :rows +one-tempo-fixtures+)
  (let* ((s (fixture-score name))
         (tm (score-timemap s))
         (starts (make-hash-table :test 'equal))
         (ends (make-hash-table :test 'equal)))
    (dolist (e (sp:score-elements s))
      (when (timed-event-p e)
        (let ((x (sp:absolute-offset s (sp:element-position s (sp:element-id e)))))
          (setf (gethash (sp:element-id e) starts) x
                (gethash (sp:element-id e) ends) (+ x (sp:event-duration e))))))
    (ok (every (lambda (en)
                 (and (every (lambda (id) (= (timemap-entry-onset en)
                                             (constant-tempo-ms s (gethash id starts -1))))
                             (timemap-entry-on en))
                      (every (lambda (id) (= (timemap-entry-onset en)
                                             (constant-tempo-ms s (gethash id ends -1))))
                             (timemap-entry-off en))))
               tm)
        (format nil "~A: onsets from offsets" name))
    (ok (equal (sort (remove-duplicates (append (loop for v being the hash-values of starts collect v)
                                                (loop for v being the hash-values of ends collect v)))
                     #'<)
               (mapcar (lambda (en) (/ (timemap-entry-onset en) (constant-tempo-ms s 1))) tm))
        (format nil "~A: one entry per instant" name))))

(deftest-parametrize timemap-shape
    ((name) :rows +fixture-names+)
  (let* ((s (fixture-score name))
         (tm (score-timemap s))
         (on (loop for e in tm append (timemap-entry-on e)))
         (off (loop for e in tm append (timemap-entry-off e))))
    (ok (zerop (timemap-entry-onset (first tm))) (format nil "~A: starts at 0" name))
    (ok (loop for (a b) on (mapcar #'timemap-entry-onset tm) while b always (< a b))
        (format nil "~A: strictly increasing" name))
    (ok (and (timemap-entry-off (car (last tm))) (null (timemap-entry-on (car (last tm)))))
        (format nil "~A: the last entry only ends events" name))
    (ok (equal (sort (copy-list (timed-ids s)) #'string<) (sort (copy-list on) #'string<))
        (format nil "~A: every timed event starts once" name))
    (ok (equal (sort (copy-list (timed-ids s)) #'string<) (sort (copy-list off) #'string<))
        (format nil "~A: and ends once" name))
    (ok (every (lambda (en)
                 (let ((positions (mapcar (lambda (id) (position id (timed-ids s) :test #'equal))
                                          (timemap-entry-on en))))
                   (equal positions (sort (copy-list positions) #'<))))
               tm)
        (format nil "~A: ON in document order" name))))

(deftest timemap-grace-and-chords
  (let* ((s (fixture-score "sc0-keyboard"))
         (ids (loop for e in (score-timemap s) append (append (timemap-entry-on e) (timemap-entry-off e))))
         (graces (loop for e in (sp:score-elements s)
                       when (and (typep e 'sp:sounding-event) (sp:event-grace e)) collect (sp:element-id e)))
         (chord-notes (loop for e in (sp:score-elements s)
                            when (typep e 'sp:chord-note) collect (sp:element-id e)))
         (chords (loop for e in (sp:score-elements s)
                       when (typep e 'sp:chord-event) collect (sp:element-id e)))
         (rests (loop for e in (sp:score-elements s)
                      when (typep e 'sp:rest-event) collect (sp:element-id e))))
    (ok graces "the fixture has a grace note")
    (ok (notany (lambda (id) (member id ids :test #'equal)) graces) "grace notes take no time")
    (ok (notany (lambda (id) (member id ids :test #'equal)) chord-notes) "chord notes are not listed")
    (ok (every (lambda (id) (member id ids :test #'equal)) chords) "their chords are")
    (ok (every (lambda (id) (member id ids :test #'equal)) rests) "rests are timed")))

(deftest timemap-empty
  (ok (null (score-timemap (empty-score))))
  (ok (null (render-timemap (make-null-engine) (empty-score))))
  (ok (typep (signalled (lambda () (score-timemap 42))) 'type-error)))

;;; RENDER-TIMEMAP ------------------------------------------------------------------

(defun playback-order (score)
  "The source ids of the timed events of (unfold SCORE), by start, document
order within one start; and the sorted distinct instants (whole notes)."
  (multiple-value-bind (view map) (sp:unfold score)
    (let* ((events (remove-if-not #'timed-event-p (sp:score-elements view)))
           (start (lambda (e) (sp:absolute-offset view (sp:element-position view (sp:element-id e))))))
      (values (mapcar (lambda (e) (fset:lookup map (sp:element-id e)))
                      (stable-sort (copy-list events) #'< :key start))
              (sort (remove-duplicates
                     (loop for e in events
                           collect (funcall start e)
                           collect (+ (funcall start e) (sp:event-duration e))))
                    #'<)))))

(deftest-parametrize timemap-follows-unfold
    ((name) :rows '("sc0-repeats-voltas" "sc0-jumps" "sc0-satb" "sc0-pickup" "sc0-carol-6-8"))
  (let* ((s (fixture-score name))
         (tm (render-timemap (make-null-engine) s)))
    (multiple-value-bind (order instants) (playback-order s)
      (ok (equal order (loop for e in tm append (timemap-entry-on e)))
          (format nil "~A: ON follows playback order, source ids" name))
      (ok (equal (mapcar (lambda (x) (constant-tempo-ms s x)) instants)
                 (mapcar #'timemap-entry-onset tm))
          (format nil "~A: onsets of the unfolded score" name)))
    (ok (null (check-timemap s tm)) (format nil "~A: CHECK-TIMEMAP" name))))

(deftest timemap-repeats
  (let* ((s (fixture-score "sc0-repeats-voltas"))
         (tm (render-timemap (make-null-engine) s))
         (on (loop for e in tm append (timemap-entry-on e))))
    (ok (= 2 (count "e-i" on :test #'equal)) "the first section twice")
    (ok (= 1 (count "e-o" on :test #'equal)) "the first ending once")
    (ok (= 1 (count "e-t" on :test #'equal)) "the second ending once")
    (ok (= 3 (count "e-u" on :test #'equal)) "the second section three times")
    (ok (= 1 (count "e-10" on :test #'equal)) "the third-pass ending once")))

(deftest render-timemap-arguments
  (let ((s (fixture-score "sc0-repeats-voltas"))
        (e (make-null-engine)))
    (ok (equalp (render-timemap e s) (render-timemap e s :unfold t)) ":unfold defaults to true")
    (ok (equalp (score-timemap s) (render-timemap e s :unfold nil)) ":unfold nil: SCORE-TIMEMAP")
    (ok (null (check-timemap s (render-timemap e s :unfold nil) :unfold nil)))
    (ok (equalp (score-timemap (fixture-score "sc0-key-change"))
                (render-timemap e (fixture-score "sc0-key-change")))
        "no repeats: the same either way")
    (ok (typep (signalled (lambda () (render-timemap e 42))) 'type-error))
    (let ((*engrave-engine* nil))
      (ok (typep (signalled (lambda () (render-timemap nil s))) 'engine-unavailable)))
    (let ((*engrave-engine* e))
      (ok (equalp (render-timemap e s) (render-timemap nil s)) "*ENGRAVE-ENGINE*"))
    (ok (typep (signalled (lambda () (render-timemap (make-instance 'offline-engine) s)))
               'engine-unavailable))
    (ok (equalp (render-timemap e s)
                (handler-bind ((engine-unavailable (lambda (c) (use-value e c))))
                  (render-timemap (make-instance 'offline-engine) s)))
        "USE-VALUE")
    (ok (equalp (render-timemap e s) (render-timemap (make-instance 'plain-engine) s))
        "every engine inherits the model's timemap")))
