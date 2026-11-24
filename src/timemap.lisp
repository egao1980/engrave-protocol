;;;; EN0 timemaps: when each event sounds, for playback cursors.
;;;;
;;;; A TIMEMAP is a list of TIMEMAP-ENTRYs (read-only struct, EQUALP-comparable)
;;;; in increasing ONSET order:
;;;;   ONSET  milliseconds from the start, a real >= 0 (exact rationals from
;;;;          SCORE-TIMEMAP; an engine of its own may round)
;;;;   ON     the ids of the events that start at ONSET
;;;;   OFF    the ids of the events that end at ONSET
;;;;   TEMPO  the music-theory TEMPO in force at ONSET
;;;; MAKE-TIMEMAP-ENTRY &key onset on off tempo stores them as given; an ONSET that
;;;; is not a real >= 0, or an ON or OFF that is not a list, is a TYPE-ERROR.
;;;;
;;;; The timemap comes from the model, not from an engine's layout: event
;;;; positions, durations and tempo marks fully decide it, so every engine
;;;; reports the same one, consistent with score-protocol's UNFOLD. An engine
;;;; whose playback differs (one that realises ornaments) may specialize
;;;; RENDER-TIMEMAP.
;;;;
;;;; SCORE-TIMEMAP score → the timemap of SCORE as it stands (no unfolding):
;;;;   Timed events: every event of SCORE (note, chord or rest, in every voice)
;;;;   that is not a grace event (SCORE-PROTOCOL:EVENT-GRACE NIL; rests have
;;;;   none). Grace events take no time and are not in the timemap. Chord notes
;;;;   are not listed: their chord event is.
;;;;   An event's START is the ABSOLUTE-OFFSET of its ELEMENT-POSITION (whole
;;;;   notes); its END is START + EVENT-DURATION.
;;;;   Tempo. The change points are, for every bar in bar order and every tempo
;;;;   mark of its measure that has a TEMPO (in the measure's TEMPO order), the
;;;;   pair (BAR-START + the mark's OFFSET . its TEMPO). The tempo in force at
;;;;   offset X is that of the last change point at or before X, so of two marks
;;;;   at one instant the later one wins; before the first change point, and in a
;;;;   score with none, it is +DEFAULT-TEMPO+ (a quarter note = 120, music-theory's
;;;;   MAKE-TEMPO default and MIDI's default tempo). Text-only marks (TEMPO NIL)
;;;;   change nothing. Fermatas and breath marks add no time.
;;;;   MS(X) = 1000 x the sum of MUSIC-THEORY:DURATION-SECONDS over the stretches
;;;;   from 0 to X between change points, each at its tempo: an exact rational.
;;;;   Entries: one per instant X at which a timed event starts or ends, in
;;;;   increasing X, with ONSET MS(X), ON the ids of the timed events whose START
;;;;   is X and OFF those whose END is X (each list in document order,
;;;;   SCORE-ELEMENTS), TEMPO the tempo in force at X. So the last entry has
;;;;   only OFF ids. A score without timed events gives ().
;;;;   SCORE not a SCORE-PROTOCOL:SCORE → TYPE-ERROR.
;;;;
;;;; RENDER-TIMEMAP engine score &key (unfold t) → a timemap of SCORE's ids.
;;;; Methods:
;;;;   (ENGINE NULL): (render-timemap (ensure-engine nil) score :unfold unfold).
;;;;   (ENGINE ENGRAVE-ENGINE):
;;;;     1. As RENDER-SVG's step 1 (render.lisp), with RENDER-TIMEMAP.
;;;;     2. SCORE not a SCORE-PROTOCOL:SCORE → TYPE-ERROR.
;;;;     3. UNFOLD true: (score-protocol:unfold score) → VIEW and ID-MAP; the
;;;;        result is (score-timemap VIEW) with every id in ON and OFF replaced
;;;;        by its source id (lookup ID-MAP id), order kept. So an event played
;;;;        on several passes appears once per pass, under its own id, and the
;;;;        timemap follows playback order.
;;;;        UNFOLD false: (score-timemap score), every event once.

(in-package #:engrave-protocol)

(defparameter +default-tempo+ (music-theory:make-tempo)
  "The tempo before the first tempo mark: a quarter note = 120.")

(defstruct (timemap-entry (:constructor make-timemap-entry (&key onset on off tempo)))
  "One instant of a timemap (file header)."
  (onset 0 :type (real 0) :read-only t)
  (on '() :type list :read-only t)
  (off '() :type list :read-only t)
  (tempo nil :read-only t))

(defun %timed-event-p (element)
  "True when ELEMENT is a timed event: an event that is not a grace event."
  (and (typep element 'score-protocol:event)
       (not (and (typep element 'score-protocol:sounding-event)
                 (score-protocol:event-grace element)))))

(defun %tempo-changes (score)
  "SCORE's tempo change points (file header), ((offset . tempo) ...) in time
order; of two at one instant the later in bar and TEMPO order comes last."
  (stable-sort
   (loop for bar from (score-protocol:score-first-bar score) to (score-protocol:score-last-bar score)
         for start = (score-protocol:bar-start score bar)
         nconc (loop for mark in (score-protocol:measure-info-tempo
                                  (score-protocol:bar-measure score bar))
                     for tempo = (score-protocol:tempo-mark-tempo mark)
                     when tempo
                       collect (cons (+ start (score-protocol:tempo-mark-offset mark)) tempo)))
   #'< :key #'car))

(defun %ms-and-tempo (x changes)
  "(values MS(X) tempo-in-force-at-X) over the change points CHANGES: exact."
  (let ((seconds 0) (from 0) (tempo +default-tempo+))
    (loop for (offset . next) in changes
          while (<= offset x)
          do (incf seconds (music-theory:duration-seconds (- offset from) tempo))
             (setf from offset
                   tempo next))
    (values (* 1000 (+ seconds (music-theory:duration-seconds (- x from) tempo)))
            tempo)))

(defun score-timemap (score)
  "The timemap of SCORE without unfolding (file header)."
  (check-type score score-protocol:score)
  (let ((changes (%tempo-changes score))
        (on (make-hash-table))          ; instant → ids, newest first (EQL: rationals)
        (off (make-hash-table))
        (instants '()))
    (dolist (element (score-protocol:score-elements score))
      (when (%timed-event-p element)
        (let* ((id (score-protocol:element-id element))
               (start (score-protocol:absolute-offset
                       score (score-protocol:element-position score id)))
               (end (+ start (score-protocol:event-duration element))))
          (push id (gethash start on))
          (push id (gethash end off))
          (push start instants)
          (push end instants))))
    (loop for x in (sort (remove-duplicates instants) #'<)
          collect (multiple-value-bind (ms tempo) (%ms-and-tempo x changes)
                    (make-timemap-entry :onset ms
                                        :on (reverse (gethash x on))
                                        :off (reverse (gethash x off))
                                        :tempo tempo)))))

(defgeneric render-timemap (engine score &key unfold)
  (:documentation "SCORE's timemap, unfolded into playback order by default. The file
header of timemap.lisp has the rules."))

(defmethod render-timemap ((engine null) score &key (unfold t))
  (render-timemap (ensure-engine nil) score :unfold unfold))

(defmethod render-timemap ((engine engrave-engine) score &key (unfold t))
  ;; 1. The engine, as in RENDER-SVG.
  (let ((e (ensure-engine engine)))
    (unless (eq e engine)
      (return-from render-timemap (render-timemap e score :unfold unfold))))
  ;; 2. The score.
  (check-type score score-protocol:score)
  ;; 3. Unfolded into playback order, under the source ids; or as it stands.
  (if unfold
      (multiple-value-bind (view id-map) (score-protocol:unfold score)
        (flet ((source (id) (values (fset:lookup id-map id))))
          (mapcar (lambda (entry)
                    (make-timemap-entry :onset (timemap-entry-onset entry)
                                        :on (mapcar #'source (timemap-entry-on entry))
                                        :off (mapcar #'source (timemap-entry-off entry))
                                        :tempo (timemap-entry-tempo entry)))
                  (score-timemap view))))
      (score-timemap score)))
