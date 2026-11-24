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

(defun score-timemap (score)
  "The timemap of SCORE without unfolding (file header)."
  (declare (ignore score))
  (%en0-pending 'score-timemap))

(defgeneric render-timemap (engine score &key unfold)
  (:documentation "SCORE's timemap, unfolded into playback order by default. The file
header of timemap.lisp has the rules."))

(defmethod render-timemap ((engine null) score &key (unfold t))
  (declare (ignore score unfold))
  (%en0-pending 'render-timemap/null))

(defmethod render-timemap ((engine engrave-engine) score &key (unfold t))
  (declare (ignore score unfold))
  (%en0-pending 'render-timemap/engrave-engine))
