;;;; EN0 views: which score an engine draws.
;;;;
;;;; Views are score-protocol's (SC1). An engine never sees a view option: the
;;;; protocol computes the score to draw and hands that to ENGINE-RENDER-PAGES,
;;;; so every engine draws the same view the same way.
;;;;
;;;; ENGRAVE-VIEW score &key (view :choral) parts → (values view-score view-map)
;;;;   VIEW-MAP: an FSet map from the id of every element of VIEW-SCORE that
;;;;   comes from SCORE to the id of the element of SCORE it comes from.
;;;;   PARTS: NIL (every part), or a non-empty list of distinct part ids of SCORE.
;;;;   The view then starts from SCORE restricted to those parts:
;;;;     (copy-score score :parts <those parts, in SCORE's order>
;;;;                       :spanners <SCORE's spanners whose START and END are
;;;;                                  both events of those parts, in order>)
;;;;   which keeps the measure map, the chord-symbol anchors, the metadata and
;;;;   the id counter.
;;;;   VIEW, one of +VIEWS+:
;;;;     :CHORAL     the score as notated: every part on its own staves, each
;;;;                 voice on the staff it names. In an open choral score (one
;;;;                 part per voice) that is one staff per part. Without PARTS
;;;;                 the result is SCORE itself (EQ) and VIEW-MAP sends every
;;;;                 element id of SCORE to itself; with PARTS, the restricted
;;;;                 score and every one of its element ids to itself.
;;;;     :CONDENSED  (score-protocol:condensed-view s) of that score S, with its
;;;;                 default voices (the first four voices in document order):
;;;;                 up to four voices on two staves, the hymnal reduction. The
;;;;                 result is CONDENSED-VIEW's score and its id map; the new
;;;;                 part and staves it mints are not in the map, and the voices
;;;;                 it leaves out are not in the view.
;;;;   Checks, in order: SCORE not a SCORE-PROTOCOL:SCORE → TYPE-ERROR. VIEW not
;;;;   in +VIEWS+ → INVALID-RENDER-OPTION (OPTION :view, VALUE the argument).
;;;;   PARTS neither NIL nor a non-empty proper list of distinct strings each the
;;;;   id of a part of SCORE → INVALID-RENDER-OPTION (OPTION :parts). Errors that
;;;;   CONDENSED-VIEW signals (a score without voices) are score-protocol's
;;;;   own and are not wrapped.

(in-package #:engrave-protocol)

(defparameter +views+ '(:choral :condensed)
  "The VIEW values of ENGRAVE-VIEW and RENDER-SVG. Data.")

(defun engrave-view (score &key (view :choral) parts)
  "The score an engine draws for VIEW and PARTS, and its id map (file header)."
  (declare (ignore score view parts))
  (%en0-pending 'engrave-view))
