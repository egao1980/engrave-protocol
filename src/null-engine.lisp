;;;; EN0 NULL-ENGINE: the reference engine. Deterministic placeholder SVG.
;;;;
;;;; It lays out no music: every element of the view becomes one SVG group,
;;;; systems are runs of measures and pages runs of systems, by the arithmetic
;;;; below. It exists so that the protocol (id maps, page splitting, system
;;;; breaks, the embed path, the conditions) can be tested without a native
;;;; engine, and so that consumers (editors, canvases) can test against a fast
;;;; engine. Same view and layout → the same pages, byte for byte.
;;;;
;;;; MAKE-NULL-ENGINE &key unsupported → a NULL-ENGINE. UNSUPPORTED is a list of
;;;; class names (symbols); NULL-ENGINE-UNSUPPORTED reads it. ENGINE-NAME is
;;;; "null"; ENGINE-AVAILABLE-P is T.
;;;;
;;;; ENGINE-RENDER-PAGES engine view layout &key page → (values pages id-map).
;;;; It renders every page whatever PAGE is.
;;;;
;;;; Drawn and skipped. The engine visits (score-elements view) in document
;;;; order. An element is skipped when one of its holders is: its
;;;; ELEMENT-PARENT; for an event also the measure of its bar; for a
;;;; chord-symbol anchor the measure of its position's bar; for a spanner its
;;;; START event. (Document order visits every holder first: measures, then
;;;; parts with their events, then anchors, then spanners.) Otherwise, when
;;;; (typep element name) for a NAME in UNSUPPORTED, it signals
;;;; UNSUPPORTED-NOTATION (ENGINE the engine, ELEMENT-ID the element's id, KIND
;;;; (class-name (class-of element))) with ERROR inside a SKIP-ELEMENT restart
;;;; (no arguments); taking it skips the element. Every other element is drawn.
;;;; So the conditions come in document order, and nothing a skipped element
;;;; holds is signalled about or drawn.
;;;;
;;;; Geometry, in user units of 1/10 mm, with f = SCALE / 100:
;;;;   W, H          the page width and height x 10
;;;;   TOP RIGHT BOTTOM LEFT  the margins x 10
;;;;   CW = W - LEFT - RIGHT, CH = H - TOP - BOTTOM   (both > 0 by MAKE-LAYOUT)
;;;;   MW = 400 f                 the width of a measure
;;;;   N  = the number of staves of the view's parts (all of them)
;;;;   SH = (100 N + 50) f        the height of a system, spacing included
;;;;   MEASURES-PER-SYSTEM = max(1, floor(CW / MW))
;;;;   SYSTEMS-PER-PAGE    = max(1, floor(CH / SH))
;;;; Systems: the measures of the view in order, split with :AUTO breaks into
;;;; runs of MEASURES-PER-SYSTEM (the last run may be shorter); with a list of
;;;; breaks, a system starts at the first measure and at every measure whose id
;;;; is in the list. Systems are numbered 1, 2, ... over the whole rendering.
;;;; Pages: the systems in order, SYSTEMS-PER-PAGE to a page (the last page may
;;;; hold fewer), numbered 1, 2, ...; a view with no measures gives one page with
;;;; no systems. SVG-PAGE-SYSTEMS of a page lists its systems' measure ids.
;;;;
;;;; Numbers in attributes are written in decimal: an integer as itself, any
;;;; other rational X as (round (* X 100)) / 100 with no trailing zeros (49.5,
;;;; 0.33); never as a ratio. Every element is made with MAKE-SVG-ELEMENT, so it
;;;; is in +SVG-NAMESPACE+. An element's group is <g id=ID class=KIND>, KIND the
;;;; element's ELEMENT-KIND in lower case ("chord-symbol", "measure").
;;;;
;;;; Page k, an xml-document (version 1.0, UTF-8) whose root is
;;;;   <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 W H"
;;;;        width="<page width>mm" height="<page height>mm"
;;;;        class="engrave-page" data-page="k">
;;;; holding, in order:
;;;;   1. On page 1 only: <g class="parts"> holding, for every drawn part, its
;;;;      group, which holds a <text> with the part's NAME (when it has one),
;;;;      then the groups of its drawn staves and voices in document order
;;;;      (staves first). Staff and voice groups are empty: events are drawn in
;;;;      their measures.
;;;;   2. For each system on the page:
;;;;        <g id="system.J" class="system" transform="translate(LEFT,Y)">
;;;;      J the system's number, Y = TOP + i x SH for the i-th system of the page
;;;;      (from 0). It holds the groups of its drawn measures, in order, then
;;;;      the groups of the drawn spanners whose START event lies in one of its
;;;;      measures, in document order.
;;;; A measure's group (transform="translate(X,0)", X = j x MW for the j-th
;;;; measure of its system, from 0) holds, in order: <rect width=MW height=100 N
;;;; f/>; the groups of the measure's drawn marks (ending, jump marks, rehearsal
;;;; mark, tempo marks) in document order; the groups of the drawn chord-symbol
;;;; anchors whose position's bar is this measure's bar, in track order; the
;;;; groups of the drawn events of this measure in every voice, in document
;;;; order (part, voice, event). A skipped measure keeps its place in its
;;;; system (and in SVG-PAGE-SYSTEMS), without a group.
;;;; An event's group holds the groups of its drawn element-tree children, in
;;;; document order (chord notes, each followed by its tie; the note's tie;
;;;; articulations, fermata, breath mark, dynamic, lyrics). Chord-note groups
;;;; hold their tie's group. All other groups are empty.
;;;;
;;;; The id map: every element of VIEW maps to ((k . ID)), k the page its group
;;;; is on (1 for parts, staves and voices), or to () when it is not drawn.
;;;; A drawn element always has exactly one group, under its own id.

(in-package #:engrave-protocol)

(defclass null-engine (engrave-engine)
  ((unsupported :initarg :unsupported :initform '() :reader null-engine-unsupported))
  (:documentation "The reference engine: placeholder SVG, one group per element (file
header)."))

(defun make-null-engine (&key unsupported)
  "A NULL-ENGINE that refuses the classes named in UNSUPPORTED."
  (check-type unsupported list)
  (make-instance 'null-engine :unsupported (copy-list unsupported)))

(defmethod engine-name ((engine null-engine))
  "null")

(defmethod engine-render-pages ((engine null-engine) view layout &key page)
  (declare (ignore view layout page))
  (%en0-pending 'engine-render-pages/null-engine))
