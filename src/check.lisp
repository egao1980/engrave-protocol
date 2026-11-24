;;;; EN0 conformance checks: what every engine's output must satisfy.
;;;;
;;;; Engines are tested with these, not with golden SVG: a backend's suite runs
;;;; CHECK-ENGRAVING and CHECK-TIMEMAP over its renderings, as this suite does for
;;;; NULL-ENGINE. Both return a list of ENGRAVING-ISSUEs (read-only struct:
;;;; KIND, a keyword; ID, a string or NIL; DETAIL, a plist), () when the output
;;;; conforms. Issues come in the order of the checks below and, within one
;;;; check, in the order given there.
;;;;
;;;; CHECK-ENGRAVING score pages id-map &key (view :choral) parts
;;;;   PAGES and ID-MAP are what RENDER-SVG returns for SCORE, VIEW and PARTS
;;;;   without :PAGE: a whole rendering. EXPECTED is the list of source ids of
;;;;   the elements of (engrave-view score :view view :parts parts) that its view
;;;;   map sends to SCORE (elements the view made are not expected), in the
;;;;   view's document order. "Page k" is the page whose NUMBER is k.
;;;;    1 :PAGE-NUMBER    the pages are not numbered 1, 2, ..., n in list order:
;;;;                      one issue, for the first page out of place (ID NIL,
;;;;                      DETAIL (:index i :number k), i from 0).
;;;;    2 :NOT-SVG        a page whose DOCUMENT is not an XML-DOCUMENT with a root
;;;;                      element "svg" in +SVG-NAMESPACE+ carrying viewBox, width
;;;;                      and height (ID NIL, DETAIL (:page k)), in list order.
;;;;                      Checks 3 to 5 skip such a page.
;;;;    3 :NAMESPACE      an element below the root that is not in +SVG-NAMESPACE+
;;;;                      (ID its id attribute or NIL, DETAIL (:page k :element
;;;;                      local-name)), page order then document order.
;;;;    4 :INVALID-SVG-ID an id attribute value VALID-SVG-ID-P refuses (ID the
;;;;                      value, DETAIL (:page k)), page then document order.
;;;;    5 :DUPLICATE-SVG-ID an id value already seen earlier in page then
;;;;                      document order (ID the value, DETAIL (:page k)): one
;;;;                      issue per repeat.
;;;;    6 :MISSING-ELEMENT an EXPECTED id that is not a key of ID-MAP (ID the id,
;;;;                      DETAIL ()), in EXPECTED order.
;;;;    7 :UNKNOWN-ELEMENT a key of ID-MAP not in EXPECTED (ID the key, DETAIL ()),
;;;;                      keys sorted with STRING<.
;;;;    8 :NOT-DRAWN      an EXPECTED id whose value in ID-MAP is () (ID the id,
;;;;                      DETAIL ()), in EXPECTED order.
;;;;    9 :DANGLING-REF   an item of an EXPECTED id's value that is not a cons of
;;;;                      a page number of PAGES and an id SVG-PAGE-IDS of that
;;;;                      page contains (ID the element id, DETAIL (:ref item)),
;;;;                      in EXPECTED order, then value order.
;;;;   10 :SYSTEMS        the SYSTEMS of the pages, concatenated in list order,
;;;;                      are not the measure ids of the view's measure map, each
;;;;                      in its own place (ID NIL, DETAIL (:expected ids :actual
;;;;                      ids), both flat lists of measure ids): at most one issue.
;;;;   SCORE not a score → TYPE-ERROR; bad VIEW or PARTS → ENGRAVE-VIEW's
;;;;   INVALID-RENDER-OPTION.
;;;;
;;;; CHECK-TIMEMAP score timemap &key (unfold t)
;;;;   TIMEMAP is what RENDER-TIMEMAP returns for SCORE and UNFOLD. TIMED is the
;;;;   list of SCORE's timed events (timemap.lisp), in document order; "entry i"
;;;;   is the i-th entry, from 0.
;;;;    1 :ONSET-ORDER    an entry whose ONSET is not greater than the ONSET of
;;;;                      the entry before it (ID NIL, DETAIL (:entry i)), in
;;;;                      entry order. (MAKE-TIMEMAP-ENTRY refuses an ONSET that
;;;;                      is not a real >= 0.)
;;;;    2 :UNKNOWN-EVENT  an id in an entry's ON or OFF that is not the id of a
;;;;                      TIMED event (ID the id, DETAIL (:entry i)), in entry
;;;;                      order, ON before OFF, list order.
;;;;    3 :EVENT-COUNT    a TIMED event that does not occur exactly E times in ON
;;;;                      and E times in OFF over all entries, where E is the
;;;;                      number of events of (score-protocol:unfold score) that
;;;;                      its id map sends to the event when UNFOLD is true, and 1
;;;;                      when it is false (ID the event id, DETAIL (:expected E
;;;;                      :on n :off m)), in TIMED order.
;;;;    4 :OFF-BEFORE-ON  a TIMED event whose k-th occurrence in OFF is in an
;;;;                      entry at or before its k-th occurrence in ON, for some k
;;;;                      up to the lesser count (ID the event id, DETAIL (:on i
;;;;                      :off j), the entries of the first such k), in TIMED order.
;;;;   SCORE not a score → TYPE-ERROR.

(in-package #:engrave-protocol)

(defparameter +engraving-issue-kinds+
  '(:page-number :not-svg :namespace :invalid-svg-id :duplicate-svg-id
    :missing-element :unknown-element :not-drawn :dangling-ref :systems)
  "CHECK-ENGRAVING issue kinds, in check order. Data.")

(defparameter +timemap-issue-kinds+
  '(:onset-order :unknown-event :event-count :off-before-on)
  "CHECK-TIMEMAP issue kinds, in check order. Data.")

(defstruct (engraving-issue (:constructor make-engraving-issue (&key kind id detail)))
  "One conformance problem (file header)."
  (kind nil :type (or null keyword) :read-only t)
  (id nil :type (or null string) :read-only t)
  (detail '() :type list :read-only t))

(defun check-engraving (score pages id-map &key (view :choral) parts)
  "Conformance issues of a whole rendering of SCORE (file header)."
  (declare (ignore score pages id-map view parts))
  (%en0-pending 'check-engraving))

(defun check-timemap (score timemap &key (unfold t))
  "Conformance issues of a timemap of SCORE (file header)."
  (declare (ignore score timemap unfold))
  (%en0-pending 'check-timemap))
