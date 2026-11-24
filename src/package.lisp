;;;; engrave-protocol: scores to engraved SVG pages, timemaps and PDF.
;;;; Brief: cl-stack/docs/capabilities/engrave.md. Scores are score-protocol
;;;; values; SVG pages are xml-protocol trees, never raw strings.

(defpackage #:engrave-protocol
  (:use #:cl)
  (:nicknames #:stack-engrave)
  (:import-from #:score-protocol #:skip-element)
  (:documentation "The engraving protocol (EN0): engines turn a score-protocol score, or
one of its views, into SVG pages with an element id map, a timemap for
playback cursors, and PDF through a printer. NULL-ENGINE is the reference
engine: deterministic placeholder SVG, no music layout. Real engines live in
backends (engrave-backend-verovio).")
  (:export
   ;; Conditions (see conditions.lisp). SKIP-ELEMENT is score-protocol's restart.
   #:engrave-error #:engrave-error-engine #:engrave-error-message
   #:engine-unavailable #:engine-unavailable-reason
   #:printer-unavailable
   #:unsupported-notation #:unsupported-notation-element-id #:unsupported-notation-kind
   #:engine-render-error #:engine-render-error-log
   #:invalid-render-option #:invalid-render-option-option #:invalid-render-option-value
   #:skip-element
   ;; Engines (see engine.lisp)
   #:engrave-engine #:*engrave-engine* #:ensure-engine
   #:engine-name #:engine-available-p #:engine-render-pages
   ;; The reference engine (see null-engine.lisp)
   #:null-engine #:make-null-engine #:null-engine-unsupported
   ;; Layout (see layout.lisp)
   #:layout #:layout-p #:make-layout #:ensure-layout
   #:layout-page-size #:layout-margins #:layout-scale #:layout-system-breaks
   #:+page-sizes+
   ;; Views (see view.lisp)
   #:+views+ #:engrave-view
   ;; SVG pages (see svg.lisp)
   #:+svg-namespace+ #:make-svg-element #:valid-svg-id-p
   #:+element-kinds+ #:element-kind
   #:svg-page #:svg-page-p #:make-svg-page
   #:svg-page-number #:svg-page-document #:svg-page-systems
   #:svg-page-root #:svg-page-ids #:find-svg-element
   ;; Rendering (see render.lisp)
   #:render-svg #:render-pdf #:*pdf-printer* #:print-pages
   ;; Timemaps (see timemap.lisp)
   #:render-timemap #:score-timemap #:+default-tempo+
   #:timemap-entry #:timemap-entry-p #:make-timemap-entry
   #:timemap-entry-onset #:timemap-entry-on #:timemap-entry-off #:timemap-entry-tempo
   ;; Conformance checks (see check.lisp)
   #:engraving-issue #:engraving-issue-p #:make-engraving-issue
   #:engraving-issue-kind #:engraving-issue-id #:engraving-issue-detail
   #:+engraving-issue-kinds+ #:+timemap-issue-kinds+
   #:check-engraving #:check-timemap))

(in-package #:engrave-protocol)

(defun %en0-pending (name)
  "Contract stub for slice EN0. Every call is replaced by the implementation,
then this function is deleted."
  (error "engrave-protocol EN0 pending: ~A" name))
