;;;; EN0 rendering: RENDER-SVG and RENDER-PDF.
;;;;
;;;; RENDER-SVG engine score &key (view :choral) page layout parts
;;;;   → (values pages id-map)
;;;;   PAGES   a list of SVG-PAGEs (svg.lisp) in page order: every page, or with
;;;;           PAGE the one page numbered PAGE.
;;;;   ID-MAP  an FSet map from SCORE's element ids to lists of
;;;;           (page-number . svg-id): where each element is drawn, over the
;;;;           whole rendering even when PAGE selects one page. An element that
;;;;           was not drawn maps to (). Its keys are the elements of the view
;;;;           that come from SCORE (ENGRAVE-VIEW's map), as the engine reports
;;;;           them; CHECK-ENGRAVING (check.lisp) says whether that is complete.
;;;; Methods:
;;;;   (ENGINE NULL): (render-svg (ensure-engine nil) score ...) with the same
;;;;     keyword arguments.
;;;;   (ENGINE ENGRAVE-ENGINE), in this order:
;;;;     1. E = (ensure-engine engine). When E is not ENGINE (the caller took
;;;;        USE-VALUE), the result is (render-svg E score ...) with the same
;;;;        arguments.
;;;;     2. SCORE not a SCORE-PROTOCOL:SCORE → TYPE-ERROR.
;;;;     3. L = (ensure-layout layout).
;;;;     4. (engrave-view score :view view :parts parts) → V and VIEW-MAP.
;;;;     5. When (layout-system-breaks L) is a list, each id in it must be the id
;;;;        of a measure of V, else INVALID-RENDER-OPTION (OPTION :system-breaks,
;;;;        VALUE the list, ENGINE the engine).
;;;;     6. PAGE must be NIL or an integer >= 1, else INVALID-RENDER-OPTION
;;;;        (OPTION :page, VALUE PAGE, ENGINE the engine).
;;;;     7. (engine-render-pages engine V L :page page) → P and M.
;;;;     8. PAGE given and no page of P numbered PAGE → INVALID-RENDER-OPTION
;;;;        (OPTION :page, VALUE PAGE, ENGINE the engine).
;;;;     9. PAGES is P, or the list of P's page numbered PAGE. ID-MAP is M with
;;;;        each key K replaced by K's source id (lookup VIEW-MAP K); the keys
;;;;        VIEW-MAP lacks (a part or staff the view made) are dropped, and the
;;;;        values are kept as they are. The pages' systems hold measure ids,
;;;;        which every view shares with SCORE.
;;;;   Conditions the engine signals (UNSUPPORTED-NOTATION, ENGINE-RENDER-ERROR)
;;;;   pass through unchanged.
;;;;
;;;; RENDER-PDF engine score &key (view :choral) layout parts printer → octets
;;;;   A PDF: an (unsigned-byte 8) vector. The pages are SVG pages printed by a
;;;;   PRINTER, an object with a PRINT-PAGES method.
;;;; Methods:
;;;;   (ENGINE NULL): (render-pdf (ensure-engine nil) score ...) with the same
;;;;     keyword arguments.
;;;;   (ENGINE ENGRAVE-ENGINE), in this order:
;;;;     1. As RENDER-SVG's step 1, with RENDER-PDF.
;;;;     2. P = PRINTER, or *PDF-PRINTER* when PRINTER is NIL. When P is NIL:
;;;;        PRINTER-UNAVAILABLE (ENGINE the engine, REASON a string) with ERROR
;;;;        inside a USE-VALUE restart taking one argument; its argument becomes
;;;;        P, and NIL signals again. So nothing is rendered without a printer.
;;;;     3. L = (ensure-layout layout); PAGES = the pages of
;;;;        (render-svg engine score :view view :layout L :parts parts).
;;;;     4. OCTETS = (print-pages P PAGES L).
;;;;     5. OCTETS must be a (vector (unsigned-byte 8)) whose first five bytes are
;;;;        "%PDF-" (37 80 68 70 45); otherwise ENGINE-RENDER-ERROR (ENGINE the
;;;;        engine, MESSAGE saying the printer returned no PDF).
;;;;     6. The result is OCTETS.
;;;;
;;;; PRINT-PAGES printer pages layout → octets: the printer protocol. EN0 ships
;;;; no printer. The browser printer (EN2) puts the pages in one HTML document
;;;; through canvas-protocol's SVG-FRAGMENT (so NEUTRALIZE and the CSP scan
;;;; apply), sets the page box from LAYOUT (@page size and margins), and prints
;;;; it with browser-protocol's PRINT-PDF. An engine with a PDF output of its
;;;; own (a LilyPond engine) specializes RENDER-PDF instead.

(in-package #:engrave-protocol)

(defvar *pdf-printer* nil
  "The printer RENDER-PDF uses when it is given no :PRINTER.")

(defgeneric print-pages (printer pages layout)
  (:documentation "Print PAGES (SVG-PAGEs) at LAYOUT's page size → PDF octets."))

(defgeneric render-svg (engine score &key view page layout parts)
  (:documentation "Engrave SCORE → (values pages id-map). The file header of render.lisp
has the rules."))

(defgeneric render-pdf (engine score &key view layout parts printer)
  (:documentation "Engrave SCORE and print it → PDF octets. The file header of
render.lisp has the rules."))

(defmethod render-svg ((engine null) score &key (view :choral) page layout parts)
  (declare (ignore score view page layout parts))
  (%en0-pending 'render-svg/null))

(defmethod render-svg ((engine engrave-engine) score &key (view :choral) page layout parts)
  (declare (ignore score view page layout parts))
  (%en0-pending 'render-svg/engrave-engine))

(defmethod render-pdf ((engine null) score &key (view :choral) layout parts printer)
  (declare (ignore score view layout parts printer))
  (%en0-pending 'render-pdf/null))

(defmethod render-pdf ((engine engrave-engine) score &key (view :choral) layout parts printer)
  (declare (ignore score view layout parts printer))
  (%en0-pending 'render-pdf/engrave-engine))
