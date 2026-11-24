;;;; EN0 engines: the protocol class, the default engine, availability.
;;;;
;;;; An ENGINE is an instance of a subclass of ENGRAVE-ENGINE. Its options (font,
;;;; spacing, the engine's own settings) are slots of the instance, never
;;;; keywords of the render functions, so the protocol's keywords mean the same
;;;; for every engine. An engine implements ENGINE-RENDER-PAGES; RENDER-SVG,
;;;; RENDER-TIMEMAP and RENDER-PDF have methods for ENGRAVE-ENGINE that every
;;;; engine inherits (render.lisp, timemap.lisp), and an engine may specialize
;;;; them (an engine with its own PDF output specializes RENDER-PDF).
;;;;
;;;; *ENGRAVE-ENGINE*: the engine used when a render function gets NIL as its
;;;; engine. Default NIL. A backend never sets it when loaded.
;;;;
;;;; ENGINE-NAME engine → a string for reports and logs. The method for
;;;; ENGRAVE-ENGINE returns the class name in lower case; NULL-ENGINE's method
;;;; returns "null".
;;;;
;;;; ENGINE-AVAILABLE-P engine → true when ENGINE can render here. The method
;;;; for ENGRAVE-ENGINE returns T; an engine over a native library returns NIL
;;;; when the library did not load.
;;;;
;;;; ENSURE-ENGINE engine → an engine that can render:
;;;;   * ENGINE NIL stands for *ENGRAVE-ENGINE*.
;;;;   * An ENGRAVE-ENGINE for which ENGINE-AVAILABLE-P is true is returned.
;;;;   * NIL (no engine bound) or an engine that is not available: signals
;;;;     ENGINE-UNAVAILABLE (ENGINE the engine or NIL, REASON a string saying
;;;;     which case) with ERROR inside a USE-VALUE restart taking one argument;
;;;;     the restart returns (ensure-engine <its argument>), so its value is
;;;;     checked the same way (and NIL again means *ENGRAVE-ENGINE*).
;;;;   * Anything else: TYPE-ERROR (datum the object, expected type
;;;;     (or null engrave-engine)).
;;;;
;;;; ENGINE-RENDER-PAGES engine view layout &key page → (values pages id-map),
;;;; the one function an engine must implement for SVG. VIEW is the score to
;;;; draw (ENGRAVE-VIEW's result, never a raw option), LAYOUT a LAYOUT struct
;;;; whose SYSTEM-BREAKS, when a list, are measure ids of VIEW. PAGE is NIL or
;;;; the page number the caller wants: an engine may then render only that page,
;;;; and returns at least it when it exists. PAGES is a list of SVG-PAGEs in page
;;;; order (svg.lisp). ID-MAP is an FSet map whose keys are ids of elements of
;;;; VIEW and whose values are lists of (page-number . svg-id), the drawings of
;;;; that element over the whole rendering (also for pages not returned), in page
;;;; order and then document order; an element the engine did not draw (one
;;;; skipped through SKIP-ELEMENT, or inside one) maps to (). RENDER-SVG
;;;; (render.lisp) turns these view ids into the caller's ids. The first drawing
;;;; of an element carries the element's own id as its SVG id whenever the
;;;; engine draws it once as an object of its own; ids an engine makes up
;;;; must not be ids or slice ids of VIEW (svg.lisp).

(in-package #:engrave-protocol)

(defclass engrave-engine () ()
  (:documentation "The protocol class of engraving engines. Subclasses keep their options
in slots and implement ENGINE-RENDER-PAGES."))

(defvar *engrave-engine* nil
  "The engine used when a render function is given NIL as its engine.")

(defgeneric engine-name (engine)
  (:documentation "ENGINE's name, a string, for reports and logs."))

(defmethod engine-name ((engine engrave-engine))
  (string-downcase (symbol-name (class-name (class-of engine)))))

(defgeneric engine-available-p (engine)
  (:documentation "True when ENGINE can render here (its native library loaded)."))

(defmethod engine-available-p ((engine engrave-engine))
  t)

(defgeneric engine-render-pages (engine view layout &key page)
  (:documentation "Draw VIEW (a score) with LAYOUT (a LAYOUT struct) →
(values pages id-map): SVG-PAGEs in order, and an FSet map from VIEW's element
ids to lists of (page-number . svg-id). PAGE, when not NIL, is the one page
the caller wants. The file header of engine.lisp has the rules."))

(defun ensure-engine (engine)
  "ENGINE, or *ENGRAVE-ENGINE* for NIL, when it can render; else
ENGINE-UNAVAILABLE inside a USE-VALUE restart (file header)."
  (declare (ignore engine))
  (%en0-pending 'ensure-engine))

(defun %engine-label (engine)
  "ENGINE's name for a condition report, or NIL."
  (cond ((null engine) nil)
        ((typep engine 'engrave-engine) (engine-name engine))
        (t (princ-to-string engine))))
