;;;; EN0 conditions and restarts (CLHS 9).
;;;;
;;;;   engrave-error (error)          ENGINE (the engine or NIL), MESSAGE (NIL or a string)
;;;;   ├── engine-unavailable         REASON (a string): no engine bound, or the engine
;;;;   │   │                          cannot run here (its native library is missing)
;;;;   │   │                          restart USE-VALUE: an engine to use instead
;;;;   │   └── printer-unavailable    RENDER-PDF found no printer (ENGINE is the engine)
;;;;   │                              restart USE-VALUE: a printer to use instead
;;;;   ├── unsupported-notation       ELEMENT-ID, KIND (the element's class name): the
;;;;   │                              engine cannot draw that element
;;;;   │                              restart SKIP-ELEMENT: draw the score without it
;;;;   ├── engine-render-error        LOG (NIL or the engine's own message): the engine
;;;;   │                              failed (an engine call returned failure)
;;;;   └── invalid-render-option      OPTION (a keyword), VALUE: a bad argument to a
;;;;                                  render function, MAKE-LAYOUT or ENGRAVE-VIEW
;;;;
;;;; The protocol owns these types; engines and printers signal them and define no
;;;; error tree of their own. Each is signalled with ERROR. The restarts are
;;;; established at the lowest frame that can recover: ENSURE-ENGINE (USE-VALUE,
;;;; engine.lisp), RENDER-PDF (USE-VALUE for the printer, render.lisp), and the
;;;; engine's own walk over the elements (SKIP-ELEMENT, null-engine.lisp).
;;;; SKIP-ELEMENT is the symbol score-protocol exports (its readers use it for
;;;; MUSICXML-UNSUPPORTED), so one handler serves both.
;;;;
;;;; Reports: every condition prints its type's phrase, then the engine's name
;;;; (ENGINE-NAME) when ENGINE is not NIL, then the slots that say what went
;;;; wrong (REASON, ELEMENT-ID and KIND, LOG, OPTION and VALUE), then MESSAGE when
;;;; it is not NIL. The exact wording is not part of the contract.

(in-package #:engrave-protocol)

(define-condition engrave-error (error)
  ((engine :initarg :engine :initform nil :reader engrave-error-engine)
   (message :initarg :message :initform nil :reader engrave-error-message))
  (:report (lambda (c s)
             (format s "Engraving failed~@[ (~A)~]~@[: ~A~]"
                     (%engine-label (engrave-error-engine c)) (engrave-error-message c))))
  (:documentation "The root of every engraving error."))

(define-condition engine-unavailable (engrave-error)
  ((reason :initarg :reason :initform nil :reader engine-unavailable-reason))
  (:report (lambda (c s)
             (format s "No engraving engine available~@[ (~A)~]~@[: ~A~]~@[ ~A~]"
                     (%engine-label (engrave-error-engine c)) (engine-unavailable-reason c)
                     (engrave-error-message c))))
  (:documentation "No engine is bound, or ENGINE cannot run (ENGINE-AVAILABLE-P is false).
Signalled inside a USE-VALUE restart that takes an engine."))

(define-condition printer-unavailable (engine-unavailable) ()
  (:report (lambda (c s)
             (format s "No PDF printer available~@[ (~A)~]~@[: ~A~]~@[ ~A~]"
                     (%engine-label (engrave-error-engine c)) (engine-unavailable-reason c)
                     (engrave-error-message c))))
  (:documentation "RENDER-PDF has no printer: bind *PDF-PRINTER* or pass :PRINTER.
Signalled inside a USE-VALUE restart that takes a printer."))

(define-condition unsupported-notation (engrave-error)
  ((element-id :initarg :element-id :reader unsupported-notation-element-id)
   (kind :initarg :kind :initform nil :reader unsupported-notation-kind))
  (:report (lambda (c s)
             (format s "Engine~@[ ~A~] cannot draw element ~S~@[ (~(~A~))~]~@[: ~A~]"
                     (%engine-label (engrave-error-engine c))
                     (unsupported-notation-element-id c) (unsupported-notation-kind c)
                     (engrave-error-message c))))
  (:documentation "The engine cannot draw one element (ELEMENT-ID, of class KIND).
Signalled inside a SKIP-ELEMENT restart: taking it renders without that element
(and without everything inside it)."))

(define-condition engine-render-error (engrave-error)
  ((log :initarg :log :initform nil :reader engine-render-error-log))
  (:report (lambda (c s)
             (format s "Engine~@[ ~A~] failed~@[: ~A~]~@[~%~A~]"
                     (%engine-label (engrave-error-engine c)) (engrave-error-message c)
                     (engine-render-error-log c))))
  (:documentation "The engine itself failed; LOG is its own message, when it gave one."))

(define-condition invalid-render-option (engrave-error)
  ((option :initarg :option :reader invalid-render-option-option)
   (value :initarg :value :initform nil :reader invalid-render-option-value))
  (:report (lambda (c s)
             (format s "Invalid ~S ~S~@[ (~A)~]~@[: ~A~]"
                     (invalid-render-option-option c) (invalid-render-option-value c)
                     (%engine-label (engrave-error-engine c)) (engrave-error-message c))))
  (:documentation "A bad argument: OPTION names it (:view, :parts, :page, :layout,
:page-size, :margins, :scale, :system-breaks), VALUE is what was given."))
