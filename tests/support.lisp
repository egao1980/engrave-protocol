(in-package #:engrave-protocol/tests)

;;; Fixtures ------------------------------------------------------------------

(defparameter +fixture-names+
  '("sc0-carol-6-8" "sc0-chords-under-rests" "sc0-jumps" "sc0-key-change"
    "sc0-keyboard" "sc0-omr-result" "sc0-pickup" "sc0-repeats-voltas" "sc0-satb"
    "sc0-three-verses" "sc1-open-score")
  "The fixture scores, tests/fixtures/<name>.sexp (PROVENANCE.md).")

(defvar *fixture-scores* (make-hash-table :test 'equal))

(defun fixture-score (name)
  "The fixture score NAME, read once per image."
  (or (gethash name *fixture-scores*)
      (setf (gethash name *fixture-scores*)
            (with-open-file (in (asdf:system-relative-pathname
                                 "engrave-protocol/tests" (format nil "tests/fixtures/~A.sexp" name))
                                :external-format :utf-8)
              (with-standard-io-syntax
                (let ((*read-eval* nil)
                      (*package* (find-package '#:engrave-protocol/tests)))
                  (sp:sexp-to-score (read in))))))))

(defun long-score (n &key (staves 1))
  "A generated score: one part with STAVES staves and one voice per staff, N
measures of 4/4, a whole note in every measure. Ids minted in document order."
  (sp:sexp-to-score
   `(:score
     :metadata (:title "Long")
     :measures ,(loop for i below n
                      collect (if (zerop i) '(:measure :time "4/4" :key "C major") '(:measure)))
     :parts ((:part :name "Long"
              :staves ,(loop for s below staves
                             collect (if (zerop s) '(:staff) '(:staff :clef :bass)))
              :voices ,(loop for s from 1 to staves
                             collect `(:voice :staff ,s
                                       :measures ,(loop repeat n
                                                        collect `((:note :pitch ,(if (= s 1) "C5" "C3")
                                                                   :duration 1))))))))))

(defun empty-score ()
  "A score without measures, parts or elements."
  (sp:make-score))

(defun element-ids (score)
  (mapcar #'sp:element-id (sp:score-elements score)))

(defun measure-ids (score)
  (mapcar #'sp:element-id (fset:convert 'list (sp:score-measure-map score))))

(defun part-ids (score)
  (mapcar #'sp:element-id (fset:convert 'list (sp:score-parts score))))

(defun timed-event-p (element)
  (and (typep element 'sp:event)
       (not (and (typep element 'sp:sounding-event) (sp:event-grace element)))))

(defun timed-ids (score)
  (mapcar #'sp:element-id (remove-if-not #'timed-event-p (sp:score-elements score))))

;;; Rendering -------------------------------------------------------------------

(defun render (score &rest keys)
  "RENDER-SVG with a fresh NULL-ENGINE → (values pages id-map)."
  (apply #'render-svg (make-null-engine) score keys))

(defun encode-page (page)
  "PAGE's SVG document as text, compact."
  (xml:encode (svg-page-document page) :pretty nil))

(defun encode-pages (pages)
  (mapcar #'encode-page pages))

(defun map-keys (map)
  "The keys of the FSet map MAP, sorted with STRING<."
  (let ((keys '()))
    (fset:do-map (k v map) (declare (ignore v)) (push k keys))
    (sort keys #'string<)))

(defun lookup (map key)
  "(values value found-p) of KEY in the FSet map MAP."
  (fset:lookup map key))

(defun maps-equal-p (a b)
  "Two FSet maps of strings to lists, compared with EQUAL on keys and values."
  (and (equal (map-keys a) (map-keys b))
       (every (lambda (k) (equal (lookup a k) (lookup b k))) (map-keys a))))

;;; SVG trees ---------------------------------------------------------------------

(defun children-elements (element)
  (remove-if-not #'xml:xml-element-p (xml:xml-element-children element)))

(defun svg-elements (page)
  "Every xml-element of PAGE, the root first, in document order."
  (let ((out '()))
    (labels ((walk (e) (push e out) (mapc #'walk (children-elements e))))
      (walk (svg-page-root page)))
    (nreverse out)))

(defun attr (element name)
  (xml:xml-attr element name))

(defun parent-table (page)
  "An EQ hash table: element of PAGE → its parent element."
  (let ((table (make-hash-table :test 'eq)))
    (labels ((walk (e) (dolist (c (children-elements e)) (setf (gethash c table) e) (walk c))))
      (walk (svg-page-root page)))
    table))

(defun ancestor-with-class (table element class)
  "The nearest ancestor of ELEMENT whose class attribute is CLASS, or NIL."
  (loop for e = (gethash element table) then (gethash e table)
        while e
        when (equal (attr e "class") class) return e))

(defun groups-with-id (pages id)
  "Every element with id ID over PAGES, as (page-number . element)."
  (loop for p in pages
        append (loop for e in (svg-elements p)
                     when (equal (attr e "id") id) collect (cons (svg-page-number p) e))))

(defun all-svg-ids (pages)
  (loop for p in pages append (svg-page-ids p)))

;;; Conditions ----------------------------------------------------------------------

(declaim (notinline runtime))
(defun runtime (x)
  "X, hidden from the compiler: for arguments of the wrong type that a test passes
on purpose (a constant would be a compile-time WARNING)."
  x)

(defun signalled (thunk)
  "The condition THUNK signals (unwinding), or NIL."
  (handler-case (progn (funcall thunk) nil)
    (condition (c) c)))

(defun issue-pairs (issues)
  "ISSUES as (kind id) lists, in order."
  (mapcar (lambda (i) (list (engraving-issue-kind i) (engraving-issue-id i))) issues))

;;; Test engines and printers -------------------------------------------------------

(defclass offline-engine (null-engine) ()
  (:documentation "A NULL-ENGINE that reports itself unavailable."))

(defmethod engine-available-p ((engine offline-engine)) nil)

(defclass recording-engine (null-engine)
  ((calls :initform '() :accessor recording-engine-calls))
  (:documentation "A NULL-ENGINE that records each ENGINE-RENDER-PAGES call as
(view layout page), oldest first."))

(defmethod engine-render-pages :before ((engine recording-engine) view layout &key page)
  (setf (recording-engine-calls engine)
        (append (recording-engine-calls engine) (list (list view layout page)))))

(defclass plain-engine (engrave-engine) ()
  (:documentation "An engine with only the protocol's default methods."))

(defclass delegating-engine (engrave-engine) ()
  (:documentation "An engine of its own class whose ENGINE-RENDER-PAGES is the null
engine's: everything else comes from the protocol's methods."))

(defmethod engine-render-pages ((engine delegating-engine) view layout &key page)
  (engine-render-pages (make-null-engine) view layout :page page))

(defparameter +fake-pdf+
  (coerce (map 'list #'char-code (format nil "%PDF-1.7~%%fake~%")) '(vector (unsigned-byte 8))))

(defclass fake-printer ()
  ((calls :initform '() :accessor fake-printer-calls)
   (output :initarg :output :initform +fake-pdf+ :reader fake-printer-output))
  (:documentation "Records PRINT-PAGES calls as (pages layout) and returns OUTPUT."))

(defmethod print-pages ((printer fake-printer) pages layout)
  (setf (fake-printer-calls printer)
        (append (fake-printer-calls printer) (list (list pages layout))))
  (fake-printer-output printer))
