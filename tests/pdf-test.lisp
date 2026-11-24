(in-package #:engrave-protocol/tests)

;;; render.lisp: RENDER-PDF delegates to a printer (PRINT-PAGES).

(deftest pdf-through-a-printer
  (let* ((s (fixture-score "sc1-open-score"))
         (printer (make-instance 'fake-printer))
         (octets (render-pdf (make-null-engine) s :printer printer :layout '(:page-size :a5 :scale 300))))
    (ok (equalp +fake-pdf+ octets) "the printer's octets")
    (ok (typep octets '(vector (unsigned-byte 8))))
    (ok (= 1 (length (fake-printer-calls printer))) "one call")
    (destructuring-bind (pages layout) (first (fake-printer-calls printer))
      (ok (equal (encode-pages (render s :layout '(:page-size :a5 :scale 300))) (encode-pages pages))
          "every page of RENDER-SVG")
      (ok (layout-p layout) "a LAYOUT struct")
      (ok (equal '(148 210) (layout-page-size layout)))
      (ok (eql 300 (layout-scale layout))))))

(deftest pdf-view-and-parts
  (let* ((s (fixture-score "sc1-open-score"))
         (printer (make-instance 'fake-printer)))
    (render-pdf (make-null-engine) s :printer printer :view :condensed)
    (render-pdf (make-null-engine) s :printer printer :parts (list (second (part-ids s))))
    (destructuring-bind (first-call second-call) (fake-printer-calls printer)
      (ok (equal (encode-pages (render s :view :condensed)) (encode-pages (first first-call))))
      (ok (equal (encode-pages (render s :parts (list (second (part-ids s)))))
                 (encode-pages (first second-call))))
      (ok (equalp (make-layout) (second first-call)) "NIL layout: the default"))))

(deftest pdf-default-printer
  (let* ((s (fixture-score "sc0-satb"))
         (printer (make-instance 'fake-printer))
         (*pdf-printer* printer))
    (ok (equalp +fake-pdf+ (render-pdf (make-null-engine) s)))
    (ok (= 1 (length (fake-printer-calls printer))))
    (let ((other (make-instance 'fake-printer)))
      (render-pdf (make-null-engine) s :printer other)
      (ok (= 1 (length (fake-printer-calls other))) ":printer wins over *PDF-PRINTER*")
      (ok (= 1 (length (fake-printer-calls printer)))))))

(deftest pdf-no-printer
  (let* ((s (fixture-score "sc0-satb"))
         (*pdf-printer* nil)
         (engine (make-instance 'recording-engine))
         (c (signalled (lambda () (render-pdf engine s)))))
    (ok (typep c 'printer-unavailable))
    (ok (typep c 'engine-unavailable) "a kind of ENGINE-UNAVAILABLE")
    (ok (and (typep c 'printer-unavailable) (eq engine (engrave-error-engine c))))
    (ok (and (typep c 'printer-unavailable) (stringp (engine-unavailable-reason c))))
    (ok (null (recording-engine-calls engine)) "nothing is rendered without a printer")
    (let* ((printer (make-instance 'fake-printer))
           (seen 0)
           (restarts '())
           (octets (handler-bind ((printer-unavailable
                                    (lambda (c)
                                      (incf seen)
                                      (push (mapcar #'restart-name (compute-restarts c)) restarts)
                                      (use-value (if (= seen 1) nil printer) c))))
                     (render-pdf (make-null-engine) s))))
      (ok (equalp +fake-pdf+ octets) "USE-VALUE")
      (ok (= 2 seen) "NIL signals again")
      (ok (every (lambda (names) (member 'use-value names)) restarts)))))

(deftest pdf-engine-resolution
  (let ((s (fixture-score "sc0-satb"))
        (printer (make-instance 'fake-printer)))
    (let ((*engrave-engine* nil)
          (*pdf-printer* nil))
      (let ((c (signalled (lambda () (render-pdf nil s)))))
        (ok (and (typep c 'engine-unavailable) (not (typep c 'printer-unavailable)))
            "the engine is resolved before the printer")))
    (let ((*engrave-engine* (make-null-engine)))
      (ok (equalp +fake-pdf+ (render-pdf nil s :printer printer)) "*ENGRAVE-ENGINE*"))
    (ok (typep (signalled (lambda () (render-pdf (make-instance 'offline-engine) s :printer printer)))
               'engine-unavailable))
    (ok (equalp +fake-pdf+
                (handler-bind ((engine-unavailable (lambda (c) (use-value (make-null-engine) c))))
                  (render-pdf (make-instance 'offline-engine) s :printer printer)))
        "USE-VALUE")
    (ok (equalp +fake-pdf+ (render-pdf (make-instance 'delegating-engine) s :printer printer))
        "every engine inherits the PDF path")))

(defparameter +not-pdf+
  (list "%PDF-1.7"
        (coerce '(37 80 68 70) '(vector (unsigned-byte 8)))
        (coerce '(1 2 3 4 5 6) '(vector (unsigned-byte 8)))
        (vector 37 80 68 70 45 49)
        nil
        42)
  "Printer outputs that are not PDF octets: a string, too short, wrong magic,
a simple vector, NIL, a number.")

(deftest pdf-bad-printer-output
  (let ((s (fixture-score "sc0-satb"))
        (engine (make-null-engine)))
    (dolist (output +not-pdf+)
      (let ((c (signalled (lambda ()
                            (render-pdf engine s :printer (make-instance 'fake-printer :output output))))))
        (ok (typep c 'engine-render-error) (format nil "~S" output))
        (ok (and (typep c 'engine-render-error) (eq engine (engrave-error-engine c))))))
    (ok (equalp (coerce '(37 80 68 70 45) '(vector (unsigned-byte 8)))
                (render-pdf engine s :printer (make-instance 'fake-printer
                                                             :output (coerce '(37 80 68 70 45)
                                                                             '(vector (unsigned-byte 8))))))
        "five bytes are enough")))

(deftest pdf-refuses
  (let ((printer (make-instance 'fake-printer)))
    (ok (typep (signalled (lambda () (render-pdf (make-null-engine) 42 :printer printer))) 'type-error))
    (let ((c (signalled (lambda () (render-pdf (make-null-engine) (fixture-score "sc0-satb")
                                               :printer printer :view :piano)))))
      (ok (and (typep c 'invalid-render-option) (eq :view (invalid-render-option-option c)))))
    (ok (null (fake-printer-calls printer)) "nothing printed")))
