(in-package #:engrave-protocol/tests)

;;; conditions.lisp: the tree, slots, reports, the shared SKIP-ELEMENT.

(deftest condition-tree
  (ok (subtypep 'engrave-error 'error))
  (ok (subtypep 'engine-unavailable 'engrave-error))
  (ok (subtypep 'printer-unavailable 'engine-unavailable))
  (ok (subtypep 'unsupported-notation 'engrave-error))
  (ok (subtypep 'engine-render-error 'engrave-error))
  (ok (subtypep 'invalid-render-option 'engrave-error))
  (ng (subtypep 'unsupported-notation 'engine-unavailable))
  (ng (subtypep 'invalid-render-option 'engine-unavailable)))

(deftest condition-slots
  (let* ((engine (make-null-engine))
         (c1 (make-condition 'engine-unavailable :engine engine :reason "offline" :message "m"))
         (c2 (make-condition 'unsupported-notation :engine engine :element-id "br-n"
                                                   :kind 'sp:breath-mark))
         (c3 (make-condition 'engine-render-error :engine engine :log "loadData failed"))
         (c4 (make-condition 'invalid-render-option :option :scale :value 0))
         (c5 (make-condition 'printer-unavailable :engine engine :reason "no printer")))
    (ok (eq engine (engrave-error-engine c1)))
    (ok (equal "offline" (engine-unavailable-reason c1)))
    (ok (equal "m" (engrave-error-message c1)))
    (ok (equal "br-n" (unsupported-notation-element-id c2)))
    (ok (eq 'sp:breath-mark (unsupported-notation-kind c2)))
    (ok (equal "loadData failed" (engine-render-error-log c3)))
    (ok (eq :scale (invalid-render-option-option c4)))
    (ok (eql 0 (invalid-render-option-value c4)))
    (ok (null (engrave-error-engine c4)) "ENGINE defaults to NIL")
    (ok (null (engrave-error-message c4)) "MESSAGE defaults to NIL")
    (ok (equal "no printer" (engine-unavailable-reason c5)))))

(deftest condition-reports
  (let ((engine (make-null-engine)))
    (dolist (c (list (make-condition 'engrave-error :engine engine :message "boom")
                     (make-condition 'engine-unavailable :reason "offline")
                     (make-condition 'printer-unavailable :engine engine :reason "no printer")
                     (make-condition 'unsupported-notation :engine engine :element-id "br-n"
                                                           :kind 'sp:breath-mark)
                     (make-condition 'engine-render-error :engine engine :log "the log")
                     (make-condition 'invalid-render-option :option :page :value 0)))
      (let ((text (princ-to-string c)))
        (ok (and (stringp text) (plusp (length text))) (format nil "~S reports" (type-of c)))))
    (ok (search "br-n" (princ-to-string (make-condition 'unsupported-notation :engine engine
                                                                              :element-id "br-n")))
        "the element id is in the report")
    (ok (search "null" (princ-to-string (make-condition 'engine-render-error :engine engine)))
        "the engine's name is in the report")
    (ok (search "the log" (princ-to-string (make-condition 'engine-render-error :engine engine
                                                                                :log "the log"))))
    (ok (search "offline" (princ-to-string (make-condition 'engine-unavailable :reason "offline"))))))

(deftest skip-element-is-score-protocols
  (ok (eq 'skip-element 'sp:skip-element))
  (ok (eq (find-symbol "SKIP-ELEMENT" '#:engrave-protocol) 'sp:skip-element)))
