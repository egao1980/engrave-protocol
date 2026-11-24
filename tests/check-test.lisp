(in-package #:engrave-protocol/tests)

;;; check.lisp: CHECK-ENGRAVING and CHECK-TIMEMAP catch each kind of bad
;;; output. Good outputs give () (id-map-test.lisp, timemap-test.lisp).

(defun kind-issues (kind issues)
  (remove-if-not (lambda (i) (eq kind (engraving-issue-kind i))) issues))

(defun repage (page &key (number (svg-page-number page)) (document (svg-page-document page))
                      (systems (svg-page-systems page)))
  (make-svg-page :number number :document document :systems systems))

(defun add-to-root (page element)
  "PAGE (fresh from a rendering) with ELEMENT appended to its root, in place."
  (let ((root (svg-page-root page)))
    (setf (xml:xml-element-children root) (append (xml:xml-element-children root) (list element)))
    page))

(defun satb-rendering ()
  (let ((s (fixture-score "sc0-satb")))
    (multiple-value-bind (pages map) (render s)
      (values s pages map))))

(defun long-rendering ()
  "Three pages."
  (let ((s (long-score 12)))
    (multiple-value-bind (pages map) (render s :layout '(:scale 400))
      (values s pages map))))

(defun check-long (pages map)
  (check-engraving (long-score 12) pages map))

(deftest check-engraving-kinds
  (ok (equal +engraving-issue-kinds+
             '(:page-number :not-svg :namespace :invalid-svg-id :duplicate-svg-id
               :missing-element :unknown-element :not-drawn :dangling-ref :systems))))

(deftest check-page-number
  (multiple-value-bind (s pages map) (long-rendering)
    (declare (ignore s))
    (ok (null (check-long pages map)))
    (let ((issues (check-long (list (first pages) (repage (second pages) :number 3) (third pages)) map)))
      (ok (equal '((:page-number nil)) (issue-pairs (kind-issues :page-number issues))))
      (ok (equal '(:index 1 :number 3) (engraving-issue-detail (first issues)))))
    (let ((issues (check-long (reverse pages) map)))
      (ok (equal '(:index 0 :number 3)
                 (engraving-issue-detail (first (kind-issues :page-number issues))))
          "one issue, for the first page out of place")
      (ok (= 1 (length (kind-issues :page-number issues)))))))

(deftest check-not-svg
  (multiple-value-bind (s pages map) (satb-rendering)
    (let ((p (first pages)))
      (dolist (doc (list (xml:make-xml-document :children (list (xml:make-xml-element "html")))
                         (xml:make-xml-document
                          :children (list (make-svg-element "svg" (list (cons "xmlns" +svg-namespace+)
                                                                        '("width" . "210mm") '("height" . "297mm")))))
                         (xml:make-xml-document
                          :children (list (xml:make-xml-element "svg" '(("viewBox" . "0 0 1 1")
                                                                        ("width" . "1mm") ("height" . "1mm")))))
                         "<svg xmlns=\"http://www.w3.org/2000/svg\"/>"
                         nil))
        (let ((issues (check-engraving s (list (repage p :document doc)) map)))
          (ok (equal '((:not-svg nil)) (issue-pairs (kind-issues :not-svg issues))) (format nil "~S" doc))
          (ok (equal '(:page 1) (engraving-issue-detail (first (kind-issues :not-svg issues)))))
          (ok (null (kind-issues :namespace issues)) "the page is not examined further"))))))

(deftest check-namespace
  (multiple-value-bind (s pages map) (satb-rendering)
    (add-to-root (first pages) (xml:make-xml-element "g" '(("id" . "plain"))))
    (add-to-root (first pages) (xml:make-xml-element "div"))
    (let ((issues (check-engraving s pages map)))
      (ok (equal '((:namespace "plain") (:namespace nil)) (issue-pairs issues)))
      (ok (equal '(:page 1 :element "g") (engraving-issue-detail (first issues))))
      (ok (equal '(:page 1 :element "div") (engraving-issue-detail (second issues)))))))

(deftest check-invalid-svg-id
  (multiple-value-bind (s pages map) (satb-rendering)
    (add-to-root (first pages) (make-svg-element "g" '(("id" . "1bad"))))
    (add-to-root (first pages) (make-svg-element "g" '(("id" . "a b"))))
    (let ((issues (check-engraving s pages map)))
      (ok (equal '((:invalid-svg-id "1bad") (:invalid-svg-id "a b")) (issue-pairs issues)))
      (ok (equal '(:page 1) (engraving-issue-detail (first issues)))))))

(deftest check-duplicate-svg-id
  (multiple-value-bind (s pages map) (satb-rendering)
    (add-to-root (first pages) (make-svg-element "g" '(("id" . "e-9"))))
    (add-to-root (first pages) (make-svg-element "g" '(("id" . "e-9"))))
    (ok (equal '((:duplicate-svg-id "e-9") (:duplicate-svg-id "e-9"))
               (issue-pairs (check-engraving s pages map)))
        "one issue per repeat"))
  (multiple-value-bind (s pages map) (long-rendering)
    (declare (ignore s))
    (add-to-root (third pages) (make-svg-element "g" '(("id" . "system.1"))))
    (let ((issues (check-long pages map)))
      (ok (equal '((:duplicate-svg-id "system.1")) (issue-pairs issues)) "across pages")
      (ok (equal '(:page 3) (engraving-issue-detail (first issues)))))))

(deftest check-id-map-keys
  (multiple-value-bind (s pages map) (satb-rendering)
    (ok (equal '((:missing-element "e-9") (:missing-element "ly-b"))
               (issue-pairs (check-engraving s pages (fset:less (fset:less map "ly-b") "e-9"))))
        "in the view's document order")
    (ok (equal '((:unknown-element "aa-1") (:unknown-element "zz-1"))
               (issue-pairs (check-engraving s pages (fset:with (fset:with map "zz-1" '((1 . "e-9")))
                                                                "aa-1" '()))))
        "sorted")
    (ok (equal '((:not-drawn "e-9") (:not-drawn "e-c"))
               (issue-pairs (check-engraving s pages (fset:with (fset:with map "e-c" '()) "e-9" '()))))
        "in the view's document order")))

(deftest check-dangling-ref
  (multiple-value-bind (s pages map) (satb-rendering)
    (let ((issues (check-engraving s pages (fset:with map "e-9" '((1 . "nope") (2 . "e-9") ("1" . "e-9")
                                                                   (1 . "e-9") (1 . "e-c") (1 . 42))))))
      (ok (equal '((:dangling-ref "e-9") (:dangling-ref "e-9") (:dangling-ref "e-9") (:dangling-ref "e-9"))
                 (issue-pairs issues)))
      (ok (equal '((:ref (1 . "nope")) (:ref (2 . "e-9")) (:ref ("1" . "e-9")) (:ref (1 . 42)))
                 (mapcar #'engraving-issue-detail issues))
          "a ref to another element's group is not dangling"))
    (ok (equal '((:dangling-ref "e-9"))
               (issue-pairs (check-engraving s pages (fset:with map "e-9" '(nope)))))
        "an item that is not a cons")))

(deftest check-systems
  (multiple-value-bind (s pages map) (satb-rendering)
    (let ((issues (check-engraving s (list (repage (first pages) :systems '(("m-1" "m-4")))) map)))
      (ok (equal '((:systems nil)) (issue-pairs issues)))
      (ok (equal '(:expected ("m-1" "m-3" "m-4") :actual ("m-1" "m-4"))
                 (engraving-issue-detail (first issues)))))
    (ok (equal '((:systems nil))
               (issue-pairs (check-engraving s (list (repage (first pages) :systems '(("m-1" "m-3") ("m-3" "m-4"))))
                                             map)))
        "a measure twice")
    (ok (equal '((:systems nil))
               (issue-pairs (check-engraving s (list (repage (first pages) :systems '(("m-3" "m-1" "m-4"))))
                                             map)))
        "out of order")
    (ok (null (check-engraving s (list (repage (first pages) :systems '(("m-1") ("m-3" "m-4")))) map))
        "any grouping into systems is fine")))

(deftest check-engraving-order
  (multiple-value-bind (s pages map) (satb-rendering)
    (add-to-root (first pages) (xml:make-xml-element "g" '(("id" . "plain"))))
    (let ((issues (check-engraving s (list (repage (first pages) :number 2 :systems '()))
                                   (fset:with (fset:less map "e-9") "zz" '()))))
      (ok (equal '(:page-number :namespace :missing-element :unknown-element :dangling-ref :systems)
                 (remove-duplicates (mapcar #'engraving-issue-kind issues) :from-end t))
          "issues in the order of the checks"))))

(deftest check-engraving-view
  (multiple-value-bind (s pages map) (satb-rendering)
    (ok (equal '((:unknown-element "p-5") (:unknown-element "st-6") (:unknown-element "st-7"))
               (issue-pairs (check-engraving s pages map :view :condensed)))
        "the condensed view does not carry the part and its staves"))
  (let ((s (fixture-score "sc1-open-score")))
    (multiple-value-bind (pages map) (render s :parts (list (first (part-ids s))))
      (ok (null (check-engraving s pages map :parts (list (first (part-ids s))))))
      (ok (kind-issues :unknown-element (check-engraving s pages map :parts (list (second (part-ids s))))))
      (ok (kind-issues :missing-element (check-engraving s pages map)) "the whole score expected")))
  (ok (typep (signalled (lambda () (check-engraving 42 '() (fset:empty-map)))) 'type-error))
  (ok (typep (signalled (lambda () (check-engraving (fixture-score "sc0-satb") '() (fset:empty-map)
                                                    :view :piano)))
             'invalid-render-option)))

;;; CHECK-TIMEMAP -----------------------------------------------------------------

(defun entry (onset on off &optional (tempo +default-tempo+))
  (make-timemap-entry :onset onset :on on :off off :tempo tempo))

(defun edit-entry (timemap index &key (on nil on-p) (off nil off-p) (onset nil onset-p))
  "TIMEMAP with entry INDEX replaced."
  (loop for e in timemap for i from 0
        collect (if (= i index)
                    (make-timemap-entry :onset (if onset-p onset (timemap-entry-onset e))
                                        :on (if on-p on (timemap-entry-on e))
                                        :off (if off-p off (timemap-entry-off e))
                                        :tempo (timemap-entry-tempo e))
                    e)))

(deftest check-timemap-kinds
  (ok (equal +timemap-issue-kinds+ '(:onset-order :unknown-event :event-count :off-before-on)))
  (ok (typep (signalled (lambda () (make-timemap-entry :onset (runtime -1)))) 'type-error))
  (ok (typep (signalled (lambda () (make-timemap-entry :onset 0 :on (runtime "e-1")))) 'type-error)))

(deftest check-onset-order
  (let* ((s (fixture-score "sc0-satb"))
         (tm (render-timemap (make-null-engine) s)))
    (ok (null (check-timemap s tm)))
    (let ((swapped (list* (first tm) (third tm) (second tm) (nthcdr 3 tm))))
      (ok (equal '((:entry 2)) (mapcar #'engraving-issue-detail
                                       (kind-issues :onset-order (check-timemap s swapped))))))
    (let ((same (edit-entry tm 1 :onset (timemap-entry-onset (first tm)))))
      (ok (equal '((:entry 1)) (mapcar #'engraving-issue-detail
                                       (kind-issues :onset-order (check-timemap s same))))
          "equal onsets"))))

(deftest check-unknown-event
  (let* ((s (fixture-score "sc0-keyboard"))
         (tm (render-timemap (make-null-engine) s))
         (grace (sp:element-id (find-if (lambda (e) (and (typep e 'sp:sounding-event) (sp:event-grace e)))
                                        (sp:score-elements s))))
         (note (sp:element-id (find-if (lambda (e) (typep e 'sp:chord-note)) (sp:score-elements s))))
         (bad (edit-entry tm 1 :on (append (timemap-entry-on (second tm)) (list "zz" grace))
                               :off (cons note (timemap-entry-off (second tm)))))
         (issues (check-timemap s bad)))
    (ok (equal (list (list :unknown-event "zz") (list :unknown-event grace) (list :unknown-event note))
               (issue-pairs issues))
        "ON before OFF; grace notes and chord notes are not timed")
    (ok (equal '(:entry 1) (engraving-issue-detail (first issues))))))

(deftest check-event-count
  (let* ((s (fixture-score "sc0-satb"))
         (tm (render-timemap (make-null-engine) s))
         (issues (check-timemap s (edit-entry tm 0 :on (remove "e-9" (timemap-entry-on (first tm))
                                                               :test #'equal)))))
    (ok (equal '((:event-count "e-9")) (issue-pairs issues)))
    (ok (equal '(:expected 1 :on 0 :off 1) (engraving-issue-detail (first issues)))))
  (let* ((s (fixture-score "sc0-repeats-voltas"))
         (folded (render-timemap (make-null-engine) s :unfold nil))
         (unfolded (render-timemap (make-null-engine) s)))
    (ok (equal '(:expected 2 :on 1 :off 1)
               (engraving-issue-detail (find "e-i" (check-timemap s folded) :key #'engraving-issue-id
                                                                            :test #'equal)))
        "a folded timemap checked as unfolded")
    (ok (equal '(:expected 1 :on 3 :off 3)
               (engraving-issue-detail (find "e-u" (check-timemap s unfolded :unfold nil)
                                             :key #'engraving-issue-id :test #'equal)))
        "an unfolded timemap checked as folded")
    (ok (equal (remove-duplicates (mapcar #'engraving-issue-kind (check-timemap s folded)))
               '(:event-count)))))

(deftest check-off-before-on
  (let* ((s (fixture-score "sc0-satb"))
         (tm (render-timemap (make-null-engine) s))
         (bad (edit-entry (edit-entry tm 1 :off (remove "e-9" (timemap-entry-off (second tm)) :test #'equal))
                          0 :off (list "e-9")))
         (issues (check-timemap s bad)))
    (ok (equal '((:off-before-on "e-9")) (issue-pairs issues)))
    (ok (equal '(:on 0 :off 0) (engraving-issue-detail (first issues)))))
  (ok (typep (signalled (lambda () (check-timemap 42 '()))) 'type-error))
  (ok (equal '((:event-count "e-f"))
             (issue-pairs (remove-if-not (lambda (i) (equal "e-f" (engraving-issue-id i)))
                                         (check-timemap (fixture-score "sc0-key-change")
                                                        (list (entry 0 '() '()))))))
      "an empty timemap misses every event"))
