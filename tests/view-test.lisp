(in-package #:engrave-protocol/tests)

;;; view.lisp: ENGRAVE-VIEW.

(defun identity-map-p (map ids)
  "MAP sends exactly the strings IDS each to itself."
  (and (equal (sort (copy-list ids) #'string<) (map-keys map))
       (every (lambda (id) (equal id (lookup map id))) ids)))

(deftest view-choral-is-the-score
  (dolist (name +fixture-names+)
    (let ((s (fixture-score name)))
      (multiple-value-bind (v map) (engrave-view s)
        (ok (eq s v) (format nil "~A: the score itself" name))
        (ok (identity-map-p map (element-ids s)) (format nil "~A: every id to itself" name))))))

(deftest view-defaults
  (let ((s (fixture-score "sc0-satb")))
    (ok (eq s (engrave-view s :view :choral)))
    (ok (eq s (engrave-view s :view :choral :parts nil)))
    (ok (equal +views+ '(:choral :condensed)))))

(defun open-score-with-cross-slur ()
  "sc1-open-score with one more slur, from a soprano event to an alto event."
  (let* ((s (fixture-score "sc1-open-score"))
         (events (lambda (part-index)
                   (remove-if-not (lambda (e)
                                    (and (typep e 'sp:event)
                                         (eq (sp:element-parent
                                              s (sp:element-id (sp:element-parent s (sp:element-id e))))
                                             (nth part-index (fset:convert 'list (sp:score-parts s))))))
                                  (sp:score-elements s))))
         (slur (sp:make-spanner :id "sp-cross" :kind :slur
                                :start (sp:element-id (first (funcall events 0)))
                                :end (sp:element-id (second (funcall events 1))))))
    (sp:copy-score s :spanners (append (fset:convert 'list (sp:score-spanners s)) (list slur)))))

(deftest view-choral-parts
  (let* ((s (open-score-with-cross-slur))
         (parts (part-ids s)))
    (ok (equal 4 (length parts)))
    (multiple-value-bind (v map) (engrave-view s :parts (list (first parts)))
      (ok (equal (list (first parts)) (part-ids v)) "only the soprano")
      (ok (equal (measure-ids s) (measure-ids v)) "the same measures")
      (ok (null (fset:convert 'list (sp:score-spanners v)))
          "the alto slur and the cross slur are dropped")
      (ok (equal (fset:convert 'list (sp:score-chord-symbols s))
                 (fset:convert 'list (sp:score-chord-symbols v)))
          "every anchor kept")
      (ok (equal (sp:score-metadata s) (sp:score-metadata v)))
      (ok (eql (sp:score-id-counter s) (sp:score-id-counter v)))
      (ok (identity-map-p map (element-ids v))))
    (multiple-value-bind (v map) (engrave-view s :parts (list (second parts) (first parts)))
      (ok (equal (list (first parts) (second parts)) (part-ids v)) "in the score's order")
      (ok (equal '("sp-1i" "sp-cross")
                 (mapcar #'sp:element-id (fset:convert 'list (sp:score-spanners v))))
          "spanners inside the kept parts stay")
      (ok (identity-map-p map (element-ids v))))
    (multiple-value-bind (v map) (engrave-view s :parts parts)
      (ok (equal (element-ids s) (element-ids v)) "all parts: the same elements")
      (ok (identity-map-p map (element-ids s))))))

(deftest view-condensed
  (let ((s (fixture-score "sc1-open-score")))
    (multiple-value-bind (v map) (engrave-view s :view :condensed)
      (multiple-value-bind (cv cmap) (sp:condensed-view s)
        (ok (equal (element-ids cv) (element-ids v)) "CONDENSED-VIEW's score")
        (ok (maps-equal-p cmap map) "and its id map"))
      (ok (= 1 (length (part-ids v))))
      (ok (= 2 (length (sp:part-staves (fset:first (sp:score-parts v))))))
      (ok (not (member (first (part-ids v)) (element-ids s) :test #'string=))
          "a new part")
      (ok (null (nth-value 1 (lookup map (first (part-ids v))))) "not in the id map")))
  (let ((s (fixture-score "sc0-satb")))
    (multiple-value-bind (v map) (engrave-view s :view :condensed)
      (multiple-value-bind (cv cmap) (sp:condensed-view s)
        (ok (equal (element-ids cv) (element-ids v)))
        (ok (maps-equal-p cmap map))))))

(deftest view-condensed-parts
  (let* ((s (fixture-score "sc1-open-score"))
         (parts (part-ids s)))
    (multiple-value-bind (v map) (engrave-view s :view :condensed :parts (subseq parts 0 2))
      (ok (= 1 (length (sp:part-staves (fset:first (sp:score-parts v)))))
          "two voices: one staff")
      (ok (= 2 (length (sp:part-voices (fset:first (sp:score-parts v))))))
      (let ((restricted (engrave-view s :parts (subseq parts 0 2))))
        (multiple-value-bind (cv cmap) (sp:condensed-view restricted)
          (ok (equal (element-ids cv) (element-ids v)) "condensed after restricting")
          (ok (maps-equal-p cmap map)))))))

(defparameter +bad-views+
  '((:view :piano :view)
    (:view nil :view)
    (:view "choral" :view)
    (:parts "p-4" :parts)
    (:parts ("nope") :parts)
    (:parts ("p-4" "p-4") :parts)
    (:parts ("p-4" nil) :parts)
    (:parts (:p-4) :parts)
    (:parts ("p-4" . "p-k") :parts)
    (:parts ("v-5") :parts))
  "(keyword value option) for ENGRAVE-VIEW on sc1-open-score; v-5 is a voice.")

(deftest-parametrize view-refuses
    ((key value option) :rows +bad-views+)
  (let ((c (signalled (lambda () (engrave-view (fixture-score "sc1-open-score") key value)))))
    (ok (typep c 'invalid-render-option) (format nil "~S ~S" key value))
    (ok (and (typep c 'invalid-render-option) (eq option (invalid-render-option-option c))))
    (ok (and (typep c 'invalid-render-option) (equal value (invalid-render-option-value c))))))

(deftest view-checks-in-order
  (ok (typep (signalled (lambda () (engrave-view 42))) 'type-error))
  (ok (typep (signalled (lambda () (engrave-view 42 :view :piano))) 'type-error) "SCORE first")
  (let ((c (signalled (lambda () (engrave-view (fixture-score "sc0-satb") :view :piano :parts '("x"))))))
    (ok (and (typep c 'invalid-render-option) (eq :view (invalid-render-option-option c)))
        "VIEW before PARTS")))
