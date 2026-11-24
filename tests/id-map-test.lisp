(in-package #:engrave-protocol/tests)

;;; The id map and SVG ids over every fixture and view: every element id of the
;;; score is in the id map, SVG ids are valid and unique, and CHECK-ENGRAVING
;;; finds nothing.

(defun renderings ()
  "(label score keys) for every fixture in both views, the multi-part fixtures
restricted to their first part, and multi-page generated scores."
  (append
   (loop for name in +fixture-names+
         append (list (list (format nil "~A choral" name) (fixture-score name) '())
                      (list (format nil "~A condensed" name) (fixture-score name) '(:view :condensed))))
   (loop for name in '("sc0-keyboard" "sc1-open-score")
         for s = (fixture-score name)
         collect (list (format nil "~A first part" name) s (list :parts (list (first (part-ids s))))))
   (list (list "long 40 x 2 at 200" (long-score 40 :staves 2) '(:layout (:scale 200)))
         (list "long 25 at 400 on A5" (long-score 25) '(:layout (:scale 400 :page-size :a5)))
         (list "sc0-satb at 1000" (fixture-score "sc0-satb") '(:layout (:scale 1000))))))

(deftest-parametrize id-map-complete
    ((label score keys) :rows (renderings))
  (multiple-value-bind (pages map) (apply #'render score keys)
    (multiple-value-bind (v view-map) (engrave-view score :view (getf keys :view :choral)
                                                          :parts (getf keys :parts))
      (let ((missing '()) (bad '()))
        (dolist (e (sp:score-elements v))
          (multiple-value-bind (src found) (lookup view-map (sp:element-id e))
            (when found
              (multiple-value-bind (refs present) (lookup map src)
                (cond ((not present) (push src missing))
                      ((not (and (consp refs)
                                 (every (lambda (r)
                                          (and (consp r) (integerp (car r)) (stringp (cdr r))
                                               (let ((page (find (car r) pages :key #'svg-page-number)))
                                                 (and page (find-svg-element page (cdr r))))))
                                        refs)))
                       (push src bad)))))))
        (ok (null missing) (format nil "~A: every element is a key, missing ~S" label missing))
        (ok (null bad) (format nil "~A: every ref names an element on its page, bad ~S" label bad))))))

(deftest-parametrize id-map-choral-keys
    ((name) :rows +fixture-names+)
  (let ((s (fixture-score name)))
    (multiple-value-bind (pages map) (render s)
      (declare (ignore pages))
      (ok (equal (sort (element-ids s) #'string<) (map-keys map))
          (format nil "~A: the keys are exactly the score's ids" name))
      (ok (every (lambda (id) (equal (list (cons 1 id)) (lookup map id)))
                 (element-ids s))
          (format nil "~A: one page, each element drawn under its own id" name)))))

(deftest-parametrize svg-ids-valid-and-unique
    ((label score keys) :rows (renderings))
  (let* ((pages (apply #'render score keys))
         (ids (all-svg-ids pages)))
    (ok (every #'valid-svg-id-p ids) (format nil "~A: valid" label))
    (ok (= (length ids) (length (remove-duplicates ids :test #'string=)))
        (format nil "~A: unique over ~D pages" label (length pages)))
    (ok (notany (lambda (id) (and (search "." id) (not (eql 0 (search "system." id)))))
                ids)
        (format nil "~A: the null engine's own ids are system.<n>" label))))

(deftest-parametrize check-engraving-clean
    ((label score keys) :rows (renderings))
  (multiple-value-bind (pages map) (apply #'render score keys)
    (let ((issues (check-engraving score pages map :view (getf keys :view :choral)
                                                   :parts (getf keys :parts))))
      (ok (null issues) (format nil "~A: ~S" label (issue-pairs issues))))))

(deftest valid-svg-id-p-cases
  (dolist (id '("a" "e-1k" "m-3.v-8" "system.12" "_x" "E003-l19nj5e9" "A_b.c-d" "Z9"))
    (ok (valid-svg-id-p id) id))
  (dolist (id '("" "1a" "-a" ".a" "a b" "a:b" "a#b" "é" "a/b" nil 42 :a))
    (ng (valid-svg-id-p id) (format nil "~S" id))))

(deftest svg-page-accessors
  (let* ((pages (render (fixture-score "sc0-satb")))
         (page (first pages)))
    (ok (eq (svg-page-root page) (xml:document-root-element (svg-page-document page))))
    (ok (equal (loop for e in (svg-elements page) when (attr e "id") collect (attr e "id"))
               (svg-page-ids page))
        "document order")
    (ok (equal "e-9" (attr (find-svg-element page "e-9") "id")))
    (ok (equal "system" (attr (find-svg-element page "system.1") "class")))
    (ok (null (find-svg-element page "nope")))
    (ok (equal '(("m-1" "m-3" "m-4")) (svg-page-systems page))))
  (let* ((g (make-svg-element "g" '(("id" . "a"))))
         (h (make-svg-element "g" '(("id" . "a"))))
         (root (make-svg-element "svg" (list (cons "xmlns" +svg-namespace+) '("viewBox" . "0 0 1 1")
                                             '("width" . "1mm") '("height" . "1mm"))
                                 g h))
         (page (make-svg-page :number 1 :document (xml:make-xml-document :children (list root)))))
    (ok (equal '("a" "a") (svg-page-ids page)) "duplicates kept")
    (ok (eq g (find-svg-element page "a")) "the first in document order"))
  (ok (typep (signalled (lambda () (make-svg-page :number (runtime 0)))) 'type-error)))

(deftest make-svg-element-namespace
  (let ((e (make-svg-element "rect" '(("width" . "1")))))
    (ok (equal +svg-namespace+ (xml:xml-element-namespace-uri e)))
    (ok (equal "rect" (xml:xml-element-local-name e)))
    (ok (equal "1" (attr e "width"))))
  (ok (signalled (lambda () (make-svg-element "svg:rect")))))

(deftest element-kinds
  (let ((s (fixture-score "sc0-keyboard")))
    (dolist (e (sp:score-elements s))
      (ok (member (element-kind e) (mapcar #'cdr +element-kinds+)) (sp:element-id e))))
  (ok (null (element-kind 42)))
  (ok (null (element-kind (fixture-score "sc0-satb"))) "a score is not an element"))
