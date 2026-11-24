(in-package #:engrave-protocol/tests)

;;; null-engine.lisp: the page root, the groups, page splitting, system breaks,
;;; skipping, determinism.

(deftest null-page-root
  (let* ((page (first (render (fixture-score "sc0-satb"))))
         (root (svg-page-root page)))
    (ok (xml:xml-document-p (svg-page-document page)))
    (ok (equal "svg" (xml:xml-element-local-name root)))
    (ok (equal +svg-namespace+ (xml:xml-element-namespace-uri root)))
    (ok (equal "0 0 2100 2970" (attr root "viewBox")))
    (ok (equal "210mm" (attr root "width")))
    (ok (equal "297mm" (attr root "height")))
    (ok (equal "engrave-page" (attr root "class")))
    (ok (equal "1" (attr root "data-page")))
    (ok (search "xmlns=\"http://www.w3.org/2000/svg\"" (encode-page page))
        "a standalone SVG document"))
  (let ((root (svg-page-root (first (render (fixture-score "sc0-satb") :layout '(:page-size :letter))))))
    (ok (equal "0 0 2159 2794" (attr root "viewBox")))
    (ok (equal "215.9mm" (attr root "width")))
    (ok (equal "279.4mm" (attr root "height")))))

(deftest null-every-element-in-the-svg-namespace
  (dolist (page (render (long-score 12 :staves 2) :layout '(:scale 300)))
    (ok (every (lambda (e) (equal +svg-namespace+ (xml:xml-element-namespace-uri e)))
               (svg-elements page)))))

(deftest-parametrize null-one-group-per-element
    ((name) :rows +fixture-names+)
  (let* ((s (fixture-score name))
         (pages (render s)))
    (dolist (e (sp:score-elements s))
      (let ((found (groups-with-id pages (sp:element-id e))))
        (ok (= 1 (length found)) (format nil "~A: one element with id ~A" name (sp:element-id e)))
        (ok (and found
                 (equal "g" (xml:xml-element-local-name (cdr (first found))))
                 (equal (string-downcase (element-kind e)) (attr (cdr (first found)) "class")))
            (format nil "~A: <g class=~(~A~)>" name (element-kind e)))))))

(deftest-parametrize null-nesting
    ((name) :rows +fixture-names+)
  (let* ((s (fixture-score name))
         (pages (render s))
         (bad '()))
    (dolist (page pages)
      (let ((parents (parent-table page)))
        (dolist (g (svg-elements page))
          (let* ((id (attr g "id"))
                 (e (and id (sp:find-element s id :errorp nil))))
            (when e
              (let ((parent (gethash g parents)))
                (flet ((check (ok-p) (unless ok-p (push id bad))))
                  (typecase e
                    ((or sp:part)
                     (check (equal "parts" (attr parent "class"))))
                    ((or sp:staff sp:voice)
                     (check (equal (sp:element-id (sp:element-parent s id)) (attr parent "id"))))
                    (sp:measure-info
                     (check (equal "system" (attr parent "class"))))
                    (sp:event
                     (let ((bar (car (sp:element-position s id))))
                       (check (equal (sp:element-id (sp:bar-measure s bar)) (attr parent "id")))))
                    (sp:chord-symbol-anchor
                     (check (equal (sp:element-id (sp:bar-measure s (car (sp:anchor-position e))))
                                   (attr parent "id"))))
                    (sp:spanner
                     (let* ((start (sp:spanner-start e))
                            (measure (sp:element-id (sp:bar-measure s (car (sp:element-position s start))))))
                       (check (and (equal "system" (attr parent "class"))
                                   (member measure (mapcar (lambda (c) (attr c "id")) (children-elements parent))
                                           :test #'equal)))))
                    (t
                     (check (equal (sp:element-id (sp:element-parent s id)) (attr parent "id"))))))))))))
    (ok (null bad) (format nil "~A: misplaced groups ~S" name (reverse bad)))))

(deftest null-group-order
  (let* ((s (fixture-score "sc0-satb"))
         (page (first (render s)))
         (system (find "system.1" (svg-elements page) :key (lambda (e) (attr e "id")) :test #'equal))
         (m1 (find "m-1" (children-elements system) :key (lambda (e) (attr e "id")) :test #'equal)))
    (ok (equal '("m-1" "m-3" "m-4" "sp-1p" "sp-1q")
               (mapcar (lambda (e) (attr e "id")) (children-elements system)))
        "measures, then spanners")
    (ok (equal '("rect" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g" "g")
               (mapcar #'xml:xml-element-local-name (children-elements m1))))
    (ok (equal '(nil "tm-2" "e-9" "e-c" "e-e" "e-g" "e-t" "e-v" "e-w" "e-x"
                 "e-14" "e-16" "e-17" "e-18" "e-1f" "e-1h" "e-1i" "e-1j")
               (mapcar (lambda (e) (attr e "id")) (children-elements m1)))
        "marks, anchors, then events in document order")
    (let ((e9 (find "e-9" (children-elements m1) :key (lambda (e) (attr e "id")) :test #'equal)))
      (ok (equal '("dy-a" "ly-b") (mapcar (lambda (e) (attr e "id")) (children-elements e9)))))
    (let* ((parts (find "parts" (children-elements (svg-page-root page))
                        :key (lambda (e) (attr e "class")) :test #'equal))
           (p5 (first (children-elements parts))))
      (ok (eq parts (first (children-elements (svg-page-root page)))) "the parts group comes first")
      (ok (equal '("text" "g" "g" "g" "g" "g" "g") (mapcar #'xml:xml-element-local-name (children-elements p5))))
      (ok (equal '(nil "st-6" "st-7" "v-8" "v-s" "v-13" "v-1e")
                 (mapcar (lambda (e) (attr e "id")) (children-elements p5)))
          "staves, then voices"))))

(deftest null-chord-groups
  (let* ((s (fixture-score "sc0-keyboard"))
         (pages (render s)))
    (dolist (e (sp:score-elements s))
      (when (typep e 'sp:chord-event)
        (let ((g (cdr (first (groups-with-id pages (sp:element-id e))))))
          (ok (equal (mapcar #'sp:element-id (sp:chord-event-notes e))
                     (remove-if-not (lambda (id) (member id (mapcar #'sp:element-id (sp:chord-event-notes e))
                                                         :test #'equal))
                                    (mapcar (lambda (c) (attr c "id")) (children-elements g))))
              (format nil "chord ~A holds its notes" (sp:element-id e))))))))

(deftest null-parts-on-page-one-only
  (let ((pages (render (long-score 40) :layout '(:scale 400))))
    (ok (find "parts" (children-elements (svg-page-root (first pages)))
              :key (lambda (e) (attr e "class")) :test #'equal))
    (ok (notany (lambda (p) (find "parts" (svg-elements p) :key (lambda (e) (attr e "class")) :test #'equal))
                (rest pages)))))

;;; Page splitting -------------------------------------------------------------

(defparameter +splits+
  '((40 1 () (4 4 4 4 4 4 4 4 4 4) (10))
    (40 1 (:scale 400) (1) (4 4 4 4 4 4 4 4 4 4))
    (40 1 (:scale 400 :page-size :a5) (1) (3 3 3 3 3 3 3 3 3 3 3 3 3 1))
    (40 1 (:scale 400 :margins 0) (1) (4 4 4 4 4 4 4 4 4 4))
    (40 2 (:scale 200) (2) (5 5 5 5))
    (40 1 (:scale 33) (14 14 12) (3))
    (10 1 (:scale 2000/10) (2 2 2 2 2) (5))
    (5 3 (:scale 1000) (1) (1 1 1 1 1))
    (7 1 (:page-size (100 100) :margins (10 20 30 40) :scale 10) (7) (1)))
  "(measures staves layout measures-per-system systems-per-page). Worked out from
the null engine's arithmetic: e.g. A4 at scale 400, one staff: CW = 1900,
MW = 1600, one measure a system; CH = 2770, SH = 600, four systems a page.")

(deftest-parametrize null-page-splitting
    ((measures staves layout per-system per-page) :rows +splits+)
  (let ((pages (render (long-score measures :staves staves) :layout layout)))
    (ok (equal per-page (mapcar (lambda (p) (length (svg-page-systems p))) pages))
        (format nil "~S: systems per page" layout))
    (ok (equal (loop with left = measures
                     for i from 0
                     while (plusp left)
                     collect (let ((n (min left (nth (min i (1- (length per-system))) per-system))))
                               (decf left n)
                               n))
               (loop for p in pages append (mapcar #'length (svg-page-systems p))))
        (format nil "~S: measures per system" layout))
    (ok (equal (loop for i from 1 to (length pages) collect i) (mapcar #'svg-page-number pages)))
    (ok (equal (loop for i from 1 to (reduce #'+ per-page) collect (format nil "system.~D" i))
               (loop for p in pages
                     append (loop for e in (svg-elements p)
                                  when (equal "system" (attr e "class")) collect (attr e "id"))))
        "systems numbered over the whole rendering")))

(deftest null-geometry
  (let* ((page (first (render (long-score 40) :layout '(:scale 33))))
         (systems (remove-if-not (lambda (e) (equal "system" (attr e "class"))) (svg-elements page))))
    (ok (equal '("translate(100,100)" "translate(100,149.5)" "translate(100,199)")
               (mapcar (lambda (e) (attr e "transform")) systems))
        "Y = TOP + i x SH, SH = 49.5")
    (let ((m (second (children-elements (first systems)))))
      (ok (equal "translate(132,0)" (attr m "transform")) "X = j x MW, MW = 132")
      (ok (equal '("132" "33") (let ((r (first (children-elements m))))
                                 (list (attr r "width") (attr r "height")))))))
  (let ((root (svg-page-root (first (render (long-score 3) :layout '(:page-size (2001/10 300)))))))
    (ok (equal "0 0 2001 3000" (attr root "viewBox")))
    (ok (equal "200.1mm" (attr root "width"))))
  (dolist (page (render (long-score 20 :staves 3) :layout '(:scale 7)))
    (dolist (e (svg-elements page))
      (dolist (a (xml:xml-element-attributes e))
        (when (member (xml:xml-attribute-local-name a) '("viewBox" "transform" "width" "height") :test #'string=)
          (ok (not (find #\/ (xml:xml-attribute-value a)))
              (format nil "no ratio in ~A" (xml:xml-attribute-value a))))))))

(deftest null-empty-score
  (multiple-value-bind (pages map) (render (empty-score))
    (ok (= 1 (length pages)))
    (ok (null (svg-page-systems (first pages))))
    (ok (equal "svg" (xml:xml-element-local-name (svg-page-root (first pages)))))
    (ok (zerop (fset:size map)))))

;;; System breaks -----------------------------------------------------------------

(deftest null-explicit-system-breaks
  (let* ((s (long-score 10))
         (m (measure-ids s)))
    (let ((pages (render s :layout (list :system-breaks (list (nth 6 m) (nth 2 m))))))
      (ok (equal (list (subseq m 0 2) (subseq m 2 6) (subseq m 6))
                 (svg-page-systems (first pages)))
          "breaks before measures 3 and 7, in any order"))
    (ok (equal (list m) (svg-page-systems (first (render s :layout '(:system-breaks ())))))
        "no breaks: one system")
    (ok (equal (mapcar #'list m)
               (loop for p in (render s :layout (list :system-breaks m)) append (svg-page-systems p)))
        "a break at every measure, the first included")
    (ok (equal (mapcar #'list m)
               (loop for p in (render s :layout (list :system-breaks m :scale 1000))
                     append (svg-page-systems p))))))

(deftest-parametrize null-break-plan-round-trip
    ((measures staves layout) :rows '((40 1 (:scale 33)) (40 2 (:scale 200)) (17 1 (:scale 400 :page-size :a5)) (9 3 ())))
  (let* ((s (long-score measures :staves staves))
         (pages (render s :layout layout))
         (plan (loop for p in pages append (mapcar #'first (svg-page-systems p))))
         (again (render s :layout (append (list :system-breaks plan) layout))))
    (ok (equal (encode-pages pages) (encode-pages again))
        "the systems fed back as breaks give the same pages")))

;;; Skipping ---------------------------------------------------------------------

(defun render-skipping (engine score &rest keys)
  "RENDER-SVG taking SKIP-ELEMENT for every UNSUPPORTED-NOTATION →
(values pages id-map conditions)."
  (let ((seen '()))
    (multiple-value-bind (pages map)
        (handler-bind ((unsupported-notation
                         (lambda (c)
                           (push c seen)
                           (invoke-restart (find-restart 'skip-element c)))))
          (apply #'render-svg engine score keys))
      (values pages map (nreverse seen)))))

(deftest null-unsupported-notation
  (let* ((s (fixture-score "sc0-satb"))
         (engine (make-null-engine :unsupported '(sp:breath-mark)))
         (restarts '())
         (c (handler-case
                (handler-bind ((unsupported-notation
                                 (lambda (c) (setf restarts (mapcar #'restart-name (compute-restarts c))))))
                  (render-svg engine s))
              (unsupported-notation (c) c))))
    (ok (typep c 'unsupported-notation))
    (ok (equal "br-n" (unsupported-notation-element-id c)))
    (ok (eq 'sp:breath-mark (unsupported-notation-kind c)))
    (ok (eq engine (engrave-error-engine c)))
    (ok (member 'skip-element restarts) "SKIP-ELEMENT is offered")))

(deftest null-skip-element
  (let ((s (fixture-score "sc0-satb")))
    (multiple-value-bind (pages map seen)
        (render-skipping (make-null-engine :unsupported '(sp:breath-mark sp:fermata)) s)
      (ok (equal '("br-n" "fe-q" "fe-12" "fe-1d" "fe-1o")
                 (mapcar #'unsupported-notation-element-id seen))
          "one condition per element, in document order")
      (dolist (id '("br-n" "fe-q" "fe-12" "fe-1d" "fe-1o"))
        (ok (null (lookup map id)) (format nil "~A maps to ()" id))
        (ok (nth-value 1 (lookup map id)) (format nil "~A is still a key" id))
        (ok (null (groups-with-id pages id)) (format nil "~A is not drawn" id)))
      (ok (equal '((1 . "e-m")) (lookup map "e-m")) "its event is drawn")
      (ok (issue-pairs (check-engraving s pages map))
          "CHECK-ENGRAVING reports what was not drawn")
      (ok (equal '((:not-drawn "br-n") (:not-drawn "fe-q") (:not-drawn "fe-12")
                   (:not-drawn "fe-1d") (:not-drawn "fe-1o"))
                 (issue-pairs (check-engraving s pages map)))))))

(deftest null-skip-holders
  (let ((s (fixture-score "sc0-satb")))
    (multiple-value-bind (pages map seen)
        (render-skipping (make-null-engine :unsupported '(sp:voice)) s)
      (ok (equal '("v-8" "v-s" "v-13" "v-1e") (mapcar #'unsupported-notation-element-id seen))
          "nothing inside a skipped voice is signalled about")
      (dolist (e (sp:score-elements s))
        (let ((id (sp:element-id e)))
          (if (typep e '(or sp:measure-info sp:tempo-mark sp:part sp:staff))
              (ok (lookup map id) (format nil "~A drawn" id))
              (ok (null (lookup map id)) (format nil "~A not drawn" id)))))
      (ok (equal '(("m-1" "m-3" "m-4")) (svg-page-systems (first pages)))))
    (multiple-value-bind (pages map seen)
        (render-skipping (make-null-engine :unsupported '(sp:measure-info))
                         (fixture-score "sc0-chords-under-rests"))
      (let ((s (fixture-score "sc0-chords-under-rests")))
        (ok (equal (measure-ids s) (mapcar #'unsupported-notation-element-id seen))
            "only the measures are signalled")
        (dolist (e (sp:score-elements s))
          (let ((id (sp:element-id e)))
            (if (typep e '(or sp:part sp:staff sp:voice))
                (ok (lookup map id) (format nil "~A drawn" id))
                (ok (null (lookup map id)) (format nil "~A not drawn" id)))))
        (ok (equal (measure-ids s) (loop for p in pages append (apply #'append (svg-page-systems p))))
            "skipped measures keep their places in the systems")
        (ok (notany (lambda (p) (some (lambda (e) (equal "measure" (attr e "class"))) (svg-elements p)))
                    pages)
            "but have no group")))
    (multiple-value-bind (pages map seen)
        (render-skipping (make-null-engine :unsupported '(sp:note-event)) s)
      (declare (ignore pages))
      (ok (= 32 (length seen)) "every note of the four voices")
      (ok (null (lookup map "sp-1p")) "a spanner whose START is skipped is not drawn")
      (ok (null (lookup map "ly-b")) "nor the lyrics of a skipped note"))))

;;; Determinism ------------------------------------------------------------------------

(deftest-parametrize null-deterministic
    ((name) :rows +fixture-names+)
  (let ((s (fixture-score name)))
    (multiple-value-bind (a am) (render s :layout '(:scale 250))
      (multiple-value-bind (b bm) (render-svg (make-null-engine) s :layout (make-layout :scale 250))
        (ok (equal (encode-pages a) (encode-pages b)) (format nil "~A: the same bytes" name))
        (ok (maps-equal-p am bm) (format nil "~A: the same id map" name))))))
