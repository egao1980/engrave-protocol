;;;; EN0 conformance checks: what every engine's output must satisfy.
;;;;
;;;; Engines are tested with these, not with golden SVG: a backend's suite runs
;;;; CHECK-ENGRAVING and CHECK-TIMEMAP over its renderings, as this suite does for
;;;; NULL-ENGINE. Both return a list of ENGRAVING-ISSUEs (read-only struct:
;;;; KIND, a keyword; ID, a string or NIL; DETAIL, a plist), () when the output
;;;; conforms. Issues come in the order of the checks below and, within one
;;;; check, in the order given there.
;;;;
;;;; CHECK-ENGRAVING score pages id-map &key (view :choral) parts
;;;;   PAGES and ID-MAP are what RENDER-SVG returns for SCORE, VIEW and PARTS
;;;;   without :PAGE: a whole rendering. EXPECTED is the list of source ids of
;;;;   the elements of (engrave-view score :view view :parts parts) that its view
;;;;   map sends to SCORE (elements the view made are not expected), in the
;;;;   view's document order. "Page k" is the page whose NUMBER is k.
;;;;    1 :PAGE-NUMBER    the pages are not numbered 1, 2, ..., n in list order:
;;;;                      one issue, for the first page out of place (ID NIL,
;;;;                      DETAIL (:index i :number k), i from 0).
;;;;    2 :NOT-SVG        a page whose DOCUMENT is not an XML-DOCUMENT with a root
;;;;                      element "svg" in +SVG-NAMESPACE+ carrying viewBox, width
;;;;                      and height (ID NIL, DETAIL (:page k)), in list order.
;;;;                      Checks 3 to 5 skip such a page.
;;;;    3 :NAMESPACE      an element below the root that is not in +SVG-NAMESPACE+
;;;;                      (ID its id attribute or NIL, DETAIL (:page k :element
;;;;                      local-name)), page order then document order.
;;;;    4 :INVALID-SVG-ID an id attribute value VALID-SVG-ID-P refuses (ID the
;;;;                      value, DETAIL (:page k)), page then document order.
;;;;    5 :DUPLICATE-SVG-ID an id value already seen earlier in page then
;;;;                      document order (ID the value, DETAIL (:page k)): one
;;;;                      issue per repeat.
;;;;    6 :MISSING-ELEMENT an EXPECTED id that is not a key of ID-MAP (ID the id,
;;;;                      DETAIL ()), in EXPECTED order.
;;;;    7 :UNKNOWN-ELEMENT a key of ID-MAP not in EXPECTED (ID the key, DETAIL ()),
;;;;                      keys sorted with STRING<.
;;;;    8 :NOT-DRAWN      an EXPECTED id whose value in ID-MAP is () (ID the id,
;;;;                      DETAIL ()), in EXPECTED order.
;;;;    9 :DANGLING-REF   an item of an EXPECTED id's value that is not a cons of
;;;;                      a page number of PAGES and an id SVG-PAGE-IDS of that
;;;;                      page contains (ID the element id, DETAIL (:ref item)),
;;;;                      in EXPECTED order, then value order.
;;;;   10 :SYSTEMS        the SYSTEMS of the pages, concatenated in list order,
;;;;                      are not the measure ids of the view's measure map, each
;;;;                      in its own place (ID NIL, DETAIL (:expected ids :actual
;;;;                      ids), both flat lists of measure ids): at most one issue.
;;;;   SCORE not a score → TYPE-ERROR; bad VIEW or PARTS → ENGRAVE-VIEW's
;;;;   INVALID-RENDER-OPTION.
;;;;
;;;; CHECK-TIMEMAP score timemap &key (unfold t)
;;;;   TIMEMAP is what RENDER-TIMEMAP returns for SCORE and UNFOLD. TIMED is the
;;;;   list of SCORE's timed events (timemap.lisp), in document order; "entry i"
;;;;   is the i-th entry, from 0.
;;;;    1 :ONSET-ORDER    an entry whose ONSET is not greater than the ONSET of
;;;;                      the entry before it (ID NIL, DETAIL (:entry i)), in
;;;;                      entry order. (MAKE-TIMEMAP-ENTRY refuses an ONSET that
;;;;                      is not a real >= 0.)
;;;;    2 :UNKNOWN-EVENT  an id in an entry's ON or OFF that is not the id of a
;;;;                      TIMED event (ID the id, DETAIL (:entry i)), in entry
;;;;                      order, ON before OFF, list order.
;;;;    3 :EVENT-COUNT    a TIMED event that does not occur exactly E times in ON
;;;;                      and E times in OFF over all entries, where E is the
;;;;                      number of events of (score-protocol:unfold score) that
;;;;                      its id map sends to the event when UNFOLD is true, and 1
;;;;                      when it is false (ID the event id, DETAIL (:expected E
;;;;                      :on n :off m)), in TIMED order.
;;;;    4 :OFF-BEFORE-ON  a TIMED event whose k-th occurrence in OFF is in an
;;;;                      entry at or before its k-th occurrence in ON, for some k
;;;;                      up to the lesser count (ID the event id, DETAIL (:on i
;;;;                      :off j), the entries of the first such k), in TIMED order.
;;;;   SCORE not a score → TYPE-ERROR.

(in-package #:engrave-protocol)

(defparameter +engraving-issue-kinds+
  '(:page-number :not-svg :namespace :invalid-svg-id :duplicate-svg-id
    :missing-element :unknown-element :not-drawn :dangling-ref :systems)
  "CHECK-ENGRAVING issue kinds, in check order. Data.")

(defparameter +timemap-issue-kinds+
  '(:onset-order :unknown-event :event-count :off-before-on)
  "CHECK-TIMEMAP issue kinds, in check order. Data.")

(defstruct (engraving-issue (:constructor make-engraving-issue (&key kind id detail)))
  "One conformance problem (file header)."
  (kind nil :type (or null keyword) :read-only t)
  (id nil :type (or null string) :read-only t)
  (detail '() :type list :read-only t))

(defun %items (object)
  "The items of OBJECT read as a list: the elements of its conses, and a final
non-NIL atom as one more item. So an atom is one item and NIL none."
  (loop for tail = object then (cdr tail)
        while (consp tail)
        collect (car tail) into items
        finally (return (if tail (append items (list tail)) items))))

(defun %svg-root (document)
  "DOCUMENT's root element when DOCUMENT is an SVG document (check 2), else NIL."
  (let ((root (and (stack-xml:xml-document-p document)
                   (stack-xml:document-root-element document))))
    (and root
         (string= "svg" (stack-xml:xml-element-local-name root))
         (equal +svg-namespace+ (stack-xml:xml-element-namespace-uri root))
         (%exact-attr root "viewBox")
         (%exact-attr root "width")
         (%exact-attr root "height")
         root)))

(defun check-engraving (score pages id-map &key (view :choral) parts)
  "Conformance issues of a whole rendering of SCORE (file header)."
  (multiple-value-bind (v view-map) (engrave-view score :view view :parts parts)
    (let* ((expected (loop for element in (score-protocol:score-elements v)
                           for (source found) = (multiple-value-list
                                                 (fset:lookup view-map (score-protocol:element-id element)))
                           when found collect source))
           ;; Page facts, once: (number root ids), ROOT NIL for a page that is not SVG.
           (facts (mapcar (lambda (page)
                            (let ((root (%svg-root (svg-page-document page))))
                              (list (svg-page-number page) root (and root (svg-page-ids page)))))
                          pages))
           (svg-facts (remove nil facts :key #'second))
           (issues '()))
      (flet ((issue (kind id &rest detail)
               (push (make-engraving-issue :kind kind :id id :detail detail) issues)))
        ;; 1 :PAGE-NUMBER, the first page out of place.
        (loop for (number) in facts
              for i from 0
              unless (eql number (1+ i))
                do (issue :page-number nil :index i :number number)
                   (return))
        ;; 2 :NOT-SVG
        (loop for (number root) in facts
              unless root do (issue :not-svg nil :page number))
        ;; 3 :NAMESPACE, below the root.
        (loop for (number root) in svg-facts
              do (dolist (child (stack-xml:xml-element-children root))
                   (%walk-elements (lambda (element)
                                     (unless (equal +svg-namespace+
                                                    (stack-xml:xml-element-namespace-uri element))
                                       (issue :namespace (%svg-id element)
                                              :page number
                                              :element (stack-xml:xml-element-local-name element))))
                                   child)))
        ;; 4 :INVALID-SVG-ID
        (loop for (number nil ids) in svg-facts
              do (dolist (id ids)
                   (unless (valid-svg-id-p id)
                     (issue :invalid-svg-id id :page number))))
        ;; 5 :DUPLICATE-SVG-ID, over all pages.
        (let ((seen (make-hash-table :test 'equal)))
          (loop for (number nil ids) in svg-facts
                do (dolist (id ids)
                     (if (gethash id seen)
                         (issue :duplicate-svg-id id :page number)
                         (setf (gethash id seen) t)))))
        ;; 6 :MISSING-ELEMENT
        (dolist (id expected)
          (unless (nth-value 1 (fset:lookup id-map id))
            (issue :missing-element id)))
        ;; 7 :UNKNOWN-ELEMENT, sorted.
        (let ((wanted (make-hash-table :test 'equal))
              (unknown '()))
          (dolist (id expected) (setf (gethash id wanted) t))
          (fset:do-map (id refs id-map)
            (declare (ignore refs))
            (unless (gethash id wanted) (push id unknown)))
          (dolist (id (sort unknown #'string<))
            (issue :unknown-element id)))
        ;; 8 :NOT-DRAWN
        (dolist (id expected)
          (multiple-value-bind (refs found) (fset:lookup id-map id)
            (when (and found (null refs))
              (issue :not-drawn id))))
        ;; 9 :DANGLING-REF. Page number → EQUAL set of its ids (empty for a page
        ;; that is not SVG); pages sharing a number pool their ids.
        (let ((sets (make-hash-table)))
          (loop for (number nil ids) in facts
                do (let ((set (or (gethash number sets)
                                  (setf (gethash number sets) (make-hash-table :test 'equal)))))
                     (dolist (id ids) (setf (gethash id set) t))))
          (dolist (id expected)
            (multiple-value-bind (refs found) (fset:lookup id-map id)
              (when found
                (dolist (item (%items refs))
                  (unless (and (consp item)
                               (stringp (cdr item))
                               (let ((set (gethash (car item) sets)))
                                 (and set (gethash (cdr item) set))))
                    (issue :dangling-ref id :ref item)))))))
        ;; 10 :SYSTEMS
        (let ((wanted (%measure-ids v))
              (actual (loop for page in pages
                            append (loop for system in (%items (svg-page-systems page))
                                         append (%items system)))))
          (unless (equal wanted actual)
            (issue :systems nil :expected wanted :actual actual))))
      (nreverse issues))))

(defun %positions-by-id (timemap reader)
  "An EQUAL table: id → the indices of the entries of TIMEMAP whose READER list
holds it, in entry order, once per occurrence."
  (let ((table (make-hash-table :test 'equal)))
    (loop for entry in timemap
          for i from 0
          do (dolist (id (funcall reader entry))
               (push i (gethash id table))))
    (maphash (lambda (id indices) (setf (gethash id table) (reverse indices))) table)
    table))

(defun %unfolded-counts (score)
  "An EQUAL table: source event id → how many events of (unfold SCORE) its id
map sends to it."
  (let ((counts (make-hash-table :test 'equal)))
    (multiple-value-bind (view id-map) (score-protocol:unfold score)
      (dolist (element (score-protocol:score-elements view) counts)
        (when (typep element 'score-protocol:event)
          (multiple-value-bind (source found)
              (fset:lookup id-map (score-protocol:element-id element))
            (when found (incf (gethash source counts 0)))))))))

(defun check-timemap (score timemap &key (unfold t))
  "Conformance issues of a timemap of SCORE (file header)."
  (check-type score score-protocol:score)
  (let* ((timed (mapcar #'score-protocol:element-id
                        (remove-if-not #'%timed-event-p (score-protocol:score-elements score))))
         (timed-set (let ((set (make-hash-table :test 'equal)))
                      (dolist (id timed set) (setf (gethash id set) t))))
         (ons (%positions-by-id timemap #'timemap-entry-on))
         (offs (%positions-by-id timemap #'timemap-entry-off))
         (counts (and unfold (%unfolded-counts score)))
         (issues '()))
    (flet ((issue (kind id &rest detail)
             (push (make-engraving-issue :kind kind :id id :detail detail) issues)))
      ;; 1 :ONSET-ORDER
      (loop for (before entry) on timemap
            for i from 1
            while entry
            unless (> (timemap-entry-onset entry) (timemap-entry-onset before))
              do (issue :onset-order nil :entry i))
      ;; 2 :UNKNOWN-EVENT, ON before OFF.
      (loop for entry in timemap
            for i from 0
            do (dolist (id (append (timemap-entry-on entry) (timemap-entry-off entry)))
                 (unless (gethash id timed-set)
                   (issue :unknown-event id :entry i))))
      ;; 3 :EVENT-COUNT
      (dolist (id timed)
        (let ((expected (if unfold (gethash id counts 0) 1))
              (on (length (gethash id ons)))
              (off (length (gethash id offs))))
          (unless (= expected on off)
            (issue :event-count id :expected expected :on on :off off))))
      ;; 4 :OFF-BEFORE-ON, the first k whose OFF is at or before its ON.
      (dolist (id timed)
        (loop for i in (gethash id ons)
              for j in (gethash id offs)
              when (<= j i)
                do (issue :off-before-on id :on i :off j)
                   (return))))
    (nreverse issues)))
