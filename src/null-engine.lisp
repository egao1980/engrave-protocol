;;;; EN0 NULL-ENGINE: the reference engine. Deterministic placeholder SVG.
;;;;
;;;; It lays out no music: every element of the view becomes one SVG group,
;;;; systems are runs of measures and pages runs of systems, by the arithmetic
;;;; below. It exists so that the protocol (id maps, page splitting, system
;;;; breaks, the embed path, the conditions) can be tested without a native
;;;; engine, and so that consumers (editors, canvases) can test against a fast
;;;; engine. Same view and layout → the same pages, byte for byte.
;;;;
;;;; MAKE-NULL-ENGINE &key unsupported → a NULL-ENGINE. UNSUPPORTED is a list of
;;;; class names (symbols); NULL-ENGINE-UNSUPPORTED reads it. ENGINE-NAME is
;;;; "null"; ENGINE-AVAILABLE-P is T.
;;;;
;;;; ENGINE-RENDER-PAGES engine view layout &key page → (values pages id-map).
;;;; It renders every page whatever PAGE is.
;;;;
;;;; Drawn and skipped. The engine visits (score-elements view) in document
;;;; order. An element is skipped when one of its holders is: its
;;;; ELEMENT-PARENT; for an event also the measure of its bar; for a
;;;; chord-symbol anchor the measure of its position's bar; for a spanner its
;;;; START event. (Document order visits every holder first: measures, then
;;;; parts with their events, then anchors, then spanners.) Otherwise, when
;;;; (typep element name) for a NAME in UNSUPPORTED, it signals
;;;; UNSUPPORTED-NOTATION (ENGINE the engine, ELEMENT-ID the element's id, KIND
;;;; (class-name (class-of element))) with ERROR inside a SKIP-ELEMENT restart
;;;; (no arguments); taking it skips the element. Every other element is drawn.
;;;; So the conditions come in document order, and nothing a skipped element
;;;; holds is signalled about or drawn.
;;;;
;;;; Geometry, in user units of 1/10 mm, with f = SCALE / 100:
;;;;   W, H          the page width and height x 10
;;;;   TOP RIGHT BOTTOM LEFT  the margins x 10
;;;;   CW = W - LEFT - RIGHT, CH = H - TOP - BOTTOM   (both > 0 by MAKE-LAYOUT)
;;;;   MW = 400 f                 the width of a measure
;;;;   N  = the number of staves of the view's parts (all of them)
;;;;   SH = (100 N + 50) f        the height of a system, spacing included
;;;;   MEASURES-PER-SYSTEM = max(1, floor(CW / MW))
;;;;   SYSTEMS-PER-PAGE    = max(1, floor(CH / SH))
;;;; Systems: the measures of the view in order, split with :AUTO breaks into
;;;; runs of MEASURES-PER-SYSTEM (the last run may be shorter); with a list of
;;;; breaks, a system starts at the first measure and at every measure whose id
;;;; is in the list. Systems are numbered 1, 2, ... over the whole rendering.
;;;; Pages: the systems in order, SYSTEMS-PER-PAGE to a page (the last page may
;;;; hold fewer), numbered 1, 2, ...; a view with no measures gives one page with
;;;; no systems. SVG-PAGE-SYSTEMS of a page lists its systems' measure ids.
;;;;
;;;; Numbers in attributes are written in decimal: an integer as itself, any
;;;; other rational X as (round (* X 100)) / 100 with no trailing zeros (49.5,
;;;; 0.33); never as a ratio. Every element is made with MAKE-SVG-ELEMENT, so it
;;;; is in +SVG-NAMESPACE+. An element's group is <g id=ID class=KIND>, KIND the
;;;; element's ELEMENT-KIND in lower case ("chord-symbol", "measure").
;;;;
;;;; Page k, an xml-document (version 1.0, UTF-8) whose root is
;;;;   <svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 W H"
;;;;        width="<page width>mm" height="<page height>mm"
;;;;        class="engrave-page" data-page="k">
;;;; holding, in order:
;;;;   1. On page 1 only: <g class="parts"> holding, for every drawn part, its
;;;;      group, which holds a <text> with the part's NAME (when it has one),
;;;;      then the groups of its drawn staves and voices in document order
;;;;      (staves first). Staff and voice groups are empty: events are drawn in
;;;;      their measures.
;;;;   2. For each system on the page:
;;;;        <g id="system.J" class="system" transform="translate(LEFT,Y)">
;;;;      J the system's number, Y = TOP + i x SH for the i-th system of the page
;;;;      (from 0). It holds the groups of its drawn measures, in order, then
;;;;      the groups of the drawn spanners whose START event lies in one of its
;;;;      measures, in document order.
;;;; A measure's group (transform="translate(X,0)", X = j x MW for the j-th
;;;; measure of its system, from 0) holds, in order: <rect width=MW height=100 N
;;;; f/>; the groups of the measure's drawn marks (ending, jump marks, rehearsal
;;;; mark, tempo marks) in document order; the groups of the drawn chord-symbol
;;;; anchors whose position's bar is this measure's bar, in track order; the
;;;; groups of the drawn events of this measure in every voice, in document
;;;; order (part, voice, event). A skipped measure keeps its place in its
;;;; system (and in SVG-PAGE-SYSTEMS), without a group.
;;;; An event's group holds the groups of its drawn element-tree children, in
;;;; document order (chord notes, each followed by its tie; the note's tie;
;;;; articulations, fermata, breath mark, dynamic, lyrics). Chord-note groups
;;;; hold their tie's group. All other groups are empty.
;;;;
;;;; The id map: every element of VIEW maps to ((k . ID)), k the page its group
;;;; is on (1 for parts, staves and voices), or to () when it is not drawn.
;;;; A drawn element always has exactly one group, under its own id.

(in-package #:engrave-protocol)

(defclass null-engine (engrave-engine)
  ((unsupported :initarg :unsupported :initform '() :reader null-engine-unsupported))
  (:documentation "The reference engine: placeholder SVG, one group per element (file
header)."))

(defun make-null-engine (&key unsupported)
  "A NULL-ENGINE that refuses the classes named in UNSUPPORTED."
  (check-type unsupported list)
  (make-instance 'null-engine :unsupported (copy-list unsupported)))

(defmethod engine-name ((engine null-engine))
  "null")

;;; Numbers ------------------------------------------------------------------------

(defun %svg-number (x)
  "X in decimal for an attribute (file header): an integer as itself, another
rational rounded to hundredths, with no trailing zeros. Never a ratio."
  (if (integerp x)
      (format nil "~D" x)
      (let ((hundredths (round (* x 100))))
        (multiple-value-bind (whole fraction) (truncate (abs hundredths) 100)
          (format nil "~:[~;-~]~D~A"
                  (minusp hundredths) whole
                  (cond ((zerop fraction) "")
                        ((zerop (mod fraction 10)) (format nil ".~D" (floor fraction 10)))
                        (t (format nil ".~2,'0D" fraction))))))))

;;; Frame: geometry, systems and pages, from the measure map and the layout -----------

(defstruct (%frame (:constructor %make-frame) (:copier nil) (:predicate nil))
  "The null engine's plan of a rendering, in tenths of a millimetre."
  (width 0 :read-only t)          ; W
  (height 0 :read-only t)         ; H
  (page-width 0 :read-only t)     ; millimetres
  (page-height 0 :read-only t)
  (top 0 :read-only t)
  (left 0 :read-only t)
  (measure-width 0 :read-only t)  ; MW
  (system-height 0 :read-only t)  ; SH
  (rect-height 0 :read-only t)    ; 100 N f
  (systems '() :read-only t)      ; lists of MEASURE-INFOs, system J = position + 1
  (pages '() :read-only t))       ; lists of (J . measures), page k = position + 1

(defun %chunk (list size)
  "LIST in runs of SIZE (the last run may be shorter)."
  (loop for tail = list then (nthcdr size tail)
        while tail
        collect (subseq tail 0 (min size (length tail)))))

(defun %break-systems (measures breaks)
  "MEASURES split before the first one and before each one whose id is in BREAKS."
  (let ((systems '()) (run '()))
    (dolist (measure measures)
      (when (and run (member (score-protocol:element-id measure) breaks :test #'string=))
        (push (nreverse run) systems)
        (setf run '()))
      (push measure run))
    (when run (push (nreverse run) systems))
    (nreverse systems)))

(defun %layout-frame (view layout)
  "The geometry, systems and pages of VIEW drawn with LAYOUT (file header)."
  (destructuring-bind (page-width page-height) (layout-page-size layout)
    (destructuring-bind (top right bottom left)
        (mapcar (lambda (x) (* 10 x)) (layout-margins layout))
      (let* ((scale (layout-scale layout))
             (w (* 10 page-width))
             (h (* 10 page-height))
             (staves (loop for part in (fset:convert 'list (score-protocol:score-parts view))
                           sum (length (score-protocol:part-staves part))))
             (mw (* 4 scale))
             (sh (/ (* (+ (* 100 staves) 50) scale) 100))
             (per-system (max 1 (floor (- w left right) mw)))
             (per-page (max 1 (floor (- h top bottom) sh)))
             (measures (fset:convert 'list (score-protocol:score-measure-map view)))
             (breaks (layout-system-breaks layout))
             (systems (if (eq breaks :auto)
                          (%chunk measures per-system)
                          (%break-systems measures breaks)))
             (numbered (loop for system in systems for j from 1 collect (cons j system))))
        (%make-frame :width w :height h :page-width page-width :page-height page-height
                     :top top :left left :measure-width mw :system-height sh
                     :rect-height (* staves scale) :systems systems
                     :pages (or (%chunk numbered per-page) (list '())))))))

;;; Classify: drawn or skipped, and where each drawn element goes ----------------------

(defun %bar-measure-id (view bar)
  "The id of the measure of BAR in VIEW, or NIL when BAR is not one of its bars."
  (and (integerp bar)
       (<= (score-protocol:score-first-bar view) bar (score-protocol:score-last-bar view))
       (score-protocol:element-id (score-protocol:bar-measure view bar))))

(defun %element-measure-id (view element)
  "The id of the measure an event, chord-symbol anchor or spanner is drawn in
(its bar's, its position's bar's, its START's bar's), or NIL."
  (let ((position (typecase element
                    (score-protocol:chord-symbol-anchor (score-protocol:anchor-position element))
                    (t (score-protocol:element-position view (score-protocol:element-id element))))))
    (and (consp position) (%bar-measure-id view (car position)))))

(defun %holder-ids (view element)
  "The ids of the holders of ELEMENT (file header): its parent; for an event and
an anchor, the measure of its bar; for a spanner, its START element."
  (let ((parent (score-protocol:element-parent view (score-protocol:element-id element))))
    (remove nil
            (list (and parent (score-protocol:element-id parent))
                  (and (typep element '(or score-protocol:event score-protocol:chord-symbol-anchor))
                       (%element-measure-id view element))
                  (and (typep element 'score-protocol:spanner)
                       (let ((start (score-protocol:find-element
                                     view (score-protocol:spanner-start element) :errorp nil)))
                         (and start (score-protocol:element-id start))))))))

(defun %classify (engine view elements frame)
  "Walk ELEMENTS (VIEW's, in document order) → (values skipped buckets), two
EQUAL tables: SKIPPED holds the ids not drawn; BUCKETS maps (slot . key) to the
drawn elements filed there, newest first. Slots: :PARTS (key NIL); :EVENTS and
:ANCHORS (key a measure id); :SPANNERS (key a system number); :KIDS (key the
parent's id). Signals UNSUPPORTED-NOTATION inside SKIP-ELEMENT. The tables are
only ever looked up, never iterated into the output."
  (let ((skipped (make-hash-table :test 'equal))
        (buckets (make-hash-table :test 'equal))
        (system-of (make-hash-table :test 'equal))
        (unsupported (null-engine-unsupported engine)))
    (loop for system in (%frame-systems frame)
          for j from 1
          do (dolist (measure system)
               (setf (gethash (score-protocol:element-id measure) system-of) j)))
    (flet ((file (slot key element)
             (push element (gethash (cons slot key) buckets))))
      (dolist (element elements)
        (let ((id (score-protocol:element-id element)))
          (cond ((some (lambda (holder) (gethash holder skipped)) (%holder-ids view element))
                 (setf (gethash id skipped) t))
                ((some (lambda (name) (typep element name)) unsupported)
                 (restart-case
                     (error 'unsupported-notation
                            :engine engine
                            :element-id id
                            :kind (class-name (class-of element)))
                   (skip-element ()
                     :report "Draw the score without this element."
                     (setf (gethash id skipped) t)))))
          (unless (gethash id skipped)
            (typecase element
              (score-protocol:measure-info)  ; placed by the frame
              (score-protocol:part (file :parts nil element))
              ((or score-protocol:event score-protocol:chord-symbol-anchor)
               (let ((measure (%element-measure-id view element)))
                 (when measure
                   (file (if (typep element 'score-protocol:event) :events :anchors)
                         measure element))))
              (score-protocol:spanner
               (let* ((measure (%element-measure-id view element))
                      (j (and measure (gethash measure system-of))))
                 (when j (file :spanners j element))))
              (t
               (let ((parent (score-protocol:element-parent view id)))
                 (when parent
                   (file :kids (score-protocol:element-id parent) element)))))))))
    (values skipped buckets)))

(defun %bucket (buckets slot key)
  "The elements filed under (SLOT . KEY), in document order."
  (reverse (gethash (cons slot key) buckets)))

;;; Emit ---------------------------------------------------------------------------------

(defun %svg-node (name attrs children)
  "MAKE-SVG-ELEMENT NAME with ATTRS, then CHILDREN (xml nodes) as its children,
set at once so a long list never goes through APPLY."
  (let ((element (make-svg-element name attrs)))
    (setf (stack-xml:xml-element-children element) children)
    element))

(defun %emit-pages (frame skipped buckets)
  "→ (values pages placed): the SVG-PAGEs, and an EQUAL table id → the number
of the page its group is on, filled as each element group is made."
  (let* ((placed (make-hash-table :test 'equal))
         (mw (%frame-measure-width frame))
         (rect-attrs (list (cons "width" (%svg-number mw))
                           (cons "height" (%svg-number (%frame-rect-height frame))))))
    (labels ((group (element k extra-attrs children)
               (let ((id (score-protocol:element-id element)))
                 (setf (gethash id placed) k)
                 (%svg-node "g"
                            (list* (cons "id" id)
                                   (cons "class" (string-downcase (symbol-name (element-kind element))))
                                   extra-attrs)
                            children)))
             (tree (element k)
               (group element k '()
                      (mapcar (lambda (child) (tree child k))
                              (%bucket buckets :kids (score-protocol:element-id element)))))
             (part-group (part)
               (let ((name (score-protocol:part-name part)))
                 (group part 1 '()
                        (append (and name (list (make-svg-element "text" '() name)))
                                (mapcar (lambda (child) (tree child 1))
                                        (%bucket buckets :kids (score-protocol:element-id part)))))))
             (measure-group (measure j k)
               (let ((id (score-protocol:element-id measure)))
                 (group measure k
                        (list (cons "transform" (format nil "translate(~A,0)" (%svg-number (* j mw)))))
                        (cons (make-svg-element "rect" rect-attrs)
                              (mapcar (lambda (element) (tree element k))
                                      (append (%bucket buckets :kids id)
                                              (%bucket buckets :anchors id)
                                              (%bucket buckets :events id)))))))
             (system-group (number measures i k)
               (%svg-node "g"
                          (list (cons "id" (format nil "system.~D" number))
                                (cons "class" "system")
                                (cons "transform"
                                      (format nil "translate(~A,~A)"
                                              (%svg-number (%frame-left frame))
                                              (%svg-number (+ (%frame-top frame)
                                                              (* i (%frame-system-height frame)))))))
                          (append
                           (loop for measure in measures
                                 for j from 0
                                 unless (gethash (score-protocol:element-id measure) skipped)
                                   collect (measure-group measure j k))
                           (mapcar (lambda (spanner) (tree spanner k))
                                   (%bucket buckets :spanners number)))))
             (page (k systems)
               (let ((root (%svg-node
                            "svg"
                            (list (cons "xmlns" +svg-namespace+)
                                  (cons "viewBox" (format nil "0 0 ~A ~A"
                                                          (%svg-number (%frame-width frame))
                                                          (%svg-number (%frame-height frame))))
                                  (cons "width" (format nil "~Amm" (%svg-number (%frame-page-width frame))))
                                  (cons "height" (format nil "~Amm" (%svg-number (%frame-page-height frame))))
                                  (cons "class" "engrave-page")
                                  (cons "data-page" (format nil "~D" k)))
                            (append
                             (and (= k 1)
                                  (list (%svg-node "g" (list (cons "class" "parts"))
                                                   (mapcar #'part-group (%bucket buckets :parts nil)))))
                             (loop for (number . measures) in systems
                                   for i from 0
                                   collect (system-group number measures i k))))))
                 (make-svg-page :number k
                                :document (stack-xml:make-xml-document :children (list root))
                                :systems (loop for (nil . measures) in systems
                                               collect (mapcar #'score-protocol:element-id measures))))))
      (values (loop for systems in (%frame-pages frame)
                    for k from 1
                    collect (page k systems))
              placed))))

(defmethod engine-render-pages ((engine null-engine) view layout &key page)
  ;; Every page is rendered whatever PAGE is (file header).
  (declare (ignore page))
  (let* ((elements (score-protocol:score-elements view))
         (frame (%layout-frame view layout)))
    (multiple-value-bind (skipped buckets) (%classify engine view elements frame)
      (multiple-value-bind (pages placed) (%emit-pages frame skipped buckets)
        (values pages
                (let ((map (fset:empty-map)))
                  (dolist (element elements map)
                    (let* ((id (score-protocol:element-id element))
                           (k (gethash id placed)))
                      (setf map (fset:with map id (if k (list (cons k id)) '())))))))))))
