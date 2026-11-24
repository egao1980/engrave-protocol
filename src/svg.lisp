;;;; EN0 SVG pages: the page value, SVG ids, element kinds, tree helpers.
;;;;
;;;; A rendering is a list of SVG-PAGEs. An SVG-PAGE is a value (read-only
;;;; struct):
;;;;   NUMBER    the page number, an integer >= 1
;;;;   DOCUMENT  an xml-protocol XML-DOCUMENT whose root element is "svg" in
;;;;             +SVG-NAMESPACE+, with viewBox, width and height attributes;
;;;;             width and height are the layout's page size in millimetres
;;;;             ("210mm", "215.9mm"). Every element of the page is in
;;;;             +SVG-NAMESPACE+ (not only the root): canvas-protocol's
;;;;             SVG-FRAGMENT refuses other namespaces, so a page must embed as
;;;;             it is. The SVG is a tree, never a string: an engine that gets
;;;;             text from its library parses it with STACK-XML:DECODE.
;;;;   SYSTEMS   the systems drawn on the page, in order: each a list of the ids
;;;;             of the measures it holds, in measure order. Concatenated over
;;;;             the pages of a whole rendering, the systems hold every measure of
;;;;             the view exactly once, in order. This is the break plan a caller
;;;;             feeds back as :SYSTEM-BREAKS to get the same lines again.
;;;; MAKE-SVG-PAGE &key number document systems checks NUMBER (TYPE-ERROR) and
;;;; stores the rest as given.
;;;;
;;;; SVG ids. Every id attribute on a page is an ASCII XML NCName
;;;; (VALID-SVG-ID-P): a first character in A-Z a-z _, the others in A-Z a-z 0-9
;;;; "." "-" "_". Ids are unique over all the pages of one rendering, so the
;;;; pages of a score can share one HTML document. An element the engine draws
;;;; once as an object of its own carries the element's own id: score-protocol
;;;; ids are NCNames without ".", and slice ids ("m-3.v-8", SLICE-ID) name one
;;;; part's, staff's or voice's share of a measure. Ids an engine makes up for
;;;; anything else (pages, systems, glyph definitions) must be neither element
;;;; ids nor slice ids of the view. NULL-ENGINE writes "system.<n>" (n a decimal
;;;; integer): no element id contains ".", and the part of a slice id after its
;;;; "." starts with a letter.
;;;;
;;;; MAKE-SVG-ELEMENT name &optional attrs &rest children: STACK-XML:MAKE-XML-ELEMENT
;;;; (same arguments: ATTRS an alist of (name . value), CHILDREN xml nodes or
;;;; strings) with the element put in +SVG-NAMESPACE+. NAME must have no prefix.
;;;; A page's root also needs ("xmlns" . +svg-namespace+) in ATTRS, so that
;;;; STACK-XML:ENCODE writes a standalone SVG document; MAKE-SVG-ELEMENT adds
;;;; nothing.
;;;;
;;;; SVG-PAGE-ROOT page: the root element of the page's document.
;;;; SVG-PAGE-IDS page: the values of the id attributes (local name "id", no
;;;;   prefix, no namespace) of every element of the page, the root included, in
;;;;   document order (pre-order), duplicates kept.
;;;; FIND-SVG-ELEMENT page svg-id: the first element in document order whose id
;;;;   attribute is SVG-ID (STRING=), or NIL.
;;;;
;;;; ELEMENT-KIND element: the keyword of the first +ELEMENT-KINDS+ row whose
;;;; class ELEMENT is an instance of; NIL for anything else. NULL-ENGINE writes
;;;; it in lower case as the class attribute of each element's group.

(in-package #:engrave-protocol)

(defparameter +svg-namespace+ "http://www.w3.org/2000/svg"
  "The SVG namespace URI. Every element of an SVG page is in it.")

(defparameter +element-kinds+
  '((score-protocol:measure-info . :measure) (score-protocol:part . :part)
    (score-protocol:staff . :staff) (score-protocol:voice . :voice)
    (score-protocol:note-event . :note) (score-protocol:chord-event . :chord)
    (score-protocol:rest-event . :rest) (score-protocol:chord-note . :chord-note)
    (score-protocol:lyric . :lyric) (score-protocol:dynamic-mark . :dynamic)
    (score-protocol:fermata . :fermata) (score-protocol:breath-mark . :breath-mark)
    (score-protocol:articulation . :articulation) (score-protocol:tie . :tie)
    (score-protocol:tempo-mark . :tempo-mark) (score-protocol:rehearsal-mark . :rehearsal-mark)
    (score-protocol:jump-mark . :jump-mark) (score-protocol:ending . :ending)
    (score-protocol:chord-symbol-anchor . :chord-symbol) (score-protocol:spanner . :spanner))
  "score-protocol element class → kind keyword. Data.")

(defun element-kind (element)
  "The kind keyword of ELEMENT (+ELEMENT-KINDS+), or NIL."
  (cdr (find-if (lambda (row) (typep element (car row))) +element-kinds+)))

(defun valid-svg-id-p (object)
  "True when OBJECT is a string that is an ASCII XML NCName (file header)."
  (flet ((letter-p (c) (let ((code (char-code c)))
                         (or (<= 65 code 90) (<= 97 code 122))))
         (digit-p (c) (<= 48 (char-code c) 57)))
    (and (stringp object)
         (plusp (length object))
         (let ((first (char object 0)))
           (or (letter-p first) (char= first #\_)))
         (every (lambda (c) (or (letter-p c) (digit-p c) (find c "._-"))) object)
         t)))

(defun make-svg-element (name &optional attrs &rest children)
  "An xml-protocol element NAME in the SVG namespace (file header)."
  (check-type name string)
  (assert (not (find #\: name)) (name) "MAKE-SVG-ELEMENT: ~S has a prefix" name)
  (let ((element (apply #'stack-xml:make-xml-element name attrs children)))
    (setf (stack-xml:xml-element-namespace-uri element) +svg-namespace+)
    element))

(defstruct (svg-page (:constructor make-svg-page (&key number document systems)))
  "One engraved page (file header)."
  (number 1 :type (integer 1) :read-only t)
  (document nil :read-only t)
  (systems '() :type list :read-only t))

(defun svg-page-root (page)
  "The root element of PAGE's SVG document."
  (stack-xml:document-root-element (svg-page-document page)))

(defun %no-namespace-p (string)
  "True for NIL or \"\": no prefix, no namespace."
  (or (null string) (equal string "")))

(defun %exact-attr (element name)
  "The value of ELEMENT's attribute whose local name is NAME (STRING=) and that
has no prefix and no namespace, or NIL. Unlike STACK-XML:XML-ATTR this is
case-sensitive and never matches a prefixed attribute such as xml:id."
  (let ((attribute (find-if (lambda (a)
                              (and (stack-xml:xml-attribute-p a)
                                   (string= name (stack-xml:xml-attribute-local-name a))
                                   (%no-namespace-p (stack-xml:xml-attribute-prefix a))
                                   (%no-namespace-p (stack-xml:xml-attribute-namespace-uri a))))
                            (stack-xml:xml-element-attributes element))))
    (and attribute (stack-xml:xml-attribute-value attribute))))

(defun %svg-id (element)
  "ELEMENT's id attribute (file header), or NIL."
  (%exact-attr element "id"))

(defun %walk-elements (function element)
  "Call FUNCTION on ELEMENT and every element below it, in pre-order (text and
other nodes are skipped). ELEMENT may be NIL."
  (when (stack-xml:xml-element-p element)
    (funcall function element)
    (dolist (child (stack-xml:xml-element-children element))
      (%walk-elements function child))))

(defun svg-page-ids (page)
  "Every id attribute value on PAGE, in document order (file header)."
  (let ((ids '()))
    (%walk-elements (lambda (element)
                      (let ((id (%svg-id element)))
                        (when id (push id ids))))
                    (svg-page-root page))
    (nreverse ids)))

(defun find-svg-element (page svg-id)
  "The first element of PAGE whose id is SVG-ID, or NIL (file header)."
  (%walk-elements (lambda (element)
                    (let ((id (%svg-id element)))
                      (when (and id (string= id svg-id))
                        (return-from find-svg-element element))))
                  (svg-page-root page))
  nil)
