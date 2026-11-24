;;;; EN0 layout: page size, margins, scale and system breaks.
;;;;
;;;; A LAYOUT is a value (read-only struct). Lengths are millimetres, as CL
;;;; rationals (integers included; floats are refused) whose tenfold is an
;;;; integer, so every length is a whole number of tenths of a millimetre, the
;;;; unit engines draw in (an A4 page is 2100 x 2970).
;;;;   PAGE-SIZE      (width height), both > 0
;;;;   MARGINS        (top right bottom left), all >= 0
;;;;   SCALE          an integer 1..1000, percent of the engine's normal size
;;;;   SYSTEM-BREAKS  :AUTO (the engine breaks lines), or a list of measure ids:
;;;;                  a new system starts at each of them (and at the first
;;;;                  measure). Their order in the list does not matter.
;;;;
;;;; MAKE-LAYOUT &key (page-size :a4) (margins 10) (scale 100) (system-breaks :auto)
;;;; → a LAYOUT, after these checks, each failure an INVALID-RENDER-OPTION whose
;;;; OPTION is the keyword and VALUE the argument as given (ENGINE NIL):
;;;;   :PAGE-SIZE      a key of +PAGE-SIZES+ (its (width height)), or a list of
;;;;                   two lengths > 0, stored as a fresh list
;;;;   :MARGINS        one length >= 0 (all four sides) or a list of four, stored
;;;;                   as a fresh list of four; left + right must be less than the
;;;;                   width and top + bottom less than the height
;;;;   :SCALE          an integer from 1 to 1000
;;;;   :SYSTEM-BREAKS  :AUTO, or a list of distinct strings each satisfying
;;;;                   SCORE-PROTOCOL:VALID-ID-P, stored as a fresh list in the
;;;;                   given order. Whether they are measures of the score is
;;;;                   checked by RENDER-SVG, which knows the score.
;;;; The checks run in that order (page size before margins).
;;;;
;;;; ENSURE-LAYOUT designator → a LAYOUT: NIL → (make-layout); a LAYOUT → itself;
;;;; a plist → (apply #'make-layout plist). Anything else, a list of odd length,
;;;; or a plist with a key MAKE-LAYOUT does not take → INVALID-RENDER-OPTION with
;;;; OPTION :LAYOUT and VALUE the designator.

(in-package #:engrave-protocol)

(defparameter +page-sizes+
  '((:a3 297 420) (:a4 210 297) (:a5 148 210) (:b4 250 353) (:b5 176 250)
    (:letter 2159/10 2794/10) (:legal 2159/10 3556/10) (:tabloid 2794/10 4318/10))
  "Named page sizes: (keyword width height), millimetres. Data.")

(defstruct (layout (:constructor %make-layout (page-size margins scale system-breaks)))
  "Page size, margins, scale and system breaks (file header). Build with MAKE-LAYOUT."
  (page-size '(210 297) :type list :read-only t)
  (margins '(10 10 10 10) :type list :read-only t)
  (scale 100 :type (integer 1 1000) :read-only t)
  (system-breaks :auto :read-only t))

(defparameter +layout-keys+ '(:page-size :margins :scale :system-breaks)
  "The keywords MAKE-LAYOUT takes, so the keys of an ENSURE-LAYOUT plist. Data.")

(defun %proper-list-p (object)
  "True when OBJECT is a proper list (NIL included)."
  (loop for tail = object then (cdr tail)
        while (consp tail)
        finally (return (null tail))))

(defun %length-p (object)
  "True when OBJECT is a length: a rational whose tenfold is an integer."
  (and (rationalp object) (integerp (* 10 object))))

(defun %refuse-layout-option (option value)
  (error 'invalid-render-option :option option :value value))

(defun %layout-page-size (page-size)
  "PAGE-SIZE as a fresh (width height), or INVALID-RENDER-OPTION."
  (let ((named (and (symbolp page-size) (assoc page-size +page-sizes+))))
    (cond (named (copy-list (rest named)))
          ((and (%proper-list-p page-size)
                (= 2 (length page-size))
                (every (lambda (x) (and (%length-p x) (plusp x))) page-size))
           (copy-list page-size))
          (t (%refuse-layout-option :page-size page-size)))))

(defun %layout-margins (margins width height)
  "MARGINS as a fresh (top right bottom left) that fits WIDTH x HEIGHT, or
INVALID-RENDER-OPTION."
  (flet ((side-p (x) (and (%length-p x) (>= x 0))))
    (let ((sides (cond ((side-p margins) (list margins margins margins margins))
                       ((and (%proper-list-p margins)
                             (= 4 (length margins))
                             (every #'side-p margins))
                        (copy-list margins))
                       (t (%refuse-layout-option :margins margins)))))
      (destructuring-bind (top right bottom left) sides
        (unless (and (< (+ left right) width) (< (+ top bottom) height))
          (%refuse-layout-option :margins margins)))
      sides)))

(defun %layout-system-breaks (system-breaks)
  "SYSTEM-BREAKS as :AUTO or a fresh list of distinct ids, or INVALID-RENDER-OPTION."
  (cond ((eq system-breaks :auto) :auto)
        ((and (%proper-list-p system-breaks)
              (every #'score-protocol:valid-id-p system-breaks)
              (= (length system-breaks)
                 (length (remove-duplicates system-breaks :test #'string=))))
         (copy-list system-breaks))
        (t (%refuse-layout-option :system-breaks system-breaks))))

(defun make-layout (&key (page-size :a4) (margins 10) (scale 100) (system-breaks :auto))
  "A checked LAYOUT (file header)."
  (let* ((size (%layout-page-size page-size))
         (sides (%layout-margins margins (first size) (second size))))
    (unless (typep scale '(integer 1 1000))
      (%refuse-layout-option :scale scale))
    (%make-layout size sides scale (%layout-system-breaks system-breaks))))

(defun %layout-plist-p (object)
  "True when OBJECT is a proper plist whose keys MAKE-LAYOUT takes."
  (and (%proper-list-p object)
       (evenp (length object))
       (loop for key in object by #'cddr
             always (member key +layout-keys+))))

(defun ensure-layout (designator)
  "NIL, a LAYOUT or a MAKE-LAYOUT plist → a LAYOUT (file header)."
  (cond ((null designator) (make-layout))
        ((layout-p designator) designator)
        ((and (consp designator) (%layout-plist-p designator))
         (apply #'make-layout designator))
        (t (%refuse-layout-option :layout designator))))
