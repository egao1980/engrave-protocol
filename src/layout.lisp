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

(defun make-layout (&key (page-size :a4) (margins 10) (scale 100) (system-breaks :auto))
  "A checked LAYOUT (file header)."
  (declare (ignore page-size margins scale system-breaks))
  (%en0-pending 'make-layout))

(defun ensure-layout (designator)
  "NIL, a LAYOUT or a MAKE-LAYOUT plist → a LAYOUT (file header)."
  (declare (ignore designator))
  (%en0-pending 'ensure-layout))
