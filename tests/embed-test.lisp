(in-package #:engrave-protocol/tests)

;;; Pages embed into canvas pages through canvas-protocol's SVG-FRAGMENT (the
;;; allowlist), with no violation, no CSP finding, nothing NEUTRALIZE changes,
;;; and every SVG id kept. The text path (encode, then SVG-FRAGMENT of the text)
;;; gives the same tree: an engine that hands over SVG text embeds the same way.

(defun html-ids (node)
  "The id attributes of the html-protocol tree NODE, in document order."
  (let ((out '()))
    (labels ((walk (n)
               (when (html:html-element-p n)
                 (let ((id (cdr (assoc "id" (html:html-element-attributes n) :test #'string=))))
                   (when id (push id out)))
                 (mapc #'walk (html:html-element-children n)))))
      (walk node))
    (nreverse out)))

(defun fragment-violations (source)
  "The UNSAFE-SVG violations of (svg-fragment SOURCE), or :NONE."
  (handler-case (progn (cv:svg-fragment source) :none)
    (cv:unsafe-svg (c) (cv:csp-error-violations c))))

(deftest-parametrize embed-through-svg-fragment
    ((label score keys) :rows (renderings))
  (dolist (page (apply #'render score keys))
    (let ((where (format nil "~A page ~D" label (svg-page-number page))))
      (ok (eq :none (fragment-violations (svg-page-document page))) (format nil "~A: allowlist" where))
      (let* ((tree (cv:svg-fragment (svg-page-document page)))
             (text (cv:to-html tree)))
        (ok (null (cv:csp-violations tree)) (format nil "~A: CSP scan" where))
        (ok (equal text (cv:to-html (cv:neutralize tree))) (format nil "~A: NEUTRALIZE keeps it" where))
        (ok (equal (svg-page-ids page) (html-ids tree)) (format nil "~A: every id kept" where))
        (ok (equal text (cv:to-html (cv:svg-fragment (encode-page page))))
            (format nil "~A: the text path gives the same tree" where))))))

(deftest embed-in-a-canvas-page
  (let* ((pages (render (long-score 12) :layout '(:scale 400)))
         (body (mapcar (lambda (p) (cv:svg-fragment (svg-page-document p) :attrs '(:width nil :height nil)))
                       pages))
         (doc (cv:to-html (cv:el :main nil body))))
    (ok (= 3 (length pages)))
    (ok (null (cv:csp-violations (cv:el :main nil body))) "the pages together")
    (ok (null (search "width=\"210mm\"" doc)) ":attrs removes the print size")
    (ok (search "viewBox=\"0 0 2100 2970\"" doc) "viewBox stays")
    (ok (= (length (all-svg-ids pages))
           (length (remove-duplicates (html-ids (cv:el :main nil body)) :test #'string=)))
        "ids stay unique on one page")))
