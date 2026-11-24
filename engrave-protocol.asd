(defsystem "engrave-protocol"
  :version "0.1.0"
  :description "CLOS engraving protocol for cl-stack: scores to SVG pages with element id maps, timemaps and PDF, behind engine backends"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "score-protocol" "0.2.0")
               (:version "music-theory" "0.5.0")
               "fset"
               (:version "xml-protocol" "0.1.0")
               (:version "xml-backend-native" "0.1.1"))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "engine")
               (:file "svg")
               (:file "layout")
               (:file "view")
               (:file "render")
               (:file "timemap")
               (:file "check")
               (:file "null-engine"))
  :in-order-to ((test-op (test-op "engrave-protocol/tests"))))

(defsystem "engrave-protocol/tests"
  :depends-on ("engrave-protocol"
               (:version "rove" "0.11.1")
               (:version "canvas-protocol" "0.2.2"))
  :pathname "tests"
  :serial t
  :components ((:static-file "fixtures/PROVENANCE.md")
               (:file "package")
               (:file "support")
               (:file "conditions-test")
               (:file "layout-test")
               (:file "view-test")
               (:file "render-test")
               (:file "null-engine-test")
               (:file "id-map-test")
               (:file "embed-test")
               (:file "timemap-test")
               (:file "check-test")
               (:file "pdf-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
