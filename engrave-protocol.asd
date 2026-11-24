(defsystem "engrave-protocol"
  :version "0.0.1"
  :description "CLOS engraving protocol for cl-stack: scores to SVG pages with element id maps, timemaps and PDF, behind engine backends"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "engrave-protocol/tests"))))

(defsystem "engrave-protocol/tests"
  :depends-on ("engrave-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "engrave-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
