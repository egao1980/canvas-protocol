(defsystem "canvas-protocol"
  :version "0.0.0"
  :description "Live HTML canvases for cl-stack: components, htmx helpers, freeze, chart model (H4h)"
  :author "egao1980"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "src"
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "canvas-protocol/tests"))))

(defsystem "canvas-protocol/tests"
  :depends-on ("canvas-protocol" "rove")
  :pathname "tests"
  :serial t
  :components ((:file "package"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
