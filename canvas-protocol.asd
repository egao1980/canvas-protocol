(defsystem "canvas-protocol"
  :version "0.1.1"
  :description "Live HTML canvases for cl-stack: components, htmx helpers, freeze, chart model (H4h)"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "html-protocol" "0.1.2")
               (:version "css-protocol" "0.1.0")
               (:version "sse-protocol" "0.1.1")
               (:version "encoding-protocol" "0.2.0")
               (:version "json-protocol" "0.2.2")
               (:version "json-backend-jzon" "0.2.2"))
  :serial t
  :components ((:module "assets"
                :components ((:static-file "htmx.min.js")
                             (:static-file "htmx-ext-sse.min.js")
                             (:static-file "canvas.css")
                             (:static-file "SHA256SUMS")
                             (:static-file "LICENSE.htmx")
                             (:static-file "LICENSE.htmx-ext-sse")
                             (:static-file "README.md")))
               (:module "src"
                :serial t
                :components ((:file "package")
                             (:file "conditions")
                             (:file "nodes")
                             (:file "resolver")
                             (:file "htmx")
                             (:file "components")
                             (:file "table")
                             (:file "forms")
                             (:file "assets")
                             (:file "page")
                             (:file "chart")
                             (:file "theme"))))
  :in-order-to ((test-op (test-op "canvas-protocol/tests"))))

(defsystem "canvas-protocol/tests"
  :depends-on ("canvas-protocol" "rove"
               (:version "crypto-protocol" "0.2.0")
               (:version "crypto-backend-ironclad" "0.2.1"))
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "render-test")
               (:file "layout-test")
               (:file "table-test")
               (:file "form-test")
               (:file "htmx-test")
               (:file "sse-test")
               (:file "page-test")
               (:file "csp-test")
               (:file "freeze-test")
               (:file "assets-test")
               (:file "chart-test")
               (:file "theme-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
