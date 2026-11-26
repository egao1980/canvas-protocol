(in-package #:canvas-protocol/tests)

;;; 0.3.0 (canvas-protocol#15): app assets (REGISTER-ASSET / DEFINE-ASSET) and
;;; PAGE / FREEZE :scripts. Rules: src/assets.lisp (App assets), src/page.lisp
;;; (Scripts).

(defparameter +hello-sha+ "180f54fa278d6f4f0d66601b63549f68eec17c7644bdfa0e44edec0bf6d9c3db"
  "shasum -a 256 tests/fixtures/app/hello.js")
(defparameter +kit-wasm-sha+ "93a44bbb96c751218e4c00d479e4c14358122a389acca16205b1e4d0dc5f9476")
(defparameter +kit-mjs-sha+ "40ecb551dbff0078a0de56c5fa1485e44934c8aa86a316c5b0eabf7a48941417")

(defmacro with-app-assets (() &body body)
  "BODY with an empty app asset table, restored afterwards."
  `(let ((canvas-protocol::*app-assets* '()))
     ,@body))

(defun fixture (file)
  (s+ "tests/fixtures/app/" file))

(defun register-fixtures ()
  (define-asset "app/hello.js" :system "canvas-protocol" :file (fixture "hello.js")
                               :sha256 +hello-sha+ :license "MIT")
  (define-asset "vendor/kit/kit.wasm" :system "canvas-protocol" :file (fixture "vendor/kit/kit.wasm")
                                      :sha256 (string-upcase +kit-wasm-sha+)
                                      :license "Apache-2.0"
                                      :license-file (fixture "vendor/kit/LICENSE")
                                      :version "1.2.3" :source "https://example.org/kit-1.2.3.tgz")
  (register-asset "vendor/kit/kit.mjs" :system "canvas-protocol" :file (fixture "vendor/kit/kit.mjs")
                                       :sha256 +kit-mjs-sha+))

(defun ascii-octets (string)
  (coerce (map 'vector #'char-code string) '(simple-array (unsigned-byte 8) (*))))

(defun asset-names-of-app ()
  (mapcar #'car canvas-protocol::*app-assets*))

(deftest app-asset-registration
  (with-app-assets ()
    (ok (equal "app/hello.js" (define-asset "app/hello.js" :system "canvas-protocol"
                                                           :file (fixture "hello.js")
                                                           :sha256 +hello-sha+)))
    (register-fixtures)
    (ok (equal '("app/hello.js" "canvas-keys.js" "canvas.css" "htmx-ext-sse.min.js" "htmx.min.js"
                 "vendor/kit/kit.mjs" "vendor/kit/kit.wasm")
               (asset-names))
        "built-ins and app assets, sorted; registering again replaces")
    (let ((js (asset-bytes "app/hello.js")))
      (ok (typep js '(simple-array (unsigned-byte 8) (*))))
      (ok (string= "document.documentElement.dataset.app = \"ready\";
" (map 'string #'code-char js)))
      (ok (string= +hello-sha+ (sha256-hex js))))
    (ok (equalp #(0 97 115 109 1 0 0 0) (asset-bytes "vendor/kit/kit.wasm")) "exact binary bytes")
    (ok (string= "text/javascript; charset=utf-8" (asset-content-type "app/hello.js")))
    (ok (string= "text/javascript; charset=utf-8" (asset-content-type "vendor/kit/kit.mjs")))
    (ok (string= "application/wasm" (asset-content-type "vendor/kit/kit.wasm")))
    (ok (string= +kit-wasm-sha+ (asset-sha256 "vendor/kit/kit.wasm")) "lowercase hex")
    (let ((i (asset-info "vendor/kit/kit.wasm")))
      (ok (string= "vendor/kit/kit.wasm" (getf i :name)))
      (ok (string= "1.2.3" (getf i :version)))
      (ok (string= "Apache-2.0" (getf i :license)))
      (ok (string= "https://example.org/kit-1.2.3.tgz" (getf i :source)))
      (ok (string= "application/wasm" (getf i :content-type)))
      (ok (string= +kit-wasm-sha+ (getf i :sha256))))
    (ok (string= "Test fixture licence text (MIT).
" (asset-license "vendor/kit/kit.wasm")))
    (ok (null (asset-license "app/hello.js")))
    (ok (string= "/canvas-assets/vendor/kit/kit.wasm"
                 (resolve-asset-url (make-path-resolver) "vendor/kit/kit.wasm"))))
  (ok (not (member "app/hello.js" (asset-names) :test #'string=)) "table restored"))

(deftest app-asset-content-types
  (with-app-assets ()
    (register-asset "a/b.css" :octets (ascii-octets "p{}"))
    (register-asset "a/b.json" :octets (ascii-octets "{}"))
    (register-asset "a/b.data" :octets (ascii-octets "x"))
    (register-asset "a/b" :octets (ascii-octets "x"))
    (register-asset "a/c.js" :octets (ascii-octets "1") :content-type "application/javascript")
    (ok (string= "text/css; charset=utf-8" (asset-content-type "a/b.css")))
    (ok (string= "application/json; charset=utf-8" (asset-content-type "a/b.json")))
    (ok (string= "application/octet-stream" (asset-content-type "a/b.data")))
    (ok (string= "application/octet-stream" (asset-content-type "a/b")))
    (ok (string= "application/javascript" (asset-content-type "a/c.js")) "explicit wins")))

(deftest app-asset-checksum-mismatch
  (with-app-assets ()
    (let ((c (handler-case (register-asset "app/hello.js" :system "canvas-protocol"
                                                          :file (fixture "hello.js")
                                                          :sha256 +kit-wasm-sha+)
               (canvas-asset-checksum-mismatch (c) c))))
      (ok (typep c 'canvas-asset-checksum-mismatch))
      (ok (typep c 'canvas-error))
      (ok (and c (string= "app/hello.js" (asset-error-name c))))
      (ok (and c (string= +kit-wasm-sha+ (asset-error-expected c))))
      (ok (and c (string= +hello-sha+ (asset-error-actual c))))
      (ok (and c (contains +hello-sha+ (princ-to-string c)))))
    (ok (not (member "app/hello.js" (asset-names) :test #'string=)) "nothing registered")
    (ok (signals (define-asset "x.js" :octets (ascii-octets "1") :sha256 +hello-sha+)
                 'canvas-asset-checksum-mismatch)
        "define-asset checks too")
    (register-asset "x.js" :octets (ascii-octets "1"))
    (ok (signals (register-asset "x.js" :octets (ascii-octets "2") :sha256 +hello-sha+)
                 'canvas-asset-checksum-mismatch))
    (ok (equalp (ascii-octets "1") (asset-bytes "x.js")) "a failed re-registration keeps the old bytes")))

(deftest app-asset-names
  (dolist (name '("hello.js" "a/b/c.js" "vendor/kit-1.0/kit_x.min.js" "a~b" ".well-known/x"))
    (ok (asset-name-p name) name))
  (dolist (name (list "" "/x.js" "x/" "a//b" "../x" "a/../b" "a..b" "./a" "a/./b" "a/."
                      "a b" "a\\b" "a?b" "a#b" "a%2e" (string (code-char 233)) nil 'x))
    (ok (not (asset-name-p name)) (format nil "~S" name))
    (with-app-assets ()
      (ok (signals (register-asset name :octets (ascii-octets "1")) 'canvas-error)
          (format nil "register ~S" name))))
  (with-app-assets ()
    (dolist (name '("htmx.min.js" "canvas.css" "canvas-keys.js"))
      (ok (signals (register-asset name :octets (ascii-octets "1")) 'canvas-error) name)
      (ok (signals (remove-asset name) 'canvas-error)))))

(deftest app-asset-refusals
  (with-app-assets ()
    (ok (signals (register-asset "a.js") 'canvas-error) "no bytes")
    (ok (signals (register-asset "a.js" :octets (ascii-octets "1") :file "x.js" :system "canvas-protocol")
                 'canvas-error)
        "both")
    (ok (signals (register-asset "a.js" :file "tests/fixtures/app/hello.js") 'canvas-error)
        "relative file without :system")
    (ok (signals (register-asset "a.js" :system "canvas-protocol" :file "tests/fixtures/nope.js")
                 'canvas-error)
        "missing file")
    (ok (signals (register-asset "a.js" :octets "text") 'canvas-error) "octets, not a string")
    (ok (signals (register-asset "a.js" :octets (ascii-octets "1") :sha256 "abc") 'canvas-error))
    (ok (signals (register-asset "a.js" :octets (ascii-octets "1") :license :mit) 'canvas-error))
    (ok (null (asset-names-of-app)))
    (let ((absolute (asdf:system-relative-pathname "canvas-protocol" (fixture "hello.js"))))
      (register-asset "abs.js" :file absolute :sha256 +hello-sha+)
      (ok (string= +hello-sha+ (sha256-hex (asset-bytes "abs.js"))) "absolute pathname, no :system"))
    (ok (eq t (remove-asset "abs.js")))
    (ok (null (remove-asset "abs.js")))
    (ok (signals (asset-bytes "abs.js") 'canvas-asset-not-found))))

(deftest app-asset-large-binary
  (with-app-assets ()
    (let* ((n (* 8 1024 1024))
           (bytes (make-array n :element-type '(unsigned-byte 8))))
      (dotimes (i n) (setf (aref bytes i) (logand (* i 31) 255)))
      (let ((sha (sha256-hex bytes)))
        (register-asset "vendor/kit/big.wasm" :octets bytes :sha256 sha)
        (ok (string= sha (asset-sha256 "vendor/kit/big.wasm")))
        (ok (string= "application/wasm" (asset-content-type "vendor/kit/big.wasm")))
        (let ((held (asset-octets "vendor/kit/big.wasm")))
          (ok (= n (length held)))
          (ok (eq held (asset-octets "vendor/kit/big.wasm")) "asset-octets does not copy")
          (ok (equalp bytes held)))
        (let ((copy (asset-bytes "vendor/kit/big.wasm")))
          (ok (= n (length copy)))
          (ok (not (eq copy (asset-octets "vendor/kit/big.wasm"))) "asset-bytes copies"))))
    ;; A large file read from disk.
    (let ((path (uiop:with-temporary-file (:pathname p :keep t :type "wasm"
                                          :element-type '(unsigned-byte 8) :stream s)
                  (let ((chunk (make-array 65536 :element-type '(unsigned-byte 8) :initial-element 7)))
                    (dotimes (i 64) (write-sequence chunk s)))
                  p)))
      (unwind-protect
           (progn
             (register-asset "vendor/kit/disk.wasm" :file path)
             (ok (= (* 64 65536) (length (asset-octets "vendor/kit/disk.wasm"))))
             (ok (every (lambda (o) (= o 7)) (asset-octets "vendor/kit/disk.wasm"))))
        (delete-file path))))
  (ok (equalp (asset-octets "htmx.min.js") (asset-bytes "htmx.min.js")))
  (ok (equalp (asset-octets "canvas.css") (asset-bytes "canvas.css"))))

;;; PAGE :scripts

(defparameter +builtin-scripts+
  "<script src=\"/a/htmx.min.js\"></script><script src=\"/a/htmx-ext-sse.min.js\"></script>")

(deftest page-scripts-order
  (with-app-assets ()
    (register-fixtures)
    (let ((out (with-fake () (to-html (page (el :p nil "hi")
                                            :scripts '("vendor/kit/kit.mjs" "app/hello.js"))))))
      (ok (contains (s+ +builtin-scripts+
                        "<script src=\"/a/vendor/kit/kit.mjs\"></script>"
                        "<script src=\"/a/app/hello.js\"></script></head>")
                    out)
          "after the built-ins, in the given order"))
    (let ((out (with-fake () (to-html (page "x" :keys t :scripts '("app/hello.js"))))))
      (ok (contains (s+ +builtin-scripts+ "<script src=\"/a/canvas-keys.js\"></script>"
                        "<script src=\"/a/app/hello.js\"></script></head>")
                    out)
          "after canvas-keys.js"))
    (let ((out (with-fake () (to-html (page "x" :scripts '(("vendor/kit/kit.mjs" :module t)
                                                           ("app/hello.js" :defer t)))))))
      (ok (contains (s+ "<script src=\"/a/vendor/kit/kit.mjs\" type=\"module\"></script>"
                        "<script src=\"/a/app/hello.js\" defer=\"\"></script></head>")
                    out)))
    (let ((out (with-fake () (to-html (page "x" :htmx nil :scripts '("app/hello.js"))))))
      (ok (contains "<link rel=\"stylesheet\" href=\"/a/canvas.css\"><script src=\"/a/app/hello.js\"></script></head>"
                    out)
          ":htmx nil still writes app scripts")
      (ok (not (contains "htmx" out))))))

(deftest page-scripts-nil-identical
  (with-app-assets ()
    (register-fixtures)
    (dolist (args '(() (:keys t) (:htmx nil) (:stylesheet :inline)))
      (ok (string= (with-fake () (to-html (apply #'page (el :p nil "hi") args)))
                   (with-fake () (to-html (apply #'page (el :p nil "hi") :scripts nil args))))
          (format nil "~S" args)))
    (ok (string= (s+ +head-start+ +htmx-config-meta+
                     "<title>canvas</title><link rel=\"stylesheet\" href=\"/a/canvas.css\">"
                     +builtin-scripts+ "</head><body><p>hi</p></body></html>")
                 (with-fake () (to-html (page (el :p nil "hi") :scripts nil))))
        "the 0.2 golden")))

(deftest page-scripts-refusals
  (with-app-assets ()
    (register-fixtures)
    (let ((c (handler-case (with-fake () (page "x" :scripts '("app/nope.js")))
               (canvas-asset-not-found (c) c))))
      (ok (typep c 'canvas-asset-not-found))
      (ok (and c (string= "app/nope.js" (asset-error-name c)))))
    (ok (signals (with-fake () (page "x" :scripts '("vendor/kit/kit.wasm"))) 'canvas-error)
        "not JavaScript")
    (ok (signals (with-fake () (page "x" :scripts '("canvas.css"))) 'canvas-error))
    (ok (signals (with-fake () (page "x" :scripts '("app/hello.js" "app/hello.js"))) 'canvas-error)
        "twice")
    (ok (signals (with-fake () (page "x" :scripts '("htmx.min.js"))) 'canvas-error)
        "a built-in already loaded")
    (ok (with-fake () (page "x" :htmx nil :scripts '("htmx.min.js")))
        "a built-in the page does not load is fine")
    (ok (signals (with-fake () (page "x" :scripts "app/hello.js")) 'canvas-error) "not a list")
    (ok (signals (with-fake () (page "x" :scripts '(("app/hello.js" :async t)))) 'canvas-error))
    (ok (signals (with-fake () (page "x" :scripts '(("app/hello.js" :module)))) 'canvas-error))
    (ok (signals (with-fake () (page "x" :scripts '(42))) 'canvas-error))))

(deftest page-scripts-csp-clean
  (with-app-assets ()
    (register-fixtures)
    (let ((doc (with-fake () (page (gallery) :keys t
                                             :scripts '(("vendor/kit/kit.mjs" :module t)
                                                        "app/hello.js")))))
      (ok (null (csp-violations doc)) "script-src 'self': every script has src and no text")
      (ok (= 5 (count "script" (element-names doc) :test #'string=)))
      (walk-elements doc (lambda (e)
                           (when (string= "script" (stack-html:html-element-name e))
                             (ok (char= #\/ (char (stack-html:element-attr e "src") 0)) "same origin")
                             (ok (null (stack-html:html-element-children e)))))))))

(deftest page-scripts-path-resolver
  (with-app-assets ()
    (register-fixtures)
    (let ((*canvas-resolver* (make-path-resolver :asset-prefix "/static/")))
      (ok (contains "<script src=\"/static/vendor/kit/kit.mjs\"></script></head>"
                    (to-html (page "x" :scripts '("vendor/kit/kit.mjs"))))))))

;;; FREEZE :scripts

(deftest freeze-scripts
  (with-app-assets ()
    (register-fixtures)
    (ok (string= (s+ +head-start+ "<title>Demo</title><style>" (canvas-stylesheet) "</style>"
                     "<script src=\"vendor/kit/kit.mjs\" type=\"module\"></script>"
                     "<script src=\"app/hello.js\"></script></head>"
                     "<body data-canvas-frozen=\"\"><div data-layout=\"stack\"><p>n=5</p><button>More</button></div>"
                     "</body></html>")
                 (freeze 'frozen-demo :args '(:n "5") :title "Demo"
                                      :scripts '(("vendor/kit/kit.mjs" :module t) "app/hello.js")))
        "relative src, after the style; body still neutralized")
    (ok (string= (freeze 'frozen-demo :args '(:n "5")) (freeze 'frozen-demo :args '(:n "5") :scripts nil))
        ":scripts nil is 0.2's output")
    (ok (signals (freeze 'frozen-demo :scripts '("app/nope.js")) 'canvas-asset-not-found))
    (ok (signals (freeze 'frozen-demo :scripts '("vendor/kit/kit.wasm")) 'canvas-error))))
