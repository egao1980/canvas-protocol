(in-package #:canvas-protocol/tests)

(defun ascii-prefix (octets n)
  (map 'string #'code-char (subseq octets 0 n)))

(defun recorded-sums ()
  "assets/SHA256SUMS as an alist file -> hex, read independently of the library."
  (with-open-file (in (asdf:system-relative-pathname "canvas-protocol" "assets/SHA256SUMS"))
    (loop for line = (read-line in nil)
          while line
          when (> (length line) 66)
            collect (cons (string-trim " *" (subseq line 64)) (subseq line 0 64)))))

(deftest asset-catalogue
  (ok (equal '("canvas-keys.js" "canvas.css" "htmx-ext-sse.min.js" "htmx.min.js") (asset-names)))
  (ok (string= "text/css; charset=utf-8" (asset-content-type "canvas.css")))
  (ok (string= "text/javascript; charset=utf-8" (asset-content-type "canvas-keys.js")))
  (ok (string= "text/javascript; charset=utf-8" (asset-content-type "htmx.min.js")))
  (ok (string= "text/javascript; charset=utf-8" (asset-content-type "htmx-ext-sse.min.js")))
  (ok (string= "2.0.11" +htmx-version+))
  (ok (string= "2.2.4" +htmx-sse-version+)))

(deftest asset-info-plists
  (let ((i (asset-info "htmx.min.js")))
    (ok (string= "htmx.min.js" (getf i :name)))
    (ok (string= "2.0.11" (getf i :version)))
    (ok (string= "0BSD" (getf i :license)))
    (ok (string= "https://registry.npmjs.org/htmx.org/-/htmx.org-2.0.11.tgz" (getf i :source)))
    (ok (string= "sha512-Thx/WtpeOQqSrqBCw/A1cwGJGg4UrVa3+sW0GmrM3p4gJgO89ecH4qtbnyzDDWFvBTqjnIMCgELTNt636dtamA=="
                 (getf i :integrity)))
    (ok (string= (asset-sha256 "htmx.min.js") (getf i :sha256)))
    (ok (string= "text/javascript; charset=utf-8" (getf i :content-type))))
  (let ((i (asset-info "htmx-ext-sse.min.js")))
    (ok (string= "2.2.4" (getf i :version)))
    (ok (string= "0BSD" (getf i :license)))))

(deftest asset-bytes-content
  (let ((htmx (asset-bytes "htmx.min.js"))
        (sse (asset-bytes "htmx-ext-sse.min.js")))
    (ok (typep htmx '(simple-array (unsigned-byte 8) (*))))
    (ok (= 52182 (length htmx)))
    (ok (string= "var htmx=function(){" (ascii-prefix htmx 20)))
    (ok (= 2853 (length sse)))
    (ok (string= "(function(){var g;htmx.defineExtension(\"sse\"" (ascii-prefix sse 44)))
    (ok (equalp htmx (asset-bytes "htmx.min.js")))))

(deftest asset-checksums
  (ok (string= "d6fdc75f204e6bdefa99b69bf1e6d4ac69b8a364f77929f45c13476b4000f717"
               (asset-sha256 "htmx.min.js")))
  (ok (string= "98a46496de0c3605fbffdce9167ba427bdd9553184f83f149c261891a92c0136"
               (asset-sha256 "htmx-ext-sse.min.js")))
  (dolist (name '("canvas-keys.js" "canvas.css" "htmx-ext-sse.min.js" "htmx.min.js"))
    (ok (string= (sha256-hex (asset-bytes name)) (asset-sha256 name)) (format nil "computed ~A" name))
    (ok (string= (cdr (assoc name (recorded-sums) :test #'string=)) (asset-sha256 name))
        (format nil "SHA256SUMS ~A" name))))

(deftest asset-licenses
  (ok (contains "Zero-Clause BSD" (asset-license "htmx.min.js")))
  (ok (contains "BSD Zero Clause License" (asset-license "htmx-ext-sse.min.js")))
  (ok (null (asset-license "canvas.css")))
  (ok (null (asset-license "canvas-keys.js"))))

(deftest asset-unknown
  (dolist (name '("evil/../x" "../canvas-protocol.asd" "SHA256SUMS" "LICENSE.htmx" ""))
    (let ((c (handler-case (asset-bytes name) (canvas-asset-not-found (c) c))))
      (ok (typep c 'canvas-asset-not-found) (format nil "~S" name))
      (ok (and c (string= name (asset-error-name c))))))
  (ok (signals (asset-content-type "nope.js") 'canvas-asset-not-found))
  (ok (signals (asset-sha256 "nope.js") 'canvas-asset-not-found))
  (ok (equalp #(1 2) (handler-bind ((canvas-asset-not-found
                                      (lambda (c) (declare (ignore c))
                                        (invoke-restart 'use-value #(1 2)))))
                       (asset-bytes "nope.js")))))

(defmethod theme-stylesheet ((theme (eql :test-theme)))
  (coerce (map 'vector #'char-code "p{}") '(simple-array (unsigned-byte 8) (*))))

(deftest theme-hook
  (ok (eq :placeholder *canvas-theme*))
  (ok (equalp (theme-stylesheet :placeholder) (asset-bytes "canvas.css")))
  (ok (signals (theme-stylesheet :no-such-theme) 'canvas-error))
  (let ((*canvas-theme* :test-theme))
    (ok (equalp (theme-stylesheet :test-theme) (asset-bytes "canvas.css")))
    (ok (string= "p{}" (canvas-stylesheet)))
    (ok (contains "<style>p{}</style>" (freeze 'frozen-demo)))))

(deftest placeholder-stylesheet
  (let ((css (canvas-stylesheet)))
    (ok (stringp css))
    (ok (contains "prefers-color-scheme: dark" css))
    (dolist (sel '("[data-layout" "[data-gap" "[data-columns" "[data-align" "[data-badge"
                   "[aria-sort" "[aria-invalid" "[role=\"tablist\"]" "figure[data-chart]" "table"))
      (ok (contains sel css) sel))
    (ok (not (contains "url(" css)))
    (ok (not (contains "@import" css)))))
