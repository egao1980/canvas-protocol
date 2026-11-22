(in-package #:canvas-protocol)

;;; Vendored assets (assets/, see assets/README.md). No CDN, ever.
;;;
;;; ASSET-NAMES -> the names, sorted (string<).
;;; ASSET-BYTES name -> (simple-array (unsigned-byte 8) (*)). The files are read
;;;   once at load time and kept in the image (a saved image needs no source
;;;   tree). "canvas.css" is (THEME-STYLESHEET *CANVAS-THEME*).
;;; ASSET-CONTENT-TYPE, ASSET-LICENSE (text of the licence file, or NIL),
;;; ASSET-INFO -> plist (:name :file :version :license :source :integrity
;;;   :content-type :sha256), ASSET-SHA256 -> the lowercase hex recorded in
;;;   assets/SHA256SUMS.
;;; An unknown name -> CANVAS-ASSET-NOT-FOUND (restart USE-VALUE: octets/value to return).
;;; THEME-STYLESHEET theme -> octets; method (eql :placeholder) = assets/canvas.css;
;;;   an unknown theme -> CANVAS-ERROR. H4h-0b adds (eql :engels) and makes it the default.
;;; CANVAS-STYLESHEET -> (THEME-STYLESHEET *CANVAS-THEME*) decoded as UTF-8.

(defparameter +htmx-version+ "2.0.11")
(defparameter +htmx-sse-version+ "2.2.4")

(defvar *canvas-theme* :placeholder
  "Theme served as canvas.css and inlined by FREEZE.")

(defparameter *asset-table*
  `(("canvas.css"
     :file "canvas.css" :version nil :license "MIT" :license-file nil
     :source "canvas-protocol placeholder theme"
     :integrity nil
     :content-type "text/css; charset=utf-8")
    ("htmx-ext-sse.min.js"
     :file "htmx-ext-sse.min.js" :version ,+htmx-sse-version+ :license "0BSD"
     :license-file "LICENSE.htmx-ext-sse"
     :source "https://registry.npmjs.org/htmx-ext-sse/-/htmx-ext-sse-2.2.4.tgz"
     :integrity "sha512-LJmxVhykyflBWgh5PvbRidcyuqMHlgfajmmzumvKctv9puvsufeH6OaejSMZTNTnEI8O2wXXn4ZZtBdpEMqmEQ=="
     :content-type "text/javascript; charset=utf-8")
    ("htmx.min.js"
     :file "htmx.min.js" :version ,+htmx-version+ :license "0BSD"
     :license-file "LICENSE.htmx"
     :source "https://registry.npmjs.org/htmx.org/-/htmx.org-2.0.11.tgz"
     :integrity "sha512-Thx/WtpeOQqSrqBCw/A1cwGJGg4UrVa3+sW0GmrM3p4gJgO89ecH4qtbnyzDDWFvBTqjnIMCgELTNt636dtamA=="
     :content-type "text/javascript; charset=utf-8"))
  "name -> plist. :sha256 comes from assets/SHA256SUMS.")

(defun %asset-pathname (file)
  "Absolute pathname of FILE in this system's assets/ directory."
  (asdf:system-relative-pathname "canvas-protocol" (concatenate 'string "assets/" file)))

(defgeneric theme-stylesheet (theme)
  (:documentation "Stylesheet octets for THEME (served as canvas.css)."))

(defmethod theme-stylesheet ((theme t))
  (%h4h0-pending 'theme-stylesheet))

(defun asset-names ()
  (%h4h0-pending 'asset-names))

(defun asset-bytes (name)
  (declare (ignore name))
  (%h4h0-pending 'asset-bytes))

(defun asset-content-type (name)
  (declare (ignore name))
  (%h4h0-pending 'asset-content-type))

(defun asset-sha256 (name)
  (declare (ignore name))
  (%h4h0-pending 'asset-sha256))

(defun asset-info (name)
  (declare (ignore name))
  (%h4h0-pending 'asset-info))

(defun asset-license (name)
  (declare (ignore name))
  (%h4h0-pending 'asset-license))

(defun canvas-stylesheet ()
  (%h4h0-pending 'canvas-stylesheet))
