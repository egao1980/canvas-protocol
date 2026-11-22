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

(defun %read-octets (file)
  "The bytes of assets/FILE as a (simple-array (unsigned-byte 8) (*))."
  (with-open-file (in (%asset-pathname file) :element-type '(unsigned-byte 8))
    (let* ((octets (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (n (read-sequence octets in)))
      (if (= n (length octets)) octets (subseq octets 0 n)))))

(defun %read-asset-files ()
  "file -> octets for every asset and licence file named in *ASSET-TABLE*."
  (loop for (nil . plist) in *asset-table*
        collect (cons (getf plist :file) (%read-octets (getf plist :file)))
        when (getf plist :license-file)
          collect (cons (getf plist :license-file) (%read-octets (getf plist :license-file)))))

(defun %parse-sums (octets)
  "assets/SHA256SUMS (\"<hex>  <file>\" lines) -> alist file -> lowercase hex."
  (let ((text (map 'string #'code-char octets)))
    (loop for start = 0 then (1+ end)
          for end = (or (position #\Newline text :start start) (length text))
          for line = (string-right-trim '(#\Return) (subseq text start end))
          when (> (length line) 66)
            collect (cons (string-trim " *" (subseq line 64))
                          (string-downcase (subseq line 0 64)))
          while (< end (length text)))))

;;; Read once, when this file is loaded (not at compile or read time): the
;;; octets live in the image, so a saved image needs no source tree.
(defparameter *asset-files* (%read-asset-files)
  "file -> octets, read from assets/ at load time.")

(defparameter *asset-sums* (%parse-sums (%read-octets "SHA256SUMS"))
  "file -> SHA-256 hex, as recorded in assets/SHA256SUMS.")

(defun %stored-octets (file)
  (cdr (assoc file *asset-files* :test #'string=)))

(defun %asset (name)
  "-> (values plist T) for a known asset; otherwise CANVAS-ASSET-NOT-FOUND with
restart USE-VALUE -> (values value NIL)."
  (let ((entry (and (stringp name) (assoc name *asset-table* :test #'string=))))
    (if entry
        (values (cdr entry) t)
        (restart-case (error 'canvas-asset-not-found :name name)
          (use-value (value)
            :report "Return a value instead of the missing asset's."
            (values value nil))))))

(defmacro %with-asset ((plist name) &body body)
  "Run BODY with PLIST bound to NAME's entry; a USE-VALUE value is returned as is."
  (let ((found (gensym "FOUND")))
    `(multiple-value-bind (,plist ,found) (%asset ,name)
       (if ,found (progn ,@body) ,plist))))

(defgeneric theme-stylesheet (theme)
  (:documentation "Stylesheet octets for THEME (served as canvas.css)."))

(defmethod theme-stylesheet ((theme t))
  (%canvas-error "unknown canvas theme ~S (no THEME-STYLESHEET method)" theme))

(defmethod theme-stylesheet ((theme (eql :placeholder)))
  (copy-seq (%stored-octets "canvas.css")))

(defun asset-names ()
  (sort (mapcar #'car *asset-table*) #'string<))

(defun asset-bytes (name)
  (%with-asset (plist name)
    (if (string= name "canvas.css")
        (theme-stylesheet *canvas-theme*)
        (copy-seq (%stored-octets (getf plist :file))))))

(defun asset-content-type (name)
  (%with-asset (plist name)
    (getf plist :content-type)))

(defun asset-sha256 (name)
  (%with-asset (plist name)
    (cdr (assoc (getf plist :file) *asset-sums* :test #'string=))))

(defun asset-info (name)
  (%with-asset (plist name)
    (list :name name
          :file (getf plist :file)
          :version (getf plist :version)
          :license (getf plist :license)
          :source (getf plist :source)
          :integrity (getf plist :integrity)
          :content-type (getf plist :content-type)
          :sha256 (asset-sha256 name))))

(defun asset-license (name)
  (%with-asset (plist name)
    (let ((file (getf plist :license-file)))
      (when file
        (stack-encoding:decode-octets (%stored-octets file) :encoding :utf-8)))))

(defun canvas-stylesheet ()
  (stack-encoding:decode-octets (theme-stylesheet *canvas-theme*) :encoding :utf-8))
