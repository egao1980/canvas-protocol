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
;;; 0.2.0 (HX2) adds "canvas-keys.js" (first-party, MIT): the keyboard
;;; bindings behind HX-GET / HX-POST :keys, loaded by (PAGE body :keys t).
;;; Given with the contract, like canvas.css; its rules are in its header.
;;;
;;; App assets (0.3.0, canvas-protocol#15): an application's own browser files
;;; (first-party scripts, a vendored multi-MB JS/WASM toolkit), served by the
;;; same asset route and seen by every function above.
;;; REGISTER-ASSET name &key file system octets content-type sha256 license
;;;   license-file version source integrity -> NAME. DEFINE-ASSET is the same
;;;   call as a toplevel form (evaluated at load time).
;;;   NAME: ASSET-NAME-P (A-Z a-z 0-9 - . _ ~ and /; non-empty segments, none
;;;     "."; no ".." anywhere, so no leading /, no escaping the prefix), and not
;;;     a built-in name; else CANVAS-ERROR. "vendor/kit/kit.wasm" is fine.
;;;   Bytes: exactly one of FILE (a pathname designator, relative to the ASDF
;;;     SYSTEM's directory, or absolute when SYSTEM is NIL; missing ->
;;;     CANVAS-ERROR) or OCTETS (a vector of octets). The file is read when the
;;;     form runs and the bytes stay in the image, like the built-ins.
;;;   SHA256: 64 hex digits (either case). The bytes' SHA-256 is always
;;;     computed (crypto-protocol; STACK-CRYPTO:*CRYPTO-BACKEND*, else
;;;     Ironclad); a given SHA256 that differs -> CANVAS-ASSET-CHECKSUM-MISMATCH
;;;     (name expected actual), nothing registered. ASSET-SHA256 -> the computed hex.
;;;   CONTENT-TYPE: default from NAME's extension (mime-protocol LOOKUP-MIME:
;;;     .js/.mjs text/javascript, .wasm application/wasm, .css, .json, ...;
;;;     unknown application/octet-stream); text/*, application/json,
;;;     application/xml and image/svg+xml get "; charset=utf-8".
;;;   LICENSE (an SPDX id), VERSION, SOURCE, INTEGRITY: strings or NIL, in
;;;     ASSET-INFO. LICENSE-FILE: read like FILE; its text is ASSET-LICENSE.
;;;   Registering a name again replaces it (reloading the app's system).
;;;   App assets survive reloading canvas-protocol (a DEFVAR).
;;; REMOVE-ASSET name -> T if it was an app asset, NIL if unknown; a built-in
;;;   name -> CANVAS-ERROR.
;;; ASSET-OCTETS name -> the held vector itself, not copied (do not modify it):
;;;   servers use it so a multi-MB asset costs no copy per request. ASSET-BYTES
;;;   still returns a fresh copy.

(defparameter +htmx-version+ "2.0.11")
(defparameter +htmx-sse-version+ "2.2.4")

(defvar *canvas-theme* :placeholder
  "Theme served as canvas.css and inlined by FREEZE.")

(defparameter *asset-table*
  `(("canvas-keys.js"
     :file "canvas-keys.js" :version nil :license "MIT" :license-file nil
     :source "canvas-protocol keyboard bindings (0.2.0): data-canvas-keys -> canvas:key events"
     :integrity nil
     :content-type "text/javascript; charset=utf-8")
    ("canvas.css"
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

;;; App assets (0.3.0, canvas-protocol#15). See the file header.

(defvar *app-assets* '()
  "name -> plist (:file :system :version :license :license-octets :source
:integrity :content-type :sha256 :octets), in registration order. A DEFVAR:
reloading canvas-protocol keeps the assets an application registered.")

(defun %builtin-entry (name)
  (and (stringp name) (assoc name *asset-table* :test #'string=)))

(defun %app-entry (name)
  (and (stringp name) (assoc name *app-assets* :test #'string=)))

(defun %asset-entry (name)
  "NAME's (name . plist) entry, built-in or app, or NIL."
  (or (%builtin-entry name) (%app-entry name)))

(defun %asset (name)
  "-> (values plist T) for a known asset; otherwise CANVAS-ASSET-NOT-FOUND with
restart USE-VALUE -> (values value NIL)."
  (let ((entry (%asset-entry name)))
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

(defun asset-name-p (name)
  "True when NAME is a well-formed app asset name: a non-empty string of
A-Z a-z 0-9 - . _ ~ and /, made of non-empty /-separated segments, none of
them \".\", and no \"..\" anywhere (so no leading or trailing /, no //)."
  (and (stringp name)
       (plusp (length name))
       (every (lambda (c) (or (%ascii-alnum-p c) (find c "-._~/"))) name)
       (not (search ".." name))
       (loop for start = 0 then (1+ end)
             for end = (or (position #\/ name :start start) (length name))
             for segment = (subseq name start end)
             always (and (plusp (length segment)) (string/= segment "."))
             while (< end (length name)))))

(defun %text-media-type-p (type)
  (or (%prefix-p "text/" type)
      (member type '("application/json" "application/xml" "image/svg+xml") :test #'string=)))

(defun %default-content-type (name)
  "The content type of the app asset NAME from its extension (mime-protocol's
table: .js text/javascript, .wasm application/wasm, ...; unknown
application/octet-stream), text types with \"; charset=utf-8\"."
  (let ((type (stack-mime:lookup-mime name)))
    (if (%text-media-type-p type) (%str type "; charset=utf-8") type)))

(defvar *asset-crypto-backend* nil
  "The crypto backend that hashes app assets: STACK-CRYPTO:*CRYPTO-BACKEND* when
set, else one Ironclad backend made on first use.")

(defun %octets-sha256 (octets)
  "Lowercase hex SHA-256 of OCTETS."
  (let ((backend (or stack-crypto:*crypto-backend*
                     *asset-crypto-backend*
                     (setf *asset-crypto-backend*
                           (crypto-backend-ironclad:make-ironclad-crypto-backend)))))
    (string-downcase
     (with-output-to-string (out)
       (loop for octet across (stack-crypto:digest octets :algorithm :sha256 :backend backend)
             do (format out "~2,'0X" octet))))))

(defun %sha256-string-p (object)
  (and (stringp object) (= 64 (length object)) (every (lambda (c) (digit-char-p c 16)) object)))

(defun %app-pathname (file system what)
  "FILE (a pathname designator) relative to SYSTEM's directory, or FILE itself
when it is absolute and SYSTEM is NIL. A missing file -> CANVAS-ERROR."
  (unless (typep file '(or string pathname))
    (%canvas-error "asset ~A must be a pathname designator, not ~S" what file))
  (let ((path (cond (system (asdf:system-relative-pathname system file))
                    ((uiop:absolute-pathname-p (pathname file)) (pathname file))
                    (t (%canvas-error "asset ~A ~S is relative: pass :system (the ASDF system it ships in)"
                                      what file)))))
    (or (probe-file path)
        (%canvas-error "asset ~A not found: ~A" what (namestring path)))))

(defun %read-file-octets (path)
  "The bytes of the file PATH as a (simple-array (unsigned-byte 8) (*))."
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let* ((octets (make-array (file-length in) :element-type '(unsigned-byte 8)))
           (n (read-sequence octets in)))
      (if (= n (length octets)) octets (subseq octets 0 n)))))

(defun %simple-octets (octets)
  (unless (and (vectorp octets) (every (lambda (o) (typep o '(unsigned-byte 8))) octets))
    (%canvas-error "asset :octets must be a vector of (unsigned-byte 8)"))
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

(defun register-asset (name &key file system octets content-type sha256
                              license license-file version source integrity)
  "Add (or replace) the app asset NAME, served by the asset route like the
built-in assets. See the file header (App assets). Returns NAME."
  (unless (asset-name-p name)
    (%canvas-error "invalid asset name ~S (A-Z a-z 0-9 - . _ ~~ and /, no .. or leading /)" name))
  (when (%builtin-entry name)
    (%canvas-error "asset name ~S is one of canvas-protocol's own" name))
  (unless (if file (null octets) octets)
    (%canvas-error "register-asset ~S needs exactly one of :file and :octets" name))
  (unless (or (null system) (typep system '(or string symbol asdf:system)))
    (%canvas-error "asset :system must be an ASDF system designator, not ~S" system))
  (dolist (pair `((:content-type . ,content-type) (:license . ,license) (:version . ,version)
                  (:source . ,source) (:integrity . ,integrity)))
    (unless (typep (cdr pair) '(or null string))
      (%canvas-error "asset ~(~S~) must be a string or NIL, not ~S" (car pair) (cdr pair))))
  (unless (or (null sha256) (%sha256-string-p sha256))
    (%canvas-error "asset :sha256 must be 64 hex digits, not ~S" sha256))
  (let* ((bytes (if file
                    (%read-file-octets (%app-pathname file system "file"))
                    (%simple-octets octets)))
         (actual (%octets-sha256 bytes))
         (license-octets (and license-file
                              (%read-file-octets (%app-pathname license-file system "licence file")))))
    (when (and sha256 (string-not-equal sha256 actual))
      (error 'canvas-asset-checksum-mismatch :name name
                                             :expected (string-downcase sha256) :actual actual))
    (let ((plist (list :file (and file (if (pathnamep file) (namestring file) file))
                       :system (and system (asdf:coerce-name system))
                       :version version :license license :license-octets license-octets
                       :source source :integrity integrity
                       :content-type (or content-type (%default-content-type name))
                       :sha256 actual :octets bytes))
          (entry (%app-entry name)))
      (if entry
          (setf (cdr entry) plist)
          (setf *app-assets* (append *app-assets* (list (cons name plist)))))
      name)))

(defmacro define-asset (name &rest options &key &allow-other-keys)
  "Toplevel form for REGISTER-ASSET: (define-asset \"vendor/kit/kit.js\" :system
\"my-app\" :file \"vendor/kit/kit.js\" :sha256 \"…\"). Evaluated when the file
is loaded (not compiled), so the bytes are read then and live in the image."
  `(register-asset ,name ,@options))

(defun remove-asset (name)
  "Remove the app asset NAME. -> T when it was registered, else NIL.
canvas-protocol's own assets cannot be removed (CANVAS-ERROR)."
  (when (%builtin-entry name)
    (%canvas-error "asset ~S is one of canvas-protocol's own" name))
  (let ((entry (%app-entry name)))
    (when entry
      (setf *app-assets* (remove entry *app-assets*))
      t)))

(defgeneric theme-stylesheet (theme)
  (:documentation "Stylesheet octets for THEME (served as canvas.css)."))

(defmethod theme-stylesheet ((theme t))
  (%canvas-error "unknown canvas theme ~S (no THEME-STYLESHEET method)" theme))

(defmethod theme-stylesheet ((theme (eql :placeholder)))
  (copy-seq (%stored-octets "canvas.css")))

(defun asset-names ()
  (sort (append (mapcar #'car *asset-table*) (mapcar #'car *app-assets*)) #'string<))

(defun asset-octets (name)
  "NAME's bytes, not copied: the vector held in the image (do not modify it).
For serving large assets without a copy per request; ASSET-BYTES copies."
  (%with-asset (plist name)
    (cond ((getf plist :octets))
          ((string= name "canvas.css") (theme-stylesheet *canvas-theme*))
          (t (%stored-octets (getf plist :file))))))

(defun asset-bytes (name)
  (%with-asset (plist name)
    (if (string= name "canvas.css")
        (theme-stylesheet *canvas-theme*)
        (copy-seq (or (getf plist :octets) (%stored-octets (getf plist :file)))))))

(defun asset-content-type (name)
  (%with-asset (plist name)
    (getf plist :content-type)))

(defun asset-sha256 (name)
  (%with-asset (plist name)
    (or (getf plist :sha256)
        (cdr (assoc (getf plist :file) *asset-sums* :test #'string=)))))

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
    (let ((octets (or (getf plist :license-octets)
                      (and (getf plist :license-file)
                           (%stored-octets (getf plist :license-file))))))
      (when octets
        (stack-encoding:decode-octets octets :encoding :utf-8)))))

(defun canvas-stylesheet ()
  (stack-encoding:decode-octets (theme-stylesheet *canvas-theme*) :encoding :utf-8))
