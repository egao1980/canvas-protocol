(in-package #:canvas-protocol/tests)

;;; HX2 (0.2.0): FILE-INPUT, multipart forms, COERCE-PARAM :file.
;;; Rules: the header of src/forms.lisp. Parts are parsed with mime-protocol,
;;; the parser rpc-backend-htmx's codec uses; the end-to-end test through
;;; htmx-client needs file parts from the codec first (see the PR).

(defun octets (&rest bytes)
  (coerce bytes '(simple-array (unsigned-byte 8) (*))))

(defun ascii-octets (string)
  (map '(simple-array (unsigned-byte 8) (*)) #'char-code string))

(defun cat-octets (&rest parts)
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (p parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (loop for b across (if (stringp p) (ascii-octets p) p) do (vector-push-extend b out)))))

(defparameter +crlf+ (format nil "~C~C" #\Return #\Linefeed))

(defparameter +svg-upload+ "<svg xmlns=\"http://www.w3.org/2000/svg\"><g id=\"a\"/></svg>")

(defparameter +png-upload+
  (octets #x89 #x50 #x4E #x47 #x0D #x0A #x1A #x0A #x00 #xFF #xFE #x80 #x0D #x0A #x2D #x2D)
  "Binary content: not UTF-8, with CR LF and \"--\" inside.")

(defun browser-form-body ()
  "multipart/form-data octets shaped as a browser sends them, with a header
block naming the boundary prepended (what rpc-backend-htmx's codec feeds to
STACK-MIME:PARSE-MIME)."
  (flet ((part (headers content)
           (list "--XyZzY" +crlf+ headers +crlf+ +crlf+ content +crlf+)))
    (apply #'cat-octets
           (append
            (list "Content-Type: multipart/form-data; boundary=XyZzY" +crlf+ +crlf+)
            (part "Content-Disposition: form-data; name=\"title\"" "My score")
            (part (s+ "Content-Disposition: form-data; name=\"score\"; filename=\"scores/Prelude in C.svg\""
                      +crlf+ "Content-Type: image/svg+xml")
                  +svg-upload+)
            (part (s+ "Content-Disposition: form-data; name=\"cover\"; filename=\"cover.PNG\""
                      +crlf+ "Content-Type: image/png")
                  +png-upload+)
            (part (s+ "Content-Disposition: form-data; name=\"notes\"; filename=\"notes\""
                      +crlf+ "Content-Type: text/plain; charset=utf-8")
                  "plain")
            (list "--XyZzY--" +crlf+)))))

(defun browser-parts ()
  "name -> parsed part (a STACK-MIME:MIME-ENTITY)."
  (loop for part in (stack-mime:mime-parts (stack-mime:parse-mime (browser-form-body)))
        collect (cons (stack-mime:disposition-name (stack-mime:mime-content-disposition part)) part)))

(defun part (name) (cdr (assoc name (browser-parts) :test #'string=)))

(defun upload-error (value &rest keys)
  (handler-case (apply #'coerce-param value :file keys)
    (canvas-coercion-error (c) c)))

(defun empty-file-part ()
  "What a browser sends for a file input left empty: filename=\"\", no content.
Built with mime-protocol rather than parsed: mime-protocol 0.2.0 cannot split a
part whose content is empty or ends in CR LF (see the PR)."
  (file-part "" (octets)))

(defun file-part (filename octets &key (type "application") (subtype "octet-stream"))
  "A part built with mime-protocol, for file names a browser would not send."
  (stack-mime:make-binary-entity octets :type type :subtype subtype :disposition-type "form-data"
                                        :name "f" :filename filename))

;;; The value

(deftest uploaded-file-value
  (let ((f (make-uploaded-file :filename "a.svg" :media-type "image/svg+xml" :size 3
                               :octets (octets 1 2 3))))
    (ok (uploaded-file-p f))
    (ok (string= "a.svg" (uploaded-file-filename f)))
    (ok (string= "image/svg+xml" (uploaded-file-media-type f)))
    (ok (= 3 (uploaded-file-size f)))
    (ok (equalp (octets 1 2 3) (uploaded-file-octets f)))
    (ok (null (uploaded-file-path f))))
  (let ((f (make-uploaded-file)))
    (ok (string= "application/octet-stream" (uploaded-file-media-type f)))
    (ok (= 0 (uploaded-file-size f)))
    (ok (null (uploaded-file-filename f))))
  (ok (member :file +coercion-types+)))

(deftest upload-condition-text
  (ok (string= "score must be a file"
               (princ-to-string (make-condition 'canvas-coercion-error :field "score" :expected :file))))
  (ok (string= "score must be a file of type: image/svg+xml, .svg"
               (princ-to-string (make-condition 'canvas-coercion-error :field "score" :expected :accept
                                                                       :detail '("image/svg+xml" ".svg")))))
  (let ((c (make-condition 'canvas-upload-too-large :field "cover" :detail 1048576 :size 2000000)))
    (ok (typep c 'canvas-coercion-error))
    (ok (eq :max-size (coercion-error-expected c)))
    (ok (= 2000000 (upload-error-size c)))
    (ok (string= "cover must be at most 1048576 bytes" (princ-to-string c)))))

;;; FILE-INPUT and the form

(deftest file-input-html
  (ok (string= "<label>Score<input type=\"file\" name=\"score\" accept=\".svg,image/svg+xml\" required=\"\"></label>"
               (html (file-input "score" :label "Score" :accept '(".svg" "image/svg+xml") :required t))))
  (ok (string= "<label><input type=\"file\" name=\"pics\" accept=\"image/*,.png\" multiple=\"\"></label>"
               (html (file-input "pics" :accept " image/* , .png" :multiple t))))
  (ok (string= "<label><input type=\"file\" name=\"f\"></label>" (html (file-input "f"))))
  (ok (string= "<label><input id=\"f1\" type=\"file\" name=\"f\" accept=\".tar.gz,application/x-musicxml+xml\" capture=\"environment\"></label>"
               (html (file-input "f" :id "f1" :accept '(".tar.gz" "application/x-musicxml+xml")
                                     :attrs '(:capture "environment")))))
  (let ((err (make-condition 'canvas-coercion-error :field "score" :value "x" :expected :file)))
    (ok (string= (s+ "<label>Score<input type=\"file\" name=\"score\" aria-invalid=\"true\" aria-describedby=\"score-error\">"
                     "<small id=\"score-error\" role=\"alert\">score must be a file</small></label>")
                 (html (file-input "score" :label "Score" :error err))))))

(deftest file-input-refusals
  (dolist (accept (list "svg" "image/" "/svg" "*/*" "image/svg+xml;charset=utf-8" "." ".s v g"
                        "" "a,,b" '("image/*" "") '(".svg" 3) 42))
    (ok (signals (file-input "f" :accept accept) 'canvas-error) (format nil "~S" accept)))
  (ok (signals (file-input "bad name") 'canvas-error))
  (ok (signals (file-input "f" :error "oops") 'canvas-error))
  (ok (signals (file-input "f" :attrs '(:onchange "x()")) 'canvas-csp-error)))

(deftest form-multipart-enctype
  (ok (string= (s+ "<form id=\"up\" hx-post=\"" +u+ "SAVE\" hx-target=\"#up\" hx-swap=\"outerHTML\""
                   " enctype=\"multipart/form-data\"><label><input type=\"text\" name=\"title\"></label>"
                   "<label><input type=\"file\" name=\"score\"></label><button type=\"submit\">Upload</button></form>")
               (html (form (list (text-input "title") (file-input "score") (submit-button "Upload"))
                           :link (canvas-link 'save) :id "up"))))
  (ok (string= (s+ "<form enctype=\"multipart/form-data\" hx-sync=\"this:replace\"><fieldset>"
                   "<label><input type=\"file\" name=\"a\"></label></fieldset></form>")
               (html (form (fieldset (file-input "a")) :attrs '(:hx-sync "this:replace"))))
      "nested, no link, caller attrs last")
  (ok (string= "<form enctype=\"multipart/form-data\"><div><input type=\"FILE\" name=\"x\"></div></form>"
               (html (form (raw "div" nil (raw "input" '(("type" . "FILE") ("name" . "x")))))))
      "a raw file input counts")
  (ok (string= "<form id=\"f\" enctype=\"multipart/form-data\"><label><input type=\"file\" name=\"a\"></label></form>"
               (let ((*canvas-resolver* (make-freeze-resolver)))
                 (to-html (form (file-input "a") :link (canvas-link 'save) :id "f"))))
      "the link resolves to NIL")
  (ok (string= "<form><label><input type=\"text\" name=\"a\"></label></form>" (html (form (text-input "a"))))
      "no file input, no enctype"))

;;; COERCE-PARAM :file

(deftest coerce-file-from-browser-parts
  (let ((score (coerce-param (part "score") :file :field "score"))
        (cover (coerce-param (part "cover") :file :field "cover"))
        (notes (coerce-param (part "notes") :file :field "notes")))
    (ok (uploaded-file-p score))
    (ok (string= "Prelude in C.svg" (uploaded-file-filename score)) "last path segment")
    (ok (string= "image/svg+xml" (uploaded-file-media-type score)))
    (ok (= (length +svg-upload+) (uploaded-file-size score)))
    (ok (equalp (ascii-octets +svg-upload+) (uploaded-file-octets score)))
    (ok (typep (uploaded-file-octets score) '(simple-array (unsigned-byte 8) (*))))
    (ok (null (uploaded-file-path score)))
    (ok (string= "cover.PNG" (uploaded-file-filename cover)))
    (ok (string= "image/png" (uploaded-file-media-type cover)))
    (ok (equalp +png-upload+ (uploaded-file-octets cover)) "binary content intact")
    (ok (= (length +png-upload+) (uploaded-file-size cover)))
    (ok (string= "text/plain" (uploaded-file-media-type notes)) "parameters dropped")
    ;; The parsed SVG upload goes straight into SVG-FRAGMENT.
    (ok (string= "<svg><g id=\"a\"></g></svg>"
                 (to-html (svg-fragment (uploaded-file-octets score)))))))

(deftest coerce-file-names-and-types
  (flet ((name-of (filename)
           (uploaded-file-filename (coerce-param (file-part filename (octets 1)) :file))))
    (ok (string= "x.svg" (name-of "C:\\fakepath\\x.svg")))
    (ok (string= "passwd" (name-of "../../etc/passwd")))
    (ok (string= "evilname.svg" (name-of (format nil "evil~Cname~C.svg" (code-char 7) (code-char 127)))))
    (ok (null (name-of "dir/")) "nothing left -> NIL"))
  (let ((f (coerce-param (file-part "a.bin" (octets 1 2) :type "Application" :subtype "X-Thing") :file)))
    (ok (string= "application/x-thing" (uploaded-file-media-type f))))
  (let ((no-type (make-instance 'stack-mime:mime-entity
                                :content (octets 9)
                                :content-disposition (stack-mime:make-content-disposition
                                                      "form-data" '(("name" . "f") ("filename" . "a"))))))
    (ok (string= "application/octet-stream"
                 (uploaded-file-media-type (coerce-param no-type :file))))))

(deftest coerce-file-missing-and-not-a-file
  (ok (eq :none (coerce-param (empty-file-part) :file :default :none)) "browser's empty file input")
  (ok (eq :none (coerce-param nil :file :default :none)))
  (ok (eq :none (coerce-param "" :file :default :none)))
  (let ((c (upload-error (empty-file-part) :field "extra" :required t)))
    (ok (eq :required (coercion-error-expected c)))
    (ok (string= "extra is required" (princ-to-string c))))
  (let ((c (upload-error (part "title") :field "title")))
    (ok (eq :file (coercion-error-expected c)) "a text part is not a file")
    (ok (string= "title must be a file" (princ-to-string c))))
  (ok (eq :file (coercion-error-expected (upload-error "abc"))))
  (ok (eq :file (coercion-error-expected (upload-error (list (part "score"))))) "a list without :multiple")
  (let ((f (make-uploaded-file :filename "a" :size 1 :octets (octets 1))))
    (ok (eq f (coerce-param f :file)) "an uploaded-file is taken as it is"))
  (dolist (bad (list 42 :file (octets 1 2)))
    (let ((c (handler-case (coerce-param bad :file) (error (c) c))))
      (ok (typep c 'canvas-error) (format nil "~S" bad))
      (ok (not (typep c 'canvas-coercion-error)) (format nil "~S is a server bug" bad)))))

(deftest coerce-file-size-limit
  (let ((n (length +png-upload+)))
    (ok (uploaded-file-p (coerce-param (part "cover") :file :max-size n)) "at the limit")
    (let ((c (upload-error (part "cover") :field "cover" :max-size 4)))
      (ok (typep c 'canvas-upload-too-large))
      (ok (eq :max-size (coercion-error-expected c)))
      (ok (= 4 (coercion-error-detail c)))
      (ok (= n (upload-error-size c)))
      (ok (string= "cover must be at most 4 bytes" (princ-to-string c)))))
  (let ((spooled (make-uploaded-file :filename "big.svg" :size 10 :path #p"/tmp/spool-1")))
    (ok (typep (upload-error spooled :max-size 5) 'canvas-upload-too-large) "SIZE, not OCTETS, counts"))
  (dolist (bad '(-1 "10" 1.5))
    (let ((c (handler-case (coerce-param (part "cover") :file :max-size bad) (error (c) c))))
      (ok (and (typep c 'canvas-error) (not (typep c 'canvas-coercion-error))) (format nil "~S" bad)))))

(deftest coerce-file-restarts
  (let ((v (handler-bind ((canvas-upload-too-large
                            (lambda (c) (declare (ignore c)) (invoke-restart 'use-value :too-big))))
             (coerce-param (part "cover") :file :max-size 1))))
    (ok (eq :too-big v)))
  (let ((v (handler-bind ((canvas-coercion-error
                            (lambda (c) (declare (ignore c)) (invoke-restart 'use-default))))
             (coerce-param (part "title") :file :default :fallback))))
    (ok (eq :fallback v)))
  (let ((names '()))
    (handler-case
        (handler-bind ((canvas-coercion-error
                         (lambda (c) (setf names (mapcar #'restart-name (compute-restarts c))))))
          (coerce-param (part "cover") :file :max-size 1))
      (canvas-coercion-error () nil))
    (ok (member 'use-value names))
    (ok (member 'use-default names))))

(deftest coerce-file-accept
  (dolist (accept (list ".svg" ".SVG" "image/*" "IMAGE/SVG+XML" "image/svg+xml" '(".png" "image/svg+xml")
                        "text/plain, .svg"))
    (ok (uploaded-file-p (coerce-param (part "score") :file :accept accept)) (format nil "~S" accept)))
  (ok (uploaded-file-p (coerce-param (part "cover") :file :accept ".png")) "extension, any case")
  (let ((c (upload-error (part "score") :field "score" :accept ".png, image/jpeg")))
    (ok (eq :accept (coercion-error-expected c)))
    (ok (equal '(".png" "image/jpeg") (coercion-error-detail c)))
    (ok (string= "score must be a file of type: .png, image/jpeg" (princ-to-string c))))
  (ok (eq :accept (coercion-error-expected
                   (upload-error (make-uploaded-file :size 1 :octets (octets 1)) :accept ".bin")))
      "a file without a name matches no extension")
  (ok (eq :accept (coercion-error-expected (upload-error (part "cover") :accept "image/svg+xml"))))
  ;; Too large is checked before the type.
  (ok (typep (upload-error (part "cover") :max-size 1 :accept "text/plain") 'canvas-upload-too-large))
  (dolist (bad (list "svg" "*/*" "image/" '("image/*" 3)))
    (let ((c (handler-case (coerce-param nil :file :accept bad) (error (c) c))))
      (ok (and (typep c 'canvas-error) (not (typep c 'canvas-coercion-error)))
          (format nil "invalid accept ~S, even for a missing value" bad)))))

(defun part-value-of (condition)
  "Which browser part a coercion error's :value is: :score, :cover, :title or NIL."
  (let ((value (coercion-error-value condition)))
    (when (typep value 'stack-mime:mime-entity)
      (let ((name (stack-mime:disposition-name (stack-mime:mime-content-disposition value))))
        (and name (intern (string-upcase name) :keyword))))))

(deftest coerce-file-multiple
  (let ((files (coerce-param (list (part "score") (empty-file-part) nil "" (part "cover")) :file
                             :multiple t)))
    (ok (= 2 (length files)) "missing items dropped")
    (ok (equal '("Prelude in C.svg" "cover.PNG") (mapcar #'uploaded-file-filename files))))
  (let ((files (coerce-param (part "score") :file :multiple t)))
    (ok (and (= 1 (length files)) (uploaded-file-p (first files))) "one item -> a list of one"))
  (ok (eq :none (coerce-param (list (empty-file-part) nil) :file :multiple t :default :none)))
  (ok (eq :required (coercion-error-expected (upload-error (list (empty-file-part)) :multiple t :required t))))
  (let ((c (upload-error (list (part "cover") (part "score") (part "title")) :multiple t :max-size 30)))
    (ok (typep c 'canvas-upload-too-large) "the first failure, in order")
    (ok (eq :score (part-value-of c)) "its :value is that item"))
  (let ((c (upload-error (list (part "score") (part "title")) :multiple t)))
    (ok (eq :file (coercion-error-expected c)))))

(deftest coerce-form-with-uploads
  (multiple-value-bind (plist errors)
      (coerce-form (list :title "My score" :score (part "score") :cover (part "cover") :extra (empty-file-part))
                   '((:title :string :required t)
                     (:score :file :required t :max-size 100000 :accept "image/svg+xml,.svg")
                     (:cover :file :accept ".svg")
                     (:extra :file :default :none)))
    (ok (string= "My score" (getf plist :title)))
    (ok (uploaded-file-p (getf plist :score)))
    (ok (null (getf plist :cover)) "the error's USE-DEFAULT")
    (ok (eq :none (getf plist :extra)))
    (ok (= 1 (length errors)))
    (ok (string= "<small id=\"cover-error\" role=\"alert\">cover must be a file of type: .svg</small>"
                 (html (error-fragment (find-field-error errors "cover")))))
    (ok (string= (s+ "<label>Cover<input type=\"file\" name=\"cover\" accept=\".svg\" aria-invalid=\"true\""
                     " aria-describedby=\"cover-error\"><small id=\"cover-error\" role=\"alert\">"
                     "cover must be a file of type: .svg</small></label>")
                 (html (file-input "cover" :label "Cover" :accept ".svg"
                                           :error (find-field-error errors "cover")))))))
