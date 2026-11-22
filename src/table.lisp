(in-package #:canvas-protocol)

;;; Tables.
;;;
;;; COLUMN header accessor &key name sortable format align
;;; - ACCESSOR: a function of the row, or a keyword K meaning (getf row K).
;;; - NAME: the sort param value (string); required when SORTABLE.
;;; - FORMAT: function value -> renderable; default: NIL -> "", string as is,
;;;   else PRINC-TO-STRING.
;;; - ALIGN: NIL, :start :center :end -> data-align on th and td.
;;;
;;; TABLE rows columns &key id link sort page page-size caption (empty "No rows") attrs
;;; - ROWS a list or vector. SORT / PAGE as received (strings) or NIL; invalid
;;;   values are ignored, never an error (unknown sort -> unsorted, bad page -> 1,
;;;   page beyond the end -> the last page).
;;; - LINK (a CANVAS-LINK) needs ID (else CANVAS-ERROR). PAGE-SIZE a positive
;;;   integer or NIL (no paging, no pager).
;;; - Output, without ID: the <table> alone. With ID: <div id="ID"><table>...</table>[pager]</div>.
;;;   <table>[<caption>c</caption>]<thead><tr><th scope="col" [data-align] [aria-sort]>...</th>...</tr></thead>
;;;   <tbody><tr><td [data-align]>formatted</td>...</tr>...</tbody></table>
;;;   Empty: <tbody><tr><td colspan="N"><em>EMPTY</em></td></tr></tbody>.
;;;   aria-sort="ascending|descending" on the column of a valid SORT.
;;;   Sortable header with LINK: <button type="button" hx-get="<link + sort=S, page=1>"
;;;   hx-target="#ID" hx-swap="outerHTML">header</button>, S = "-name" when the
;;;   column is the current ascending sort, else "name". Without LINK: plain text.
;;;   Pager (PAGE-SIZE given): <nav aria-label="Pages"><button type="button"
;;;   [disabled=""] | [hx-get=... hx-target hx-swap]>Previous</button><span>Page P of N</span>
;;;   <button ...>Next</button></nav>; page links carry the current valid sort
;;;   (sort=... then page=...). A bound button gets disabled="" and no hx attributes.
;;;   Link params come first in every URL (LINK-URL merge).
;;;
;;; SORT-ROWS rows columns sort -> fresh sequence of the same type, stably
;;; sorted by the column's accessor value with VALUE< ("-name" descending; ties
;;; keep input order in both directions). Unknown / non-sortable / NIL sort ->
;;; a fresh copy in input order. ROWS is never modified.
;;; PAGE-ROWS rows page page-size -> (values subsequence page page-count);
;;; PAGE an integer, a string or NIL (invalid -> 1), clamped to 1..page-count;
;;; page-count = max(1, ceiling(length / page-size)); PAGE-SIZE not a positive
;;; integer -> CANVAS-ERROR. Same sequence type as ROWS.
;;; PARSE-SORT sort columns -> (values column descending-p), or NIL.
;;; VALUE< a b: NIL before everything; reals by <; reals before strings;
;;; strings by STRING<; otherwise by PRINC-TO-STRING with STRING<.

(defclass table-column ()
  ((header :initarg :header :reader column-header)
   (accessor :initarg :accessor :reader column-accessor)
   (name :initarg :name :initform nil :reader column-name)
   (sortable :initarg :sortable :initform nil :reader column-sortable-p)
   (format :initarg :format :initform nil :reader column-format)
   (align :initarg :align :initform nil :reader column-align)))

(defgeneric validate-column (column)
  (:documentation "CANVAS-ERROR when COLUMN's options are invalid; return it."))

(defmethod validate-column ((column table-column))
  column)

(defun column (header accessor &key name sortable format align)
  (validate-column
   (make-instance 'table-column :header header :accessor accessor :name name
                                :sortable sortable :format format :align align)))

(defclass table (component)
  ((rows :initarg :rows :reader table-rows)
   (columns :initarg :columns :reader table-columns)
   (link :initarg :link :initform nil :reader table-link)
   (sort :initarg :sort :initform nil :reader table-sort)
   (page :initarg :page :initform nil :reader table-page)
   (page-size :initarg :page-size :initform nil :reader table-page-size)
   (caption :initarg :caption :initform nil :reader table-caption)
   (empty :initarg :empty :initform "No rows" :reader table-empty)))

(defun table (rows columns &key id link sort page page-size caption (empty "No rows") attrs)
  (check-type rows sequence)
  (check-type columns list)
  (%make 'table :rows rows :columns columns :id id :link link :sort sort :page page
                :page-size page-size :caption caption :empty empty :attrs attrs))

(defmethod validate-column :before ((column table-column))
  (let ((accessor (column-accessor column))
        (name (column-name column)))
    (unless (or (functionp accessor) (keywordp accessor))
      (%canvas-error "column accessor must be a function or a keyword: ~S" accessor))
    (unless (member (column-align column) '(nil :start :center :end))
      (%canvas-error "column align must be NIL, :start, :center or :end: ~S" (column-align column)))
    (unless (or (null name) (and (stringp name) (plusp (length name))))
      (%canvas-error "column name must be a non-empty string: ~S" name))
    (when (and (column-sortable-p column) (null name))
      (%canvas-error "a sortable column needs a :name (the sort param value)"))
    (unless (or (null (column-format column)) (functionp (column-format column)))
      (%canvas-error "column format must be a function: ~S" (column-format column)))))

(defmethod validate-component ((c table))
  (let ((link (table-link c))
        (size (table-page-size c)))
    (unless (every (lambda (col) (typep col 'table-column)) (table-columns c))
      (%canvas-error "table columns must be made by COLUMN: ~S" (table-columns c)))
    (unless (or (null link) (canvas-link-p link))
      (%canvas-error "table :link must be a canvas-link: ~S" link))
    (when (and link (null (component-id c)))
      (%canvas-error "a table with :link needs an :id (the hx-target)"))
    (unless (or (null size) (typep size '(integer 1)))
      (%canvas-error "page-size must be a positive integer or NIL: ~S" size)))
  (call-next-method))

(defun value< (a b)
  (cond ((null a) (not (null b)))
        ((null b) nil)
        ((and (realp a) (realp b)) (< a b))
        ((realp a) t)
        ((realp b) nil)
        ((and (stringp a) (stringp b)) (and (string< a b) t))
        (t (and (string< (princ-to-string a) (princ-to-string b)) t))))

(defun parse-sort (sort columns)
  (when (and (stringp sort) (plusp (length sort)))
    (let* ((descending (char= (char sort 0) #\-))
           (name (if descending (subseq sort 1) sort))
           (column (and (plusp (length name))
                        (find-if (lambda (col)
                                   (and (column-sortable-p col)
                                        (stringp (column-name col))
                                        (string= name (column-name col))))
                                 columns))))
      (when column
        (values column descending)))))

(defun %cell-value (column row)
  (let ((accessor (column-accessor column)))
    (if (keywordp accessor)
        (getf row accessor)
        (funcall accessor row))))

(defun %cell-content (column row)
  (let ((value (%cell-value column row))
        (format (column-format column)))
    (cond (format (funcall format value))
          ((null value) "")
          ((stringp value) value)
          (t (princ-to-string value)))))

(defun sort-rows (rows columns sort)
  (let ((copy (copy-seq rows)))
    (multiple-value-bind (column descending) (parse-sort sort columns)
      (if column
          (stable-sort copy
                       (if descending (lambda (a b) (value< b a)) #'value<)
                       :key (lambda (row) (%cell-value column row)))
          copy))))

(defun %page-number (page)
  (cond ((integerp page) page)
        ((and (stringp page) (plusp (length page)) (every #'%ascii-digit-p page))
         (parse-integer page))
        (t 1)))

(defun page-rows (rows page page-size)
  (unless (typep page-size '(integer 1))
    (%canvas-error "page-size must be a positive integer: ~S" page-size))
  (let* ((length (length rows))
         (count (max 1 (ceiling length page-size)))
         (page (min count (max 1 (%page-number page))))
         (start (* (1- page) page-size)))
    (values (subseq rows (min length start) (min length (+ start page-size)))
            page
            count)))

(defun %align-attr (column)
  (and (column-align column) (string-downcase (symbol-name (column-align column)))))

(defmethod render ((c table))
  (let* ((columns (table-columns c))
         (link (table-link c))
         (id (component-id c))
         (target (and id (%str "#" id)))
         (size (table-page-size c))
         (rows (sort-rows (table-rows c) columns (table-sort c))))
    (multiple-value-bind (sort-column descending) (parse-sort (table-sort c) columns)
      (let ((valid-sort (and sort-column (table-sort c))))
        (multiple-value-bind (shown page count)
            (if size (page-rows rows (table-page c) size) (values rows 1 1))
          (flet ((header (column)
                   (let* ((name (column-name column))
                          (next (if (and (eq column sort-column) (not descending))
                                    (%str "-" name)
                                    name))
                          (hx (and link (column-sortable-p column)
                                   (hx-get (%link-merge link :sort next :page 1)
                                           :target target :swap :outer-html))))
                     (el :th (list :scope "col" :data-align (%align-attr column)
                                   :aria-sort (when (eq column sort-column)
                                                (if descending "descending" "ascending")))
                         (if hx
                             (el :button (list* :type "button" hx) (column-header column))
                             (column-header column)))))
                 (pager-button (label target-page)
                   (let ((hx (and target-page link
                                  (hx-get (apply #'%link-merge link
                                                 (append (when valid-sort (list :sort valid-sort))
                                                         (list :page target-page)))
                                          :target target :swap :outer-html))))
                     (if hx
                         (el :button (list* :type "button" hx) label)
                         (el :button '(:type "button" :disabled t) label)))))
            (let ((table
                    (el :table (unless id (%own c))
                        (when (table-caption c) (el :caption nil (table-caption c)))
                        (el :thead nil (el :tr nil (mapcar #'header columns)))
                        (el :tbody nil
                            (if (zerop (length shown))
                                (el :tr nil (el :td (list :colspan (length columns))
                                                (el :em nil (table-empty c))))
                                (map 'list
                                     (lambda (row)
                                       (el :tr nil
                                           (mapcar (lambda (column)
                                                     (el :td (list :data-align (%align-attr column))
                                                         (%cell-content column row)))
                                                   columns)))
                                     shown))))))
              (if id
                  (el :div (%own c)
                      table
                      (when size
                        (el :nav '(:aria-label "Pages")
                            (pager-button "Previous" (when (> page 1) (1- page)))
                            (el :span nil (format nil "Page ~D of ~D" page count))
                            (pager-button "Next" (when (< page count) (1+ page))))))
                  table))))))))
