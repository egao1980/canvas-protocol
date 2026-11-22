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

(defun value< (a b)
  (declare (ignore a b))
  (%h4h0-pending 'value<))

(defun parse-sort (sort columns)
  (declare (ignore sort columns))
  (%h4h0-pending 'parse-sort))

(defun sort-rows (rows columns sort)
  (declare (ignore rows columns sort))
  (%h4h0-pending 'sort-rows))

(defun page-rows (rows page page-size)
  (declare (ignore rows page page-size))
  (%h4h0-pending 'page-rows))
