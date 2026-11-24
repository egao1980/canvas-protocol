(in-package #:canvas-protocol/tests)

(defparameter +thead+
  "<thead><tr><th scope=\"col\">Name</th><th scope=\"col\" data-align=\"end\">Age</th></tr></thead>")

(defun row-html (name age)
  (format nil "<tr><td>~A</td><td data-align=\"end\">~A</td></tr>" name age))

(deftest table-plain
  (ok (string= (s+ "<table>" +thead+ "<tbody>" (row-html "Bo" 30) (row-html "Al" 25)
                   (row-html "&lt;i&gt;Cy&lt;/i&gt;" 41) "</tbody></table>")
               (html (table *rows* (cols)))))
  (ok (string= (s+ "<table><caption>People &amp; co</caption>" +thead+ "<tbody>" (row-html "Bo" 30)
                   "</tbody></table>")
               (html (table (list (first *rows*)) (cols) :caption "People & co"))))
  (ok (string= (s+ "<table>" +thead+ "<tbody>" (row-html "Bo" 30) "</tbody></table>")
               (html (table (vector (first *rows*)) (cols))))
      "vector rows"))

(deftest table-empty-state
  (ok (string= (s+ "<table>" +thead+ "<tbody><tr><td colspan=\"2\"><em>No rows</em></td></tr></tbody></table>")
               (html (table nil (cols)))))
  (ok (contains "<td colspan=\"2\"><em>Nobody &lt;here&gt;</em></td>"
                (html (table #() (cols) :empty "Nobody <here>")))))

(deftest table-format-and-align
  (ok (string= "<table><thead><tr><th scope=\"col\" data-align=\"center\">Age</th></tr></thead><tbody><tr><td data-align=\"center\">30 y</td></tr></tbody></table>"
               (html (table (list (first *rows*))
                            (list (column "Age" :age :align :center
                                                     :format (lambda (v) (format nil "~D y" v))))))))
  (ok (string= "<table><thead><tr><th scope=\"col\">Age</th></tr></thead><tbody><tr><td><span data-badge=\"ok\">30</span></td></tr></tbody></table>"
               (html (table (list (first *rows*))
                            (list (column "Age" :age
                                          :format (lambda (v) (badge (princ-to-string v) :status :ok))))))))
  (ok (string= "<table><thead><tr><th scope=\"col\">X</th></tr></thead><tbody><tr><td></td></tr></tbody></table>"
               (html (table (list '(:x nil)) (list (column "X" :x)))))
      "NIL formats as empty")
  (ok (signals (column "A" :a :align :middle) 'canvas-error))
  (ok (signals (column "A" :a :sortable t) 'canvas-error) "sortable needs a name")
  (ok (signals (column "A" 42) 'canvas-error) "accessor"))

(defun sort-button (sort page label)
  (format nil "<button type=\"button\" hx-get=\"~APEOPLE?sort=~A&amp;page=~A\" hx-target=\"#people\" hx-swap=\"outerHTML\">~A</button>"
          +u+ sort page label))

(defun page-button (label &optional sort page)
  (if page
      (format nil "<button type=\"button\" hx-get=\"~APEOPLE?~@[sort=~A&amp;~]page=~A\" hx-target=\"#people\" hx-swap=\"outerHTML\">~A</button>"
              +u+ sort page label)
      (format nil "<button type=\"button\" disabled=\"\">~A</button>" label)))

(deftest table-sorted-paged-golden
  (ok (string= (s+ "<div id=\"people\"><table><thead><tr>"
                   "<th scope=\"col\">" (sort-button "name" 1 "Name") "</th>"
                   "<th scope=\"col\" data-align=\"end\" aria-sort=\"descending\">" (sort-button "age" 1 "Age") "</th>"
                   "</tr></thead><tbody>" (row-html "&lt;i&gt;Cy&lt;/i&gt;" 41) (row-html "Bo" 30) "</tbody></table>"
                   "<nav aria-label=\"Pages\">" (page-button "Previous")
                   "<span>Page 1 of 2</span>" (page-button "Next" "-age" 2) "</nav></div>")
               (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                          :sort "-age" :page "1" :page-size 2)))))

(deftest table-ascending-and-last-page
  (let ((out (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                        :sort "age" :page "2" :page-size 2))))
    (ok (contains (s+ "<th scope=\"col\" data-align=\"end\" aria-sort=\"ascending\">"
                      (sort-button "-age" 1 "Age"))
                  out))
    (ok (contains (s+ "<tbody>" (row-html "&lt;i&gt;Cy&lt;/i&gt;" 41) "</tbody>") out))
    (ok (contains (s+ (page-button "Previous" "age" 1) "<span>Page 2 of 2</span>" (page-button "Next"))
                  out))))

(deftest table-invalid-params-ignored
  (let ((out (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                        :sort "bogus" :page "x" :page-size 2))))
    (ok (not (contains "aria-sort" out)))
    (ok (contains (s+ "<tbody>" (row-html "Bo" 30) (row-html "Al" 25) "</tbody>") out))
    (ok (contains (page-button "Next" nil 2) out))
    (ok (contains "<span>Page 1 of 2</span>" out)))
  (ok (contains "<span>Page 2 of 2</span>"
                (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                           :page "99" :page-size 2)))))

(deftest table-link-params-first
  (ok (contains (s+ "hx-get=\"" +u+ "PEOPLE?q=x&amp;sort=name\"")
                (html (table *rows* (cols) :id "people" :link (canvas-link 'people :q "x")))))
  (ok (contains (s+ "hx-get=\"" +u+ "PEOPLE?q=x&amp;sort=name&amp;page=1\"")
                (html (table *rows* (cols) :id "people" :link (canvas-link 'people :q "x")
                                           :page-size 2)))))

;;; canvas-protocol#13: a sortable header carries page=1 only when the table pages
;;; (PAGE-SIZE given), so an unpaged table canvas can take &key sort alone.

(defun unpaged-sort-button (sort label)
  (format nil "<button type=\"button\" hx-get=\"~APEOPLE?sort=~A\" hx-target=\"#people\" hx-swap=\"outerHTML\">~A</button>"
          +u+ sort label))

(deftest table-unpaged-sort-links-carry-no-page
  (let ((out (html (table *rows* (cols) :id "people" :link (canvas-link 'people) :sort "age"))))
    (ok (string= (s+ "<div id=\"people\"><table><thead><tr>"
                     "<th scope=\"col\">" (unpaged-sort-button "name" "Name") "</th>"
                     "<th scope=\"col\" data-align=\"end\" aria-sort=\"ascending\">"
                     (unpaged-sort-button "-age" "Age") "</th>"
                     "</tr></thead><tbody>" (row-html "Al" 25) (row-html "Bo" 30)
                     (row-html "&lt;i&gt;Cy&lt;/i&gt;" 41) "</tbody></table></div>")
                 out))
    (ok (not (contains "page=" out)) "no page param anywhere")
    (ok (not (contains "<nav" out)) "no pager"))
  (ok (not (contains "page="
                     (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                                :sort "-name" :page "2"))))
      "PAGE without PAGE-SIZE is ignored and adds no page param"))

(deftest table-paged-sort-links-unchanged
  (let ((out (html (table *rows* (cols) :id "people" :link (canvas-link 'people)
                                        :sort "name" :page "2" :page-size 2))))
    (ok (contains (s+ "<th scope=\"col\" aria-sort=\"ascending\">" (sort-button "-name" 1 "Name") "</th>")
                  out))
    (ok (contains (s+ "<th scope=\"col\" data-align=\"end\">" (sort-button "age" 1 "Age") "</th>")
                  out))
    (ok (contains (s+ (page-button "Previous" "name" 1) "<span>Page 2 of 2</span>" (page-button "Next"))
                  out))))

(deftest table-without-link
  (let ((out (html (table *rows* (cols) :id "people" :sort "name"))))
    (ok (string= (s+ "<div id=\"people\"><table><thead><tr><th scope=\"col\" aria-sort=\"ascending\">Name</th>"
                     "<th scope=\"col\" data-align=\"end\">Age</th></tr></thead><tbody>"
                     (row-html "&lt;i&gt;Cy&lt;/i&gt;" 41) (row-html "Al" 25) (row-html "Bo" 30)
                     "</tbody></table></div>")
                 out)))
  (ok (signals (table *rows* (cols) :link (canvas-link 'people)) 'canvas-error) "link needs id")
  (ok (signals (table *rows* (cols) :page-size 0) 'canvas-error)))

(defun ages (rows) (map 'list (lambda (r) (getf r :age)) rows))

(deftest sort-rows-orders
  (ok (equal '(25 30 41) (ages (sort-rows *rows* (cols) "age"))))
  (ok (equal '(41 30 25) (ages (sort-rows *rows* (cols) "-age"))))
  (ok (equal '("<i>Cy</i>" "Al" "Bo")
             (mapcar (lambda (r) (getf r :name)) (sort-rows *rows* (cols) "name")))))

(deftest sort-rows-stable
  (let ((rows '((:name "a" :age 1) (:name "b" :age 1) (:name "c" :age 0))))
    (ok (equal '("c" "a" "b") (mapcar (lambda (r) (getf r :name)) (sort-rows rows (cols) "age"))))
    (ok (equal '("a" "b" "c") (mapcar (lambda (r) (getf r :name)) (sort-rows rows (cols) "-age"))))))

(deftest sort-rows-ignores-bad-sorts
  (dolist (sort '(nil "" "-" "bogus" "x"))
    (let ((out (sort-rows *rows* (append (cols) (list (column "X" :name :name "x"))) sort)))
      (ok (equal *rows* out) (format nil "sort ~S" sort))
      (ok (not (eq *rows* out)) "fresh copy"))))

(deftest sort-rows-vectors-and-no-mutation
  (let* ((v (coerce *rows* 'vector))
         (copy (copy-seq v))
         (out (sort-rows v (cols) "-age")))
    (ok (vectorp out))
    (ok (equal '(41 30 25) (ages out)))
    (ok (equalp copy v) "input untouched"))
  (let ((list (copy-list *rows*)))
    (sort-rows list (cols) "age")
    (ok (equal *rows* list))))

(deftest page-rows-basics
  (multiple-value-bind (rows page count) (page-rows '(1 2 3 4 5) 2 2)
    (ok (equal '(3 4) rows)) (ok (= 2 page)) (ok (= 3 count)))
  (multiple-value-bind (rows page) (page-rows '(1 2 3 4 5) "3" 2)
    (ok (equal '(5) rows)) (ok (= 3 page)))
  (dolist (bad '(0 -1 "x" nil "" "1.5"))
    (ok (= 1 (nth-value 1 (page-rows '(1 2 3) bad 2))) (format nil "page ~S" bad)))
  (multiple-value-bind (rows page) (page-rows '(1 2 3) 9 2)
    (ok (equal '(3) rows)) (ok (= 2 page)))
  (multiple-value-bind (rows page count) (page-rows '() 1 10)
    (ok (null rows)) (ok (= 1 page)) (ok (= 1 count)))
  (multiple-value-bind (rows page count) (page-rows #(1 2 3) 2 2)
    (ok (equalp #(3) rows)) (ok (vectorp rows)) (ok (= 2 page)) (ok (= 2 count)))
  (ok (signals (page-rows '(1) 1 0) 'canvas-error))
  (ok (signals (page-rows '(1) 1 "2") 'canvas-error)))

(deftest parse-sort-cases
  (multiple-value-bind (col desc) (parse-sort "-age" (cols))
    (ok (string= "age" (column-name col))) (ok desc))
  (multiple-value-bind (col desc) (parse-sort "name" (cols))
    (ok (string= "name" (column-name col))) (ng desc))
  (dolist (bad '(nil "" "-" "bogus" "--age"))
    (ok (null (parse-sort bad (cols))) (format nil "sort ~S" bad))))

(deftest value<-ordering
  (ok (value< nil 1)) (ng (value< 1 nil)) (ng (value< nil nil))
  (ok (value< 1 2)) (ng (value< 2 1)) (ok (value< 1/2 1.0))
  (ok (value< 5 "a")) (ng (value< "a" 5))
  (ok (value< "a" "b")) (ng (value< "b" "a")) (ok (value< "B" "a"))
  (ok (value< :a :b)) (ng (value< :b :a)))
