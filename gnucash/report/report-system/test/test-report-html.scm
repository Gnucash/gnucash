(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-html") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                      ;; will create Testing/Temporary/test-report-html.log
    (test-assert "HTML Document Definition" (test-check1))
    (test-assert "HTML Objects Definition for literals" (test-check2))
    (test-assert "HTML Text Object" (test-check3))
    (test-assert "HTML Table Cell" (test-check4))
    (test-assert "HTML Table" (test-check5))
    (test-end "Testing/Temporary/test-report-html")
)

;; -----------------------------------------------------------------------

(define (test-check1)
  (let (
         (test-doc (gnc:make-html-document))
       )
    (and
      (gnc:html-document? test-doc)
      (not (gnc:html-document-style-sheet test-doc))
      (null? (gnc:html-document-style-stack test-doc))
      (gnc:html-style-table? (gnc:html-document-style test-doc))
      (not (gnc:html-document-style-text test-doc))
      (string-null? (gnc:html-document-title test-doc))
      (not (gnc:html-document-headline test-doc))
      (null? (gnc:html-document-objects test-doc))
      (string=? (gnc:html-document-render test-doc)
        "<html>\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n<title>\n</title></head><body></body>\n</html>\n"
        ;; BUG?:
        ;; this code looks ugly
        ;;<html>
        ;;<head>
        ;;<meta http-equiv="content-type" content="text/html; charset=utf-8" />
        ;;<title>
        ;;</title></head><body></body>
        ;;</html>

        ;; BUG?:
        ;; There is no way to suppress the header, (not (null? headers?)) is always true

        ;; BUG?:
        ;; There is no way to suppress the title, (if (title)) is always true
        ;; BUG?:
        ;; title is already defined, no reason to make a (let) statement
        ;; so this
        ;;        (let ((title (gnc:html-document-title doc)))
        ;;          (if title
        ;;              (push (list "</title>" title "<title>\n"))))
        ;; should be this      
        ;;          (if (not (string-null? title))
        ;;              (push (list "</title>" title "<title>\n")))

      )
      (gnc:html-document-set-title! test-doc "HTML Document Title")
      (string=? (gnc:html-document-render test-doc)
        "<html>\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n<title>\nHTML Document Title</title></head><body></body>\n</html>\n"
      )
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check2)
  (let (
         (test-doc (gnc:make-html-document))
       )
    
    (gnc:html-document-append-objects! test-doc 
      (list 
        (gnc:make-html-object "HTML Plain Text Body") 
        (gnc:make-html-object 1234567890) 
        (gnc:make-html-object #t) 
        (gnc:make-html-object #f) 
        (gnc:make-html-object '(a b c d))
      )
    )

    (string=? (gnc:html-document-render test-doc) "<html>\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n<title>\n</title></head><body><string> HTML Plain Text Body<number> 1234567890<boolean> #t<string>  <generic> (a b c d)</body>\n</html>\n")
    ;; BUG: it is not possible to create a boolean false object, instead a string place holder is created
  )
)

;; -----------------------------------------------------------------------

(define (test-check3)
  (let (
         (test-doc (gnc:make-html-document))
       )
    
    (gnc:html-document-append-objects! test-doc 
      (list 
        (gnc:make-html-text "HTML Text Body - Part 1." "Part 2.")
        (gnc:make-html-text (gnc:html-markup/format "HTML Text with number ~a in decimal format." 7))
        (gnc:make-html-text (gnc:html-markup/format "HTML Text with number ~a in float format." 8.8))
        (gnc:make-html-text (gnc:html-markup/format "HTML Text with boolean ~a." #f))
        (gnc:make-html-text (gnc:html-markup/format "HTML Text with literal ~a." "text123"))
        (gnc:make-html-text (gnc:html-markup/format "HTML Text with generic ~a." '(a b c d)))
        (gnc:make-html-text (gnc:html-markup-p "HTML Text Paragraph"))
        (gnc:make-html-text (gnc:html-markup-tt "HTML Text Typewriter"))
        (gnc:make-html-text (gnc:html-markup-em "HTML Text Emphasized"))
        (gnc:make-html-text (gnc:html-markup-b "HTML Text Bold"))
        (gnc:make-html-text (gnc:html-markup-i "HTML Text Italic"))
        (gnc:make-html-text (gnc:html-markup-h1 "HTML Text Heading1"))
        (gnc:make-html-text (gnc:html-markup-h2 "HTML Text Heading2"))
        (gnc:make-html-text (gnc:html-markup-h3 "HTML Text Heading3"))
        (gnc:make-html-text (gnc:html-markup-br) "HTML Text Linebreak")
        (gnc:make-html-text (gnc:html-markup-hr) "HTML Text Headrow")
        (gnc:make-html-text "HTML Text Unsorted List" (gnc:html-markup-ul '("Item1" "Item2")))
        (gnc:make-html-text (gnc:html-markup-anchor "HTML Text Anchor Link" "HTML Text Anchor Description"))
        (gnc:make-html-text (gnc:html-markup-img "http://www.gnucash.org/images/banner5.png" '("width" "72") '("height" "48") '("alt" "GunCash web site")))
      )
    )

    (string=? (gnc:html-document-render test-doc) "<html>\n<head>\n<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n<title>\n</title></head><body><string> HTML Text Body - Part 1.<string> Part 2.HTML Text with number <number> 7 in decimal format.HTML Text with number <number> 8.8 in float format.HTML Text with boolean <boolean> #f.HTML Text with literal <string> text123.HTML Text with generic <generic> (a b c d).<p><string> HTML Text Paragraph</p>\n<tt><string> HTML Text Typewriter</tt>\n<em><string> HTML Text Emphasized</em>\n<b><string> HTML Text Bold</b>\n<i><string> HTML Text Italic</i>\n<h1><string> HTML Text Heading1</h1>\n<h2><string> HTML Text Heading2</h2>\n<h3><string> HTML Text Heading3</h3>\n<br /><string> HTML Text Linebreak<hr /><string> HTML Text Headrow<string> HTML Text Unsorted List<ul><li><string> Item1</li>\n<li><string> Item2</li>\n</ul>\n<a href=\"HTML Text Anchor Link\"><string> HTML Text Anchor Description</a>\n<img src=\"http://www.gnucash.org/images/banner5.png\" width=\"72\" height=\"48\" alt=\"GunCash web site\"  /></body>\n</html>\n")

  )
)

;; -----------------------------------------------------------------------

(define (test-check4)
  (let (
         (test-doc (gnc:make-html-document))
         (html-table-cell (gnc:make-html-table-cell "HTML Table Cell"))
       )
    (gnc:html-table-cell-set-rowspan! html-table-cell 2)
    (gnc:html-table-cell-set-colspan! html-table-cell 3)
    (gnc:html-table-cell-set-tag! html-table-cell "tag")
    (gnc:html-table-cell-append-objects! html-table-cell "obj1" "obj2" 123 #t #f '(a b c d))

    (string=? (string-concatenate (gnc:html-document-tree-collapse (gnc:html-table-cell-render html-table-cell test-doc)))
      "<tag rowspan=\"2\" colspan=\"3\"><string> HTML Table Cell<string> obj1<string> obj2<number> 123<boolean> #t<string>  <generic> (a b c d)</tag>\n"
    )    ;; BUG: it is not possible to create a boolean false object, instead a string place holder is created
  )
)

;; -----------------------------------------------------------------------

(define (test-check5)
  (let (
         (test-doc (gnc:make-html-document))
         (test-table (gnc:make-html-table))
       )

       ;; A table is list of rows in reverse order
       ;; Each row is a list of cells
       ;; The position the cell corresponds a column of the table
       ;;
       ;; Example:
       ;; ((r2c0 r2c1 r2c2) (r1c0 r1c1) (r0c0))
       ;;
       ;; The cell in row 1 and col 1 is r1c1. Each cell should hold
       ;; a html cell object (see previous test case).

    ;; change the default settings just to see what effect it has
    ;;(gnc:html-table-set-col-headers! test-table #t)
       ;; -> this make (gnc:html-table-render test-table test-doc) crash, col-headers must be #f or a list
    (gnc:html-table-set-row-headers! test-table #t)
    (gnc:html-table-set-caption! test-table #t)

    (and
      (= (gnc:html-table-append-row! test-table "Row 1") 1)
      (= (gnc:html-table-append-row! test-table "Row 2") 2) ;; data is now: (("Row 2") ("Row 1"))
      (= (gnc:html-table-num-rows test-table) 2)
      (= (length (gnc:html-table-remove-last-row! test-table)) 1)
      (= (length (gnc:html-table-remove-last-row! test-table)) 0)
      (null? (gnc:html-table-remove-last-row! test-table)) ;; simple negative test: try to remove non existing row
      (= (gnc:html-table-append-row! test-table "Row 2") 1)
      (= (gnc:html-table-prepend-row! test-table "Row 1") 2)
      (= (gnc:html-table-prepend-row! test-table "Row 0") 3)
      (= (gnc:html-table-prepend-row! test-table "Row -1") 4)
      ;; BUG: data is now: (("Row 2") "Row 1" "Row 0" "Row -1")
      ;;      for (gnc:html-table-get-cell test-table 2 0)
      ;;      this leads to error: (wrong-type-arg "length" "Wrong type argument in position ~A: ~S" (1 "Row 1") ("Row 1"))
      ;; BUG: gnc:html-table-prepend-row! updates the row-markup hash table which is
      ;;      - not updated on deletion of a row
      ;;      - not updated anywhere else in the code
      ;;      - not used anywhere else in GnuCash
      ;;      --> should be removed
      ;;      (same goes for gnc:html-table-row-markup, gnc:html-table-set-row-markup-table! gnc:html-table-set-row-markup!)
      ;; Reset table data:
      (gnc:html-table-set-data! test-table '()) ;; reset the table data due to bug above
      (gnc:html-table-set-num-rows-internal! test-table 0) ;; luckily for testng, this is not internal - BUG?
      (= (gnc:html-table-append-row! test-table "Row 1") 1)
      (= (gnc:html-table-append-row! test-table "Row 2") 2)
      (= (gnc:html-table-append-row! test-table "Row 3") 3)
      (string=? (gnc:html-table-get-cell test-table 2 0) "Row 3")
      (not (gnc:html-table-get-cell test-table 1 1)) ;; simple negative test
      (not (gnc:html-table-get-cell test-table -1 0)) ;; simple negative test
      (and
        (gnc:html-table-set-cell! test-table 2 1 "Row 3 Col 1")
        (string=? (car (gnc:html-table-cell-data (gnc:html-table-get-cell test-table 2 1))) "Row 3 Col 1")
      )
      (and
        (gnc:html-table-remove-last-row! test-table) ;; -> (("Row 2") ("Row 1"))
        (not (gnc:html-table-append-column! test-table '("Col A" "Col B" "Col C"))) ;; -> (("Col C") ("Row 2" "Col B") ("Row 1" "Col A"))
        (string=? (gnc:html-table-get-cell test-table 0 0) "Row 1")
        (string=? (gnc:html-table-get-cell test-table 1 0) "Row 2")
        ;;(string=? (gnc:html-table-get-cell test-table 2 0) "Col C") ;; -> error: "Value out of range"
        ;; Bug: the row counter has not been adjusted, should be three
      )
      (string=? (string-concatenate (gnc:html-document-tree-collapse (gnc:html-table-render test-table test-doc)))
        "<table><caption><boolean> #t</caption>\n<tbody><tr><td><string> Row 1</td>\n<td><string> Col A</td>\n</tr>\n<tr><td><string> Row 2</td>\n<td><string> Col B</td>\n</tr>\n<tr><td><string> Col C</td>\n</tr>\n</tbody>\n</table>\n")
    )
  )
)
