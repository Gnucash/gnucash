(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (gnucash report stylesheets))
(use-modules (srfi srfi-64))
(use-modules (ice-9 pretty-print))
(use-modules (sxml simple))
(use-modules (gnucash engine test srfi64-extras))
(use-modules (system vm coverage))

(define (coverage-test)
  (let* ((currfile (dirname (current-filename)))
         (path (string-take currfile (string-rindex currfile #\/))))
    (add-to-load-path path))
  (call-with-values
      (lambda()
        (with-code-coverage run-test-proper))
    (lambda (data result)
      (let ((port (open-output-file "/tmp/lcov.info")))
        (coverage-data->lcov data port)
        (close port)))))

(define (run-test)
  (if #f
      (coverage-test)
      (run-test-proper)))

(define (run-test-proper)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-html")
      ;; if (test-runner-factory gnc:test-runner) is commented out, this
      ;; will create Testing/Temporary/test-report-html.log
    (test-html-document-defintion)
    (test-html-objects-definition-for-literals)
    (test-html-objects)
    (test-html-cells)
    (test-html-table)
    (test-gnc:html-table-add-labeled-amount-line!)
    (test-gnc:make-html-acct-table/env/accts)
    (test-end "Testing/Temporary/test-report-html")
)

(define html-doc-header-no-title
"<!DOCTYPE html>
<html dir='auto'>\n\
<head>\n\
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n\
</head><body>")

(define html-doc-no-header-empty-body "")

(define html-doc-tail
"</body>\n\
</html>\n")

;; -----------------------------------------------------------------------

(define (test-html-document-defintion)

  (test-begin "HTML Document - Basic Creation")

  (let (
         (test-doc (gnc:make-html-document))
       )

    (test-assert "HTML Document - check predicate" (gnc:html-document? test-doc))
    (test-assert "HTML Document - default no stylesheet" (not (gnc:html-document-style-sheet test-doc)))
    (test-assert "HTML Document - default no style stack" (null? (gnc:html-document-style-stack test-doc)))
    (test-assert "HMTL Document - check style table predicate" (gnc:html-style-table? (gnc:html-document-style test-doc)))
    (test-assert "HTML Document - default no style text" (not (gnc:html-document-style-text test-doc)))
    (test-assert "HTML Document - default no title" (string-null? (gnc:html-document-title test-doc)))
    (test-assert "HTML Document - default no headline" (not (gnc:html-document-headline test-doc)))
    (test-assert "HTML Document - default no objects" (null? (gnc:html-document-objects test-doc)))

    (test-equal "HTML Document - Render empty body"
      (string-append html-doc-header-no-title html-doc-tail)
      (gnc:html-document-render test-doc)
    )

    (test-equal "HTML Document - Render empty body without header"
      html-doc-no-header-empty-body
      (gnc:html-document-render test-doc #f)
    )

    (gnc:html-document-set-title! test-doc "HTML Document Title")
    (test-equal "HTML Document - Render with title"
"<!DOCTYPE html>
<html dir='auto'>\n\
<head>\n\
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n\
<title>\n\
HTML Document Title</title></head><body></body>\n\
</html>\n"
      (gnc:html-document-render test-doc)
    )
  )

  (test-end "HTML Document - Creation")
)

;; -----------------------------------------------------------------------

(define (test-html-objects-definition-for-literals)

  (test-begin "HTML Object Definitions for literals")

  (test-equal "HTML Object for Strings"
    (string-append html-doc-header-no-title "<string> HTML Plain Text Body" html-doc-tail)
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-object "HTML Plain Text Body")
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Object for Numbers"
    (string-append html-doc-header-no-title "<number> 1234567890" html-doc-tail)
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-object 1234567890)
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Object for Boolean TRUE"
    (string-append html-doc-header-no-title "<boolean> #t" html-doc-tail)
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-object #t)
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  ;; NOTE: The following test for Boolean FALSE is correct.
  ;; #f values are used to indicate empty cells and will be converted to spaces
  (test-equal "HTML Object for Boolean FALSE"
    (string-append html-doc-header-no-title "<string>  " html-doc-tail)
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-object #f)
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Object for generic types"
    (string-append html-doc-header-no-title "<generic> (a b c d)" html-doc-tail)
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-object '(a b c d))
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-end "HTML Object Definitions for literals")
)

;; -----------------------------------------------------------------------

(define (test-html-objects)

  (test-begin "HTML Text Objects")

  (test-equal "HTML Text Object - no markup"
    (string-append html-doc-header-no-title
                   "<string> HTML Text Body - Part 1.<string> Part 2."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            "HTML Text Body - Part 1."
            "Part 2."
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - with number in decimal format"
    (string-append html-doc-header-no-title
                   "HTML Text with number <number> 7 in decimal format."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup/format
              "HTML Text with number ~a in decimal format."
              7
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - with number in float format"
    (string-append html-doc-header-no-title
                   "HTML Text with number <number> 8.8 in float format."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup/format
              "HTML Text with number ~a in float format."
              8.8
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - with boolean format"
    (string-append html-doc-header-no-title
                   "HTML Text with boolean <boolean> #f."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup/format
              "HTML Text with boolean ~a."
              #f
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - with literal format"
    (string-append html-doc-header-no-title
                   "HTML Text with literal <string> text123."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup/format
              "HTML Text with literal ~a."
              "text123"
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - with generic format"
    (string-append html-doc-header-no-title
                   "HTML Text with generic <generic> (a b c d)."
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup/format
              "HTML Text with generic ~a."
              '(a b c d)
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Paragraph"
    (string-append html-doc-header-no-title
                   "<p><string> HTML Text Paragraph</p>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-p "HTML Text Paragraph")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Typewriter"
    (string-append html-doc-header-no-title
                   "<tt><string> HTML Text Typewriter</tt>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-tt "HTML Text Typewriter")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Emphasized"
    (string-append html-doc-header-no-title
                   "<em><string> HTML Text Emphasized</em>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-em "HTML Text Emphasized")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Bold"
    (string-append html-doc-header-no-title
                   "<b><string> HTML Text Bold</b>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-b "HTML Text Bold")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Italic"
    (string-append html-doc-header-no-title
                   "<i><string> HTML Text Italic</i>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-i "HTML Text Italic")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Heading1"
    (string-append html-doc-header-no-title
                   "<h1><string> HTML Text Heading1</h1>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-h1 "HTML Text Heading1")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Heading2"
    (string-append html-doc-header-no-title
                   "<h2><string> HTML Text Heading2</h2>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-h2 "HTML Text Heading2")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Heading3"
    (string-append html-doc-header-no-title
                   "<h3><string> HTML Text Heading3</h3>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-h3 "HTML Text Heading3")
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Linebreak"
    (string-append html-doc-header-no-title
                   "<br /><string> HTML Text Linebreak"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-br)
            "HTML Text Linebreak"
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Headrow"
    (string-append html-doc-header-no-title
                   "<hr /><string> HTML Text Headrow"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-hr)
            "HTML Text Headrow"
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Unsorted List"
    (string-append html-doc-header-no-title
                   "<string> HTML Text Unsorted List<ul><li><string> Item1</li>\n<li><string> Item2</li>\n</ul>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            "HTML Text Unsorted List"
            (gnc:html-markup-ul '("Item1" "Item2"))
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Anchor Link"
    (string-append html-doc-header-no-title
                   "<a href=\"HTML Text Anchor Link\"><string> HTML Text Anchor Description</a>\n"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-anchor
              "HTML Text Anchor Link"
              "HTML Text Anchor Description"
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-equal "HTML Text Object - Image"
    (string-append html-doc-header-no-title
                   "<img src=\"https://www.gnucash.org/images/banner5.png\" width=\"72\" height=\"48\" alt=\"GunCash web site\"  />"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-img
              "https://www.gnucash.org/images/banner5.png"
              '("width" "72")
              '("height" "48")
              '("alt" "GunCash web site")
            )
          )
        )
      )
      (gnc:html-document-render test-doc)
    )
  )

  (test-end "HTML Text Objects")
)

;; -----------------------------------------------------------------------

(define (test-html-cells)

  (test-begin "HTML Cells")

  (test-equal "HTML Cell Creation"

"<tag rowspan=\"2\" colspan=\"3\">\
<string> HTML Table Cell\
<string> obj1\
<string> obj2<number> 123\
<boolean> #t\
<string>  \
<generic> (a b c d)</tag>\n"

    (let (
           (test-doc (gnc:make-html-document))
           (html-table-cell (gnc:make-html-table-cell "HTML Table Cell"))
         )
      (gnc:html-table-cell-set-rowspan! html-table-cell 2)
      (gnc:html-table-cell-set-colspan! html-table-cell 3)
      (gnc:html-table-cell-set-tag! html-table-cell "tag")
      (gnc:html-table-cell-append-objects!
        html-table-cell "obj1" "obj2" 123 #t #f '(a b c d)
      )
      (string-concatenate 
        (gnc:html-document-tree-collapse 
          (gnc:html-table-cell-render html-table-cell test-doc)
        )
      )
    )
  )

  (test-end "HTML Cells")
)

;; -----------------------------------------------------------------------

(define (test-html-table)

   ;; A table is list of rows in reverse order
   ;; Each row is a list of cells
   ;; The position the cell corresponds a column of the table
   ;;
   ;; Example:
   ;; ((r2c0 r2c1 r2c2) (r1c0 r1c1) (r0c0))
   ;;
   ;; The cell in row 1 and col 1 is r1c1. Each cell should hold
   ;; a html cell object (see previous test case).

  (test-begin "HTML Tables - without style sheets")

    (test-begin "HTML Table - Row Manipulations")
      (test-begin "HTML Table - Append Rows")
        (let (
               (test-doc (gnc:make-html-document))
               (test-table (gnc:make-html-table))
             )
          ;; change the default settings just to see what effect it has
          ;;(gnc:html-table-set-col-headers! test-table #t)
          ;; -> this make (gnc:html-table-render test-table test-doc) crash
          ;; col-headers must be #f or a list
          (gnc:html-table-set-row-headers! test-table #t)
          (gnc:html-table-set-caption! test-table #t)
          (gnc:html-table-append-row! test-table "Row 1")
          (gnc:html-table-append-row! test-table "Row 2")
          (test-equal "HTML Table - Check Num Rows after append row"
            2
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "HTML Table - Check data after append row"
            '(("Row 2") ("Row 1"))
            (gnc:html-table-data test-table)
          )
        )
      (test-end "HTML Table - Append Rows")
      (test-begin "HTML Table - Remove Rows")
        (let (
               (test-doc (gnc:make-html-document))
               (test-table (gnc:make-html-table))
             )
          ;; change the default settings just to see what effect it has
          ;;(gnc:html-table-set-col-headers! test-table #t)
          ;; -> this make (gnc:html-table-render test-table test-doc) crash
          ;; col-headers must be #f or a list
          (gnc:html-table-set-row-headers! test-table #t)
          (gnc:html-table-set-caption! test-table #t)
          (gnc:html-table-append-row! test-table "Row 1")
          (gnc:html-table-append-row! test-table "Row 2")
          (gnc:html-table-remove-last-row! test-table)
          (test-equal "HTML Table - Check Num Rows after remove row"
            1
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "HTML Table - Check data after remove row"
            '(("Row 1"))
            (gnc:html-table-data test-table)
          )
          (gnc:html-table-remove-last-row! test-table)
          (test-equal "HTML Table - Negative Test: Remove non-existing rows" '() (gnc:html-table-remove-last-row! test-table))
        )
      (test-end "HTML Table - Remove Rows")
      (test-begin "HTML Table - Prepend Rows")
        (let (
               (test-doc (gnc:make-html-document))
               (test-table (gnc:make-html-table))
             )
          ;; change the default settings just to see what effect it has
          ;;(gnc:html-table-set-col-headers! test-table #t)
          ;; -> this make (gnc:html-table-render test-table test-doc) crash
          ;; col-headers must be #f or a list
          (gnc:html-table-set-row-headers! test-table #t)
          (gnc:html-table-set-caption! test-table #t)
          (gnc:html-table-append-row! test-table "Row 2")
          (gnc:html-table-prepend-row! test-table "Row 1")
          (gnc:html-table-prepend-row! test-table "Row 0")
          (gnc:html-table-prepend-row! test-table "Row -1")
          (gnc:html-table-prepend-row! test-table '("r-2-c1" "r-2-c2"))
          (test-equal "HTML Table - Check Num Rows after prepend row"
            5
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "HTML Table - Check data after prepend row"
            '(("Row 2") ("Row 1") ("Row 0") ("Row -1") ("r-2-c1" "r-2-c2"))
            (gnc:html-table-data test-table)
          )
        )
      (test-end "HTML Table - Prepend Rows")
    (test-end "HTML Table - Row Manipulations")
    (test-begin "HTML Table - Cell Access and Edit")
      (let (
             (test-doc (gnc:make-html-document))
             (test-table (gnc:make-html-table))
           )
        ;; change the default settings just to see what effect it has
        ;;(gnc:html-table-set-col-headers! test-table #t)
        ;; -> this make (gnc:html-table-render test-table test-doc) crash
        ;; col-headers must be #f or a list
        (gnc:html-table-set-row-headers! test-table #t)
        (gnc:html-table-set-caption! test-table #t)
        (gnc:html-table-append-row! test-table "Row 1")
        (gnc:html-table-append-row! test-table "Row 2")
        (gnc:html-table-append-row! test-table "Row 3")
        (test-equal "HTML Table - Check Cell Access"
          "Row 1Row 2Row 3"
          (string-append
            (gnc:html-table-get-cell test-table 0 0)
            (gnc:html-table-get-cell test-table 1 0)
            (gnc:html-table-get-cell test-table 2 0)
          )
        )
        (test-assert "HTML Table - Negative Test: Check Cell Access - non-existing cells"
          (not
            (or (gnc:html-table-get-cell test-table 1 1)
                (gnc:html-table-get-cell test-table -1 0)
            )
          )
        )
      )
    (test-end "HTML Table - Cell Access and Edit")
    (test-begin "HTML Table - Append Columns")
      (let (
             (test-doc (gnc:make-html-document))
             (test-table (gnc:make-html-table))
           )
        ;; change the default settings just to see what effect it has
        ;;(gnc:html-table-set-col-headers! test-table #t)
        ;; -> this make (gnc:html-table-render test-table test-doc) crash
        ;; col-headers must be #f or a list
        (gnc:html-table-set-row-headers! test-table #t)
        (gnc:html-table-set-caption! test-table #t)
        (gnc:html-table-append-row! test-table "r1c1")
        (gnc:html-table-append-row! test-table '("r2c1" "r2c2" "r2c3"))
        (gnc:html-table-append-row! test-table '("r3c1" "r3c2"))
        (gnc:html-table-append-column! test-table '("r1c4" "r2c4" "r3c4" "r4c4"))
        (test-equal "HTML Table - Check Num Rows after append column"
          4
          (gnc:html-table-num-rows test-table)
        )
        (test-equal "HTML Table - Check data after append column"
          '((#f #f #f "r4c4") ("r3c1" "r3c2" #f "r3c4") ("r2c1" "r2c2" "r2c3" "r2c4") ("r1c1" #f #f "r1c4"))
          (gnc:html-table-data test-table)
        )
        (test-equal "HTML Table - Check Cell Access after append column"
          "r3c2"
          (gnc:html-table-get-cell test-table 2 1)
        )
      )
    (test-end "HTML Table - Append Columns")
    (test-begin "HTML Table - Table Rendering")
      (let (
             (test-doc (gnc:make-html-document))
             (test-table (gnc:make-html-table))
           )
        ;; change the default settings just to see what effect it has
        ;;(gnc:html-table-set-col-headers! test-table #t)
        ;; -> this make (gnc:html-table-render test-table test-doc) crash
        ;; col-headers must be #f or a list
        (gnc:html-table-set-row-headers! test-table #t)
        (gnc:html-table-set-caption! test-table #t)
        (gnc:html-table-append-row! test-table "Row 1")
        (gnc:html-table-append-row! test-table "Row 2")
        (gnc:html-table-append-column! test-table '("Col A" "Col B"))
        (test-equal "HTML Table - Check table rendering result"
"<table><caption><boolean> #t</caption>\n\
<tbody>\
<tr><td><string> Row 1</td>\n<td><string> Col A</td>\n</tr>\n\
<tr><td><string> Row 2</td>\n<td><string> Col B</td>\n</tr>\n\
</tbody>\n\
</table>\n"
          (string-concatenate
            (gnc:html-document-tree-collapse
              (gnc:html-table-render test-table test-doc)
            )
          )
        )
      )
    (test-end "HTML Table - Table Rendering")

    (test-begin "html-table arbitrary row/col modification")
    (let ((doc (gnc:make-html-document))
          (table (gnc:make-html-table)))
      (gnc:html-table-set-cell! table 0 0 "x")
      (test-equal "html-table-set-cell! 0 0"
        "<table><tbody><tr><td rowspan=\"1\" colspan=\"1\"><string> x</td>\n</tr>\n</tbody>\n</table>\n"
        (string-concatenate
         (gnc:html-document-tree-collapse
          (gnc:html-table-render table doc))))

      (gnc:html-table-set-cell! table 2 2 "y" "z")
      (test-equal "html-table-set-cell! 2 2"
        "<table><tbody><tr><td rowspan=\"1\" colspan=\"1\"><string> x</td>\n</tr>\n<tr></tr>\n<tr><td><string>  </td>\n<td><string>  </td>\n<td rowspan=\"1\" colspan=\"1\"><string> y<string> z</td>\n</tr>\n</tbody>\n</table>\n"
        (string-concatenate
         (gnc:html-document-tree-collapse
          (gnc:html-table-render table doc))))

      (let* ((table1 (gnc:make-html-table))
             (cell (gnc:make-html-table-cell "ab")))
        (gnc:html-table-set-cell! table1 1 4 cell)
        (test-equal "html-table-set-cell! 1 4"
          "<table><tbody><tr></tr>\n<tr><td><string>  </td>\n<td><string>  </td>\n<td><string>  </td>\n<td><string>  </td>\n<td rowspan=\"1\" colspan=\"1\"><string> ab</td>\n</tr>\n</tbody>\n</table>\n"
          (string-concatenate
           (gnc:html-document-tree-collapse
            (gnc:html-table-render table1 doc))))

        (gnc:html-table-set-cell/tag! table1 1 4 "tag" cell)
        (test-equal "html-table-set-cell/tag! 1 4"
          "<table><tbody><tr></tr>\n<tr><td><string>  </td>\n<td><string>  </td>\n<td><string>  </td>\n<td><string>  </td>\n<tag rowspan=\"1\" colspan=\"1\"><string> ab</tag>\n</tr>\n</tbody>\n</table>\n"
          (string-concatenate
           (gnc:html-document-tree-collapse
            (gnc:html-table-render table1 doc))))))
    (test-end "html-table arbitrary row/col modification")

    (test-begin "html-table-cell renderers")
    (let ((doc (gnc:make-html-document))
          (cell (gnc:make-html-table-cell 4)))
      (test-equal "html-table-cell renders correctly"
        "<td rowspan=\"1\" colspan=\"1\"><number> 4</td>\n"
        (string-concatenate
         (gnc:html-document-tree-collapse
          (gnc:html-table-cell-render cell doc)))))

    ;; the following is tailor-made to test bug 797357. if the report
    ;; system is refactored, this test will probably need fixing. it
    ;; aims to ensure the table-cell class eg 'number-cell'
    ;; 'total-number-cell' is augmented with a '-neg', and the
    ;; resulting renderer renders as <td class='number-cell neg' ...>
    (let* ((doc (gnc:make-html-document))
           (comm-table (gnc-commodity-table-get-table (gnc-get-current-book)))
           (USD (gnc-commodity-table-lookup comm-table "CURRENCY" "USD"))
           (USD-neg10 (gnc:make-gnc-monetary USD -10))
           (cell (gnc:make-html-table-cell/markup "number-cell" USD-neg10)))
      (test-equal "html-table-cell negative-monetary -> tag gets -neg appended"
        "number-cell-neg"
        (cadr
         (gnc:html-document-tree-collapse
          (gnc:html-table-cell-render cell doc)))))
    (test-end "html-table-cell renderers")

  (test-end "HTML Tables - without style sheets")
)

(define (test-gnc:html-table-add-labeled-amount-line!)

  (define (table->html table)
    (let ((doc (gnc:make-html-document)))
      (string-concatenate
       (gnc:html-document-tree-collapse
        (gnc:html-table-render table doc)))))

  (let ((table (gnc:make-html-table)))
    (gnc:html-table-add-labeled-amount-line!
     table #f #f #f "label" #f #f #f #f #f #f #f)
    (test-equal "gnc:html-table-add-labeled-amount-line!"
      "<table><tbody><tr><td rowspan=\"1\" colspan=\"1\"><string> <string> label</td>\n<td rowspan=\"1\" colspan=\"1\"><string>  </td>\n</tr>\n</tbody>\n</table>\n"
      (table->html table)))

  (let* ((table (gnc:make-html-table)))
    (gnc:html-table-add-labeled-amount-line!
     table 5 "tdd" #t "label1" 1 2 "label-markup"
     "amount" 3 2 "amount-markup")
    (test-equal "gnc:html-table-add-labeled-amount-line! all options"
      "<table><tbody><tdd><label-markup rowspan=\"1\" colspan=\"1\"><string> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<string> label1</label-markup>\n<td rowspan=\"1\" colspan=\"1\"><hr /></td>\n<amount-markup rowspan=\"1\" colspan=\"1\"><string> amount</amount-markup>\n<td><string>  </td>\n</tdd>\n</tbody>\n</table>\n"
      (table->html table))))

(define (test-gnc:make-html-acct-table/env/accts)

  ;; create html-document, add table, render, convert to sxml
  (define (table->sxml table prefix)
    (let* ((doc (gnc:make-html-document)))
      (gnc:html-document-set-style-sheet! doc (gnc:html-style-sheet-find "Default"))
      (gnc:html-document-add-object! doc table)
      (let ((render (gnc:html-document-render doc)))
        (call-with-output-file (format #f "/tmp/html-acct-table-~a.html" prefix)
          (lambda (p)
            (display render p)))
        (xml->sxml render
                   #:trim-whitespace? #t
                   #:entities '((nbsp . "\xa0")
                                (ndash . "­"))))))

  (let* ((accounts-alist (create-test-data))
         (accounts (map cdr accounts-alist)))

    (let* ((table (gnc:make-html-table))
           (get-balance (lambda (acc start-date end-date)
                          (let ((coll (gnc:make-commodity-collector)))
                            (coll 'add (xaccAccountGetCommodity acc) 10)
                            coll)))
           (acct-table (gnc:make-html-acct-table/env/accts
                        `((get-balance-fn ,get-balance)
                          (display-tree-depth 9))
                        accounts)))
      (gnc:html-table-add-account-balances table acct-table '())
      (let ((sxml (table->sxml table "basic - combo 1")))
        (test-equal "gnc:make-html-acct-table/env/accts combo 1"
          '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities"
            "Income" "Income-GBP" "Expenses" "Equity")
          (sxml->table-row-col sxml 1 #f 1))))

    (let* ((table (gnc:make-html-table))
           (acct-table (gnc:make-html-acct-table/env/accts
                        `((balance-mode pre-closing)
                          (display-tree-depth 9))
                        accounts)))
      (gnc:html-table-add-account-balances table acct-table '())
      (let ((sxml (table->sxml table "basic - combo 2")))
        (test-equal "gnc:make-html-acct-table/env/accts combo 2"
          '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities"
            "Income" "Income-GBP" "Expenses" "Equity")
          (sxml->table-row-col sxml 1 #f 1))))

    (let* ((table (gnc:make-html-table))
           (acct-table (gnc:make-html-acct-table/env/accts
                        '((balance-mode pre-adjusting)
                          (display-tree-depth 9))
                        accounts)))
      (gnc:html-table-add-account-balances table acct-table '())
      (let ((sxml (table->sxml table "basic - combo 3")))
        (test-equal "gnc:make-html-acct-table/env/accts combo 3"
          '("Root" "Asset" "Bank" "GBP Bank" "Wallet" "Liabilities"
            "Income" "Income-GBP" "Expenses" "Equity")
          (sxml->table-row-col sxml 1 #f 1))))))
