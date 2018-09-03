(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-html")
      ;; if (test-runner-factory gnc:test-runner) is commented out, this
      ;; will create Testing/Temporary/test-report-html.log
    (test-html-document-defintion)
    (test-html-objects-definition-for-literals)
    (test-html-objects)
    (test-html-cells)
    (test-html-table)
    (test-end "Testing/Temporary/test-report-html")
)

(define html-doc-header-empty-title
"<html>\n\
<head>\n\
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n\
<title>\n\
</title></head><body>")

(define html-doc-header-no-title
"<html>\n\
<head>\n\
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n\
</head><body>")

(define html-doc-no-header-empty-body
"<body></body>\n")

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

    (test-equal "HTML Document - Render empty body (without enhancement bug 796832)"
      (string-append html-doc-header-empty-title html-doc-tail)
      (gnc:html-document-render test-doc)
    )

    (test-expect-fail 2)
    (test-equal "HTML Document - Render without title (Bug 796827)"
      (string-append html-doc-header-no-title html-doc-tail)
      (gnc:html-document-render test-doc)
    )

    (test-equal "HTML Document - Render without header (Bug 796826)"
      html-doc-no-header-empty-body
      (gnc:html-document-render test-doc '())
    )

    (gnc:html-document-set-title! test-doc "HTML Document Title")
    (test-equal "HTML Document - Render with title"
"<html>\n\
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
    (string-append html-doc-header-empty-title "<string> HTML Plain Text Body" html-doc-tail)
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
    (string-append html-doc-header-empty-title "<number> 1234567890" html-doc-tail)
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
    (string-append html-doc-header-empty-title "<boolean> #t" html-doc-tail)
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

  (test-expect-fail 1)
  (test-equal "HTML Object for Boolean FALSE - Bug 796828"
    (string-append html-doc-header-empty-title "<boolean> #f" html-doc-tail)
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
    (string-append html-doc-header-empty-title "<generic> (a b c d)" html-doc-tail)
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
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
    (string-append html-doc-header-empty-title 
                   "<img src=\"http://www.gnucash.org/images/banner5.png\" width=\"72\" height=\"48\" alt=\"GunCash web site\"  />"
                   html-doc-tail
    )
    (let (
         (test-doc (gnc:make-html-document))
         )
      (gnc:html-document-append-objects! test-doc 
        (list 
          (gnc:make-html-text
            (gnc:html-markup-img
              "http://www.gnucash.org/images/banner5.png"
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

  (test-expect-fail 1)
  (test-equal "HTML Cell Creation - Bug 796828"

"<tag rowspan=\"2\" colspan=\"3\">\
<string> HTML Table Cell\
<string> obj1\
<string> obj2<number> 123\
<boolean> #t\
<boolean> #f\
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

    (test-begin "Row Manipulations")
      (test-begin "Append Rows")
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
          (test-equal "Check Num Rows after append row"
            2
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "Check data after append row"
            '(("Row 2") ("Row 1"))
            (gnc:html-table-data test-table)
          )
        )
      (test-end "Append Rows")
      (test-begin "Remove Rows")
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
          (test-equal "Check Num Rows after remove row"
            1
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "Check data after remove row"
            '(("Row 1"))
            (gnc:html-table-data test-table)
          )
          (gnc:html-table-remove-last-row! test-table)
          (test-equal "Negative Test: Remove non-existing rows" '() (gnc:html-table-remove-last-row! test-table))
        )
      (test-end "Remove Rows")
      (test-begin "Prepend Rows")
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
          (test-equal "Check Num Rows after prepend row"
            5
            (gnc:html-table-num-rows test-table)
          )
          (test-equal "Check data after prepend row"

            '(("Row 2") ("Row 1") ("Row 0") ("Row -1") ("r-2-c1" "r-2-c2"))
            (gnc:html-table-data test-table)
          )
        )
      (test-end "Prepend Rows")
    (test-end "Row Manipulations")
    (test-begin "Cell Access and Edit")
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
        (test-equal "Check Cell Access"
          "Row 1Row 2Row 3"
          (string-append
            (gnc:html-table-get-cell test-table 0 0)
            (gnc:html-table-get-cell test-table 1 0)
            (gnc:html-table-get-cell test-table 2 0)
          )
        )
        (test-assert "Negative Test: Check Cell Access - non-existing cells"
          (not
            (or (gnc:html-table-get-cell test-table 1 1)
                (gnc:html-table-get-cell test-table -1 0)
            )
          )
        )
      )
    (test-end "Cell Access and Edit")
    (test-begin "Append Columns")
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
        (test-equal "Check Num Rows after append column"
          4
          (gnc:html-table-num-rows test-table)
        )
        (test-equal "Check data after append column"
          '((#f #f #f "r4c4") ("r3c1" "r3c2" #f "r3c4") ("r2c1" "r2c2" "r2c3" "r2c4") ("r1c1" #f #f "r1c4"))
          (gnc:html-table-data test-table)
        )
        (test-equal "Check Cell Access after append column"
          "r3c2"
          (gnc:html-table-get-cell test-table 2 1)
        )
      )
    (test-end "Append Columns")
    (test-begin "Table Rendering")
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
        (test-equal "Check table rendering result"
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
    (test-end "Table Rendering")

  (test-end "HTML Tables - without style sheets")
)
