;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-table.scm : generate HTML programmatically, with support
;; for simple style elements. 
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
;; 
;; * 2004.06.18: David Montenegro, added gnc:html-table-get-cell
;; 
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report html-table))

(use-modules (srfi srfi-2))
(use-modules (srfi srfi-9))
(use-modules (gnucash engine))
(use-modules (gnucash report html-style-info))
(use-modules (gnucash report html-document))
(use-modules (gnucash report report-utilities))

(export <html-table>)
(export gnc:html-table?)
(export <html-table-cell>)
(export gnc:make-html-table-cell-internal)
(export gnc:make-html-table-cell)
(export gnc:make-html-table-cell/size)
(export gnc:make-html-table-cell/markup)
(export gnc:make-html-table-cell/size/markup)
(export gnc:make-html-table-header-cell)
(export gnc:make-html-table-header-cell/markup)
(export gnc:make-html-table-header-cell/size)
(export gnc:make-html-table-cell/min-width)
(export gnc:html-table-cell?)
(export gnc:html-table-cell-rowspan)
(export gnc:html-table-cell-set-rowspan!)
(export gnc:html-table-cell-colspan)
(export gnc:html-table-cell-set-colspan!)
(export gnc:html-table-cell-tag)
(export gnc:html-table-cell-set-tag!)
(export gnc:html-table-cell-data)
(export gnc:html-table-cell-set-data-internal!)
(export gnc:html-table-cell-style)
(export gnc:html-table-cell-set-style-internal!)
(export gnc:html-table-cell-set-style!)
(export gnc:html-table-cell-append-objects!)
(export gnc:html-table-cell-render)
(export gnc:make-html-table-internal)
(export gnc:make-html-table)
(export gnc:html-table-data)
(export gnc:html-table-set-data!)
(export gnc:html-table-caption)
(export gnc:html-table-set-caption!)
(export gnc:html-table-set-col-headers!)
(export gnc:html-table-multirow-col-headers)
(export gnc:html-table-set-multirow-col-headers!)
(export gnc:html-table-style)
(export gnc:html-table-set-style-internal!)
(export gnc:html-table-row-styles)
(export gnc:html-table-set-row-styles!)
(export gnc:html-table-row-markup-table)
(export gnc:html-table-row-markup)
(export gnc:html-table-set-row-markup-table!)
(export gnc:html-table-set-row-markup!)
(export gnc:html-table-col-styles)
(export gnc:html-table-set-col-styles!)
(export gnc:html-table-col-headers-style)
(export gnc:html-table-set-col-headers-style!)
(export gnc:html-table-row-headers-style)
(export gnc:html-table-set-row-headers-style!)
(export gnc:html-table-set-last-row-style!)
(export gnc:html-table-set-style!)
(export gnc:html-table-set-col-style!)
(export gnc:html-table-set-row-style!)
(export gnc:html-table-row-style)
(export gnc:html-table-col-style)
(export gnc:html-table-num-rows)
(export gnc:html-table-set-num-rows-internal!)
(export gnc:html-table-num-columns)
(export gnc:html-table-append-row/markup!)
(export gnc:html-table-prepend-row/markup!)
(export gnc:html-table-append-row!)
(export gnc:html-table-prepend-row!)
(export gnc:html-table-get-cell)
(export gnc:html-table-set-cell!)
(export gnc:html-table-set-cell/tag!)
(export gnc:html-table-append-column!)
(export gnc:html-table-render)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; NB: In this code, "markup" and "/markup" *do not* refer to
;; style information.  Rather, they let you override the tag
;; associated with an html-table row or cell.  Style
;; information is stored in addition to this "markup" (in
;; an entirely different record field).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record-type <html-table>
  (make-html-table col-headers row-headers caption data num-rows style
                   col-styles row-styles row-markup-table col-headers-style
                   row-headers-style)
  html-table?
  (col-headers html-table-col-headers html-table-set-col-headers!)
  (row-headers html-table-row-headers html-table-set-row-headers!)
  (caption html-table-caption html-table-set-caption!)
  (data html-table-data html-table-set-data!)
  (num-rows html-table-num-rows html-table-set-num-rows!)
  (style html-table-style html-table-set-style!)
  (col-styles html-table-col-styles html-table-set-col-styles!)
  (row-styles html-table-row-styles html-table-set-row-styles!)
  (row-markup-table html-table-row-markup-table html-table-set-row-markup-table!)
  (col-headers-style html-table-col-headers-style)
  (row-headers-style html-table-row-headers-style))

(define gnc:html-table? html-table?)
(define gnc:make-html-table-internal make-html-table)
(define gnc:html-table-data html-table-data)
(define gnc:html-table-set-data! html-table-set-data!)
(define gnc:html-table-caption html-table-caption)
(define gnc:html-table-set-caption! html-table-set-caption!)
(define gnc:html-table-multirow-col-headers html-table-col-headers)
(define gnc:html-table-set-multirow-col-headers! html-table-set-col-headers!)
(define gnc:html-table-style html-table-style)
(define gnc:html-table-set-style-internal! html-table-set-style!)
(define gnc:html-table-row-styles html-table-row-styles)
(define gnc:html-table-set-row-styles! html-table-set-row-styles!)
(define gnc:html-table-row-markup-table html-table-row-markup-table)
(define gnc:html-table-set-row-markup-table! html-table-set-row-markup-table!)
(define gnc:html-table-col-styles html-table-col-styles)
(define gnc:html-table-set-col-styles! html-table-set-col-styles!)
(define gnc:html-table-col-headers-style html-table-col-headers-style)
(define gnc:html-table-row-headers-style html-table-row-headers-style)
(define gnc:html-table-num-rows html-table-num-rows)
(define gnc:html-table-set-num-rows-internal! html-table-set-num-rows!)


(define-record-type <html-table-cell>
  (make-html-table-cell rowspan colspan tag data style)
  html-table-cell?
  (rowspan html-table-cell-rowspan html-table-cell-set-rowspan!)
  (colspan html-table-cell-colspan html-table-cell-set-colspan!)
  (tag html-table-cell-tag html-table-cell-set-tag!)
  (data html-table-cell-data html-table-cell-set-data!)
  (style html-table-cell-style html-table-cell-set-style!))

(define gnc:make-html-table-cell-internal make-html-table-cell)
(define gnc:html-table-cell? html-table-cell?)
(define gnc:html-table-cell-rowspan html-table-cell-rowspan)
(define gnc:html-table-cell-set-rowspan! html-table-cell-set-rowspan!)
(define gnc:html-table-cell-colspan html-table-cell-colspan)
(define gnc:html-table-cell-set-colspan! html-table-cell-set-colspan!)
(define gnc:html-table-cell-tag html-table-cell-tag)
(define gnc:html-table-cell-set-tag! html-table-cell-set-tag!)
(define gnc:html-table-cell-data html-table-cell-data)
(define gnc:html-table-cell-set-data-internal! html-table-cell-set-data!)
(define gnc:html-table-cell-style html-table-cell-style)
(define gnc:html-table-cell-set-style-internal! html-table-cell-set-style!)

(define (gnc:make-html-table-cell . objects)
  (gnc:make-html-table-cell-internal 1 1 "td" objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-cell/size rowspan colspan . objects)
  (gnc:make-html-table-cell-internal rowspan colspan "td"
                                     objects (gnc:make-html-style-table)))

(define (gnc:make-html-table-cell/markup markup . objects)
  (gnc:make-html-table-cell-internal 1 1 markup objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-cell/size/markup rowspan colspan markup . objects)
  (gnc:make-html-table-cell-internal rowspan colspan markup
                                     objects (gnc:make-html-style-table)))

(define (gnc:make-html-table-cell/min-width px)
  (let ((cell (gnc:make-html-table-cell)))
    (gnc:html-table-cell-set-style!
     cell "td" 'attribute (list "style" (format #f "min-width:~apx" px)))
    cell))

(define (gnc:make-html-table-header-cell . objects)
  (gnc:make-html-table-cell-internal 1 1 "th" objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-header-cell/markup markup . objects)
  (gnc:make-html-table-cell-internal 1 1 markup objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-header-cell/size rowspan colspan . objects)
  (gnc:make-html-table-cell-internal rowspan colspan "th"
                                     objects (gnc:make-html-style-table)))

(define (gnc:html-table-cell-set-style! cell tag . rest)
  (let ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                      (apply gnc:make-html-data-style-info rest)
                      (apply gnc:make-html-markup-style-info rest)))
        (styletable (gnc:html-table-cell-style cell)))
    (gnc:html-style-table-set! styletable tag newstyle)))

(define (gnc:html-table-cell-append-objects! cell . objects)
  (gnc:html-table-cell-set-data-internal! 
   cell (append (gnc:html-table-cell-data cell) objects)))

(define (gnc:html-table-cell-render cell doc)
  ;; This function renders a html-table-cell to a document tree
  ;; segment. Note: if the first element in a html-table-cell data is
  ;; a negative number or gnc:monetary, it fixes the tag
  ;; eg. "number-cell" becomes "number-cell-red". The number and
  ;; gnc:monetary renderers do not have an automatic -neg tag
  ;; modifier. See bug 759005 and bug 797357.
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (cell-tag (gnc:html-table-cell-tag cell))
         (cell-data (gnc:html-table-cell-data cell))
         (tag (if (and (not (null? cell-data))
                       (not (string=? cell-tag "td"))
                       (or (and (gnc:gnc-monetary? (car cell-data))
                                (negative? (gnc:gnc-monetary-amount (car cell-data))))
                           (and (number? (car cell-data))
                                (negative? (car cell-data)))))
                  (string-append cell-tag "-neg")
                  cell-tag)))
    (gnc:html-document-push-style doc (gnc:html-table-cell-style cell))
    (push (gnc:html-document-markup-start
           doc tag #t
           (format #f "rowspan=\"~a\"" (gnc:html-table-cell-rowspan cell))
           (format #f "colspan=\"~a\"" (gnc:html-table-cell-colspan cell))))
    (for-each
     (lambda (child)
       (push (gnc:html-object-render child doc)))
     cell-data)
    (push (gnc:html-document-markup-end doc cell-tag))
    (gnc:html-document-pop-style doc)
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-table> class
;;  wrapper around HTML tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:make-html-table)
  (gnc:make-html-table-internal 
   #f                    ;; col-headers 
   #f                    ;; row-headers 
   #f                    ;; caption 
   '()                   ;; data (stored in reverse row-major order)
   0                     ;; num-rows
   (gnc:make-html-style-table) ;; style
   (make-hash-table 21)  ;; hash of col number to col-style 
   (make-hash-table 21)  ;; hash of row number to row-style
   (make-hash-table 21)  ;; hash of row number to row markup
   (gnc:make-html-style-table) ;; col-headers-style
   (gnc:make-html-style-table) ;; row-headers-style
   ))


(define (gnc:html-table-set-col-headers! table col-headers)
  (gnc:html-table-set-multirow-col-headers! table (list col-headers)))

(define (gnc:html-table-row-markup table row)
  (hash-ref (gnc:html-table-row-markup-table table) row))

(define (gnc:html-table-set-row-markup! table row markup)
  (hash-set! (gnc:html-table-row-markup-table table) row markup))

(define (gnc:html-table-set-col-headers-style! table tag . rest)
  (let ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                      (apply gnc:make-html-data-style-info rest)
                      (apply gnc:make-html-markup-style-info rest)))
        (style (gnc:html-table-col-headers-style table)))
    (gnc:html-style-table-set! style tag newstyle)))


(define (gnc:html-table-set-row-headers-style! table tag . rest)
  (let* ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                       (apply gnc:make-html-data-style-info rest)
                       (apply gnc:make-html-markup-style-info rest)))
         (style (gnc:html-table-row-headers-style table)))
    (gnc:html-style-table-set! style tag newstyle)))

(define (gnc:html-table-set-style! table tag . rest)
  (let* ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                       (apply gnc:make-html-data-style-info rest)
                       (apply gnc:make-html-markup-style-info rest)))
         (style (gnc:html-table-style table)))
    (gnc:html-style-table-set! style tag newstyle)))

(define (gnc:html-table-set-col-style! table col tag . rest)
  (let* ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                       (apply gnc:make-html-data-style-info rest)
                       (apply gnc:make-html-markup-style-info rest)))
         (newhash #f)
         (style (or (gnc:html-table-col-style table col)
                    (begin (set! newhash #t)
                           (gnc:make-html-style-table)))))
    (gnc:html-style-table-set! style tag newstyle)
    (if newhash (hash-set! (gnc:html-table-col-styles table) col style))))

(define (gnc:html-table-set-row-style! table row tag . rest)
  (let* ((newstyle (if (and (= (length rest) 2) (procedure? (car rest)))
                       (apply gnc:make-html-data-style-info rest)
                       (apply gnc:make-html-markup-style-info rest)))
         (newhash #f)
         (style (or (gnc:html-table-row-style table row)
                    (begin (set! newhash #t)
                           (gnc:make-html-style-table)))))
    (gnc:html-style-table-set! style tag newstyle)
    (when newhash (hash-set! (gnc:html-table-row-styles table) row style))))

(define (gnc:html-table-row-style table row)
  (hash-ref (gnc:html-table-row-styles table) row))

(define (gnc:html-table-col-style table col)
  (hash-ref (gnc:html-table-col-styles table) col))

(define (gnc:html-table-num-columns table)
  (apply max (cons 0 (map length (gnc:html-table-data table)))))

(define (gnc:html-table-append-row/markup! table markup newrow)
  (let ((rownum (gnc:html-table-append-row! table newrow)))
    (gnc:html-table-set-row-markup! table (- rownum 1) markup)))

(define (gnc:html-table-prepend-row/markup! table markup newrow)
  (gnc:html-table-prepend-row! table newrow)
  (gnc:html-table-set-row-markup! table 0 markup))
    

(define (gnc:html-table-append-row! table newrow)
  (let* ((current-num-rows (gnc:html-table-num-rows table))
	 (new-num-rows (1+ current-num-rows)))
    (gnc:html-table-set-num-rows-internal! table new-num-rows)
    (gnc:html-table-set-data! table
                              (cons (if (list? newrow) newrow (list newrow))
                                    (gnc:html-table-data table)))
    new-num-rows))

(define (gnc:html-table-prepend-row! table newrow)
  (let* ((new-num-rows (1+ (gnc:html-table-num-rows table)))
         (newrow-list (if (list? newrow) newrow (list newrow)))
         (dd (append (gnc:html-table-data table) (list newrow-list))))
    (gnc:html-table-set-num-rows-internal! table new-num-rows)
    (gnc:html-table-set-data! table dd)

    ;; have to bump up the row index of the row styles and row markup
    ;; table on a prepend.  just another reason you probably don't
    ;; want to prepend.
    (let ((new-rowstyles (make-hash-table 21)))
      (hash-for-each
       (lambda (row style)
         (hash-set! new-rowstyles (+ 1 row) style))
       (gnc:html-table-row-styles table))
      (gnc:html-table-set-row-styles! table new-rowstyles))

    (let ((new-rowmarkup (make-hash-table 21)))
      (hash-for-each
       (lambda (row markup)
         (hash-set! new-rowmarkup (+ 1 row) markup))
       (gnc:html-table-row-markup-table table))
      (gnc:html-table-set-row-markup-table! table new-rowmarkup))
    new-num-rows))

(define (gnc:html-table-get-cell table row col)
  (and-let* ((row (gnc:html-table-get-row table row)))
    (list-ref-safe row col)))

(define (gnc:html-table-get-row table row)
  (and-let* ((dd (gnc:html-table-data table))
             (len (length dd)))
    (list-ref-safe dd (- len row 1))))

;; this function is not exported
(define (gnc:html-table-set-cell-datum! table row col datum)
  (let lp ((len (length (gnc:html-table-data table))))
    (cond
     ((< row len)
      (let* ((row-loc (- len row 1))
             (old-tbldata (gnc:html-table-data table))
             (old-rowdata (list-ref old-tbldata row-loc))
             (new-rowdata (list-set-safe! old-rowdata col datum))
             (new-tbldata (list-set-safe! old-tbldata row-loc new-rowdata)))
        ;; add the row-data back to the table
        (gnc:html-table-set-data! table new-tbldata)))
     (else
      (gnc:html-table-append-row! table '())
      (lp (1+ len))))))

(define (gnc:html-table-set-cell! table row col . objects)
  (let ((tc (if (and (= (length objects) 1) (gnc:html-table-cell? (car objects)))
                (car objects)
                (apply gnc:make-html-table-cell objects))))
    (gnc:html-table-set-cell-datum! table row col tc)))

(define (gnc:html-table-set-cell/tag! table row col tag . objects)
  (let ((tc (if (and (= (length objects) 1) (gnc:html-table-cell? (car objects)))
                (car objects)
                (apply gnc:make-html-table-cell objects))))
    (gnc:html-table-cell-set-tag! tc tag)
    (gnc:html-table-set-cell-datum! table row col tc)))

(define (gnc:html-table-append-column! table newcol)
  (define width (apply max (cons 0 (map length (gnc:html-table-data table)))))
  (define (add-fn a b) (list-set-safe! b width a))
  (issue-deprecation-warning "gnc:html-table-append-column! deprecated. please \
populate html-table row-wise using gnc:html-table-append-row! instead.")
  (let lp ((newcol newcol)
           (olddata (reverse (gnc:html-table-data table)))
           (res '())
           (numrows 0))
    (cond
     ((null? newcol)
      (gnc:html-table-set-num-rows-internal! table numrows)
      (gnc:html-table-set-data! table res))
     ((null? olddata)
      (lp (cdr newcol)
          '()
          (cons (add-fn (car newcol) '()) res)
          (1+ numrows)))
     (else
      (lp (cdr newcol)
          (cdr olddata)
          (cons (add-fn (car newcol) (car olddata)) res)
          (1+ numrows))))))

(define (gnc:html-table-render table doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval)))))

    ;; compile the table style to make other compiles faster
    (gnc:html-style-table-compile (gnc:html-table-style table)
                                  (gnc:html-document-style-stack doc))

    (gnc:html-document-push-style doc (gnc:html-table-style table))
    (push (gnc:html-document-markup-start doc "table" #t))

    ;; render the caption
    (let ((c (gnc:html-table-caption table)))
      (when c
        (push (gnc:html-document-markup-start doc "caption" #t))
        (push (gnc:html-object-render c doc))
        (push (gnc:html-document-markup-end doc "caption"))))

    ;; the first row is the column headers.  Columns styles apply.
    ;; compile the col styles with the header style pushed; we'll
    ;; recompile them later, but this will have the benefit of
    ;; compiling in the col-header-style.
    (let ((ch (gnc:html-table-multirow-col-headers table)))
      (when ch
        (gnc:html-document-push-style doc (gnc:html-table-col-headers-style table))

        ;; compile the column styles just in case there's something
        ;; interesting in the table header cells.
        (hash-for-each
         (lambda (col style)
           (when style
             (gnc:html-style-table-compile
              style (gnc:html-document-style-stack doc))))
         (gnc:html-table-col-styles table))

        ;; render the headers
        (push (gnc:html-document-markup-start doc "thead" #t))

        (for-each
         (lambda (ch-row)
           (push (gnc:html-document-markup-start doc "tr" #t))
           (let lp ((ch-row ch-row) (colnum 0))
             (unless (null? ch-row)
               (let* ((hdr (car ch-row))
                      (table-cell? (gnc:html-table-cell? hdr))
                      (col-style (gnc:html-table-col-style table colnum)))
                 (gnc:html-document-push-style doc col-style)
                 (cond
                  (table-cell?
                   (push (gnc:html-object-render hdr doc)))
                  (else
                   (push (gnc:html-document-markup-start doc "th" #t))
                   (push (gnc:html-object-render hdr doc))
                   (push (gnc:html-document-markup-end doc "th"))))
                 (gnc:html-document-pop-style doc)
                 (lp (cdr ch-row)
                     (+ colnum
                        (if table-cell? (gnc:html-table-cell-colspan hdr) 1))))))
           (push (gnc:html-document-markup-end doc "tr")))
         ch)
        (push (gnc:html-document-markup-end doc "thead"))

        ;; pop the col header style
        (gnc:html-document-pop-style doc)))

    ;; recompile the column styles.  We won't worry about the row
    ;; styles; if they're there, we may lose, but not much, and they
    ;; will be pretty rare (I think).
    (hash-for-each
     (lambda (col style)
       (when style
         (gnc:html-style-table-compile style (gnc:html-document-style-stack doc))))
     (gnc:html-table-col-styles table))

    (push (gnc:html-document-markup-start doc "tbody" #t))
    ;; now iterate over the rows
    (let rowloop ((rows (reverse (gnc:html-table-data table))) (rownum 0))
      (unless (null? rows)
        (let* ((row (car rows))
               (rowstyle (gnc:html-table-row-style table rownum))
               (rowmarkup (or (gnc:html-table-row-markup table rownum) "tr")))

          ;; push the style for this row and write the start tag, then
          ;; pop it again.
          (when rowstyle (gnc:html-document-push-style doc rowstyle))
          (push (gnc:html-document-markup-start doc rowmarkup #t))
          (when rowstyle (gnc:html-document-pop-style doc))

          ;; write the column data, pushing the right column style
          ;; each time, then the row style.
          (let colloop ((cols row) (colnum 0))
            (unless (null? cols)
              (let* ((datum (car cols))
                     (colstyle (gnc:html-table-col-style table colnum)))
                ;; push col and row styles
                (when colstyle (gnc:html-document-push-style doc colstyle))
                (when rowstyle (gnc:html-document-push-style doc rowstyle))

                ;; render the cell contents
                (unless (gnc:html-table-cell? datum)
                  (push (gnc:html-document-markup-start doc "td" #t)))
                (push (gnc:html-object-render datum doc))
                (unless (gnc:html-table-cell? datum)
                  (push (gnc:html-document-markup-end doc "td")))

                ;; pop styles
                (when rowstyle (gnc:html-document-pop-style doc))
                (when colstyle (gnc:html-document-pop-style doc))
                (colloop (cdr cols) (1+ colnum)))))

          ;; write the row end tag and pop the row style
          (when rowstyle (gnc:html-document-push-style doc rowstyle))
          (push (gnc:html-document-markup-end doc rowmarkup))
          (when rowstyle (gnc:html-document-pop-style doc))

          (rowloop (cdr rows) (1+ rownum)))))
    (push (gnc:html-document-markup-end doc "tbody"))

    ;; write the table end tag and pop the table style
    (push (gnc:html-document-markup-end doc "table"))
    (gnc:html-document-pop-style doc)
    retval))

(define (gnc:html-table-set-last-row-style! table tag . rest)
  (apply gnc:html-table-set-row-style!
         (cons* table (1- (gnc:html-table-num-rows table)) tag rest)))
