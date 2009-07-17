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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; NB: In this code, "markup" and "/markup" *do not* refer to
;; style information.  Rather, they let you override the tag
;; associated with an html-table row or cell.  Style
;; information is stored in addition to this "markup" (in
;; an entirely different record field).
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-table>
  (make-record-type "<html-table>"
                    '(col-headers
                      row-headers
                      caption 
                      data
		      num-rows
                      style
                      col-styles
                      row-styles
                      row-markup-table
                      col-headers-style
                      row-headers-style)))

(define gnc:html-table? 
  (record-predicate <html-table>))

(define <html-table-cell>
  (make-record-type "<html-table-cell>"
                    '(rowspan colspan tag data style)))

(define gnc:make-html-table-cell-internal
  (record-constructor <html-table-cell>))

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

(define (gnc:make-html-table-header-cell . objects)
  (gnc:make-html-table-cell-internal 1 1 "th" objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-header-cell/markup markup . objects)
  (gnc:make-html-table-cell-internal 1 1 markup objects 
                                     (gnc:make-html-style-table)))

(define (gnc:make-html-table-header-cell/size rowspan colspan . objects)
  (gnc:make-html-table-cell-internal rowspan colspan "th"
                                     objects (gnc:make-html-style-table)))

(define gnc:html-table-cell? 
  (record-predicate <html-table-cell>))

(define gnc:html-table-cell-rowspan
  (record-accessor <html-table-cell> 'rowspan))

(define gnc:html-table-cell-set-rowspan!
  (record-modifier <html-table-cell> 'rowspan))

(define gnc:html-table-cell-colspan
  (record-accessor <html-table-cell> 'colspan))

(define gnc:html-table-cell-set-colspan!
  (record-modifier <html-table-cell> 'colspan))

(define gnc:html-table-cell-tag
  (record-accessor <html-table-cell> 'tag))

(define gnc:html-table-cell-set-tag!
  (record-modifier <html-table-cell> 'tag))

(define gnc:html-table-cell-data
  (record-accessor <html-table-cell> 'data))

(define gnc:html-table-cell-set-data-internal!
  (record-modifier <html-table-cell> 'data))

(define gnc:html-table-cell-style
  (record-accessor <html-table-cell> 'style))

(define gnc:html-table-cell-set-style-internal!
  (record-modifier <html-table-cell> 'style))

(define (gnc:html-table-cell-set-style! cell tag . rest)
  (let ((newstyle #f)
        (styletable (gnc:html-table-cell-style cell)))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (gnc:html-style-table-set! styletable tag newstyle)))

(define (gnc:html-table-cell-append-objects! cell . objects)
  (gnc:html-table-cell-set-data-internal! 
   cell (append (gnc:html-table-cell-data cell) objects)))

(define (gnc:html-table-cell-render cell doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval))))
         (style (gnc:html-table-cell-style cell)))
    
;    ;; why dont colspans export??!
;    (gnc:html-table-cell-set-style! cell "td"
;	'attribute (list "colspan"
;	    (or (gnc:html-table-cell-colspan cell) 1)))
    (gnc:html-document-push-style doc style)
    (push (gnc:html-document-markup-start 
           doc (gnc:html-table-cell-tag cell)
           (sprintf #f "rowspan=%a" (gnc:html-table-cell-rowspan cell))
           (sprintf #f "colspan=%a" (gnc:html-table-cell-colspan cell))))
    (for-each 
     (lambda (child) 
       (push (gnc:html-object-render child doc)))
     (gnc:html-table-cell-data cell))
    (push (gnc:html-document-markup-end 
           doc (gnc:html-table-cell-tag cell)))
    (gnc:html-document-pop-style doc)
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-table> class
;;  wrapper around HTML tables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:make-html-table-internal
  (record-constructor <html-table>))

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

(define gnc:html-table-data
  (record-accessor <html-table> 'data))

(define gnc:html-table-set-data!
  (record-modifier <html-table> 'data))

(define gnc:html-table-caption
  (record-accessor <html-table> 'caption))

(define gnc:html-table-set-caption!
  (record-modifier <html-table> 'caption))

(define gnc:html-table-col-headers
  (record-accessor <html-table> 'col-headers))

(define gnc:html-table-set-col-headers!
  (record-modifier <html-table> 'col-headers))

(define gnc:html-table-row-headers
  (record-accessor <html-table> 'row-headers))

(define gnc:html-table-set-row-headers!
  (record-modifier <html-table> 'row-headers))

(define gnc:html-table-style
  (record-accessor <html-table> 'style))

(define gnc:html-table-set-style-internal!
  (record-modifier <html-table> 'style))

(define gnc:html-table-row-styles
  (record-accessor <html-table> 'row-styles))

(define gnc:html-table-set-row-styles!
  (record-modifier <html-table> 'row-styles))

(define gnc:html-table-row-markup-table
  (record-accessor <html-table> 'row-markup-table))

(define (gnc:html-table-row-markup table row)
  (hash-ref (gnc:html-table-row-markup-table table) row))

(define gnc:html-table-set-row-markup-table!
  (record-modifier <html-table> 'row-markup-table))

(define (gnc:html-table-set-row-markup! table row markup)
  (hash-set! (gnc:html-table-row-markup-table table) row markup))

(define gnc:html-table-col-styles
  (record-accessor <html-table> 'col-styles))

(define gnc:html-table-set-col-styles!
  (record-modifier <html-table> 'col-styles))

(define gnc:html-table-col-headers-style
  (record-accessor <html-table> 'col-headers-style))

(define (gnc:html-table-set-col-headers-style! table tag . rest)
  (let ((newstyle #f)
        (style (gnc:html-table-col-headers-style table)))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (gnc:html-style-table-set! style tag newstyle)))

(define gnc:html-table-row-headers-style
  (record-accessor <html-table> 'row-headers-style))

(define (gnc:html-table-set-row-headers-style! table tag . rest)
  (let ((newstyle #f)
        (style (gnc:html-table-row-headers-style table)))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (gnc:html-style-table-set! style tag newstyle)))

(define (gnc:html-table-set-style! table tag . rest)
  (let ((newstyle #f)
        (style (gnc:html-table-style table)))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (gnc:html-style-table-set! style tag newstyle)))

(define (gnc:html-table-set-col-style! table col tag . rest)
  (let ((newstyle #f)
        (style #f)
        (newhash #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (set! style 
          (gnc:html-table-col-style table col))
    (if (not style)
        (begin 
          (set! style (gnc:make-html-style-table))
          (set! newhash #t)))
    (gnc:html-style-table-set! style tag newstyle)
    (if newhash 
        (hash-set! (gnc:html-table-col-styles table) col style))))

(define (gnc:html-table-set-row-style! table row tag . rest)
  (let ((newstyle #f)
        (style #f)
        (newhash #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (set! style 
          (gnc:html-table-row-style table row))
    (if (not style)
        (begin 
          (set! style (gnc:make-html-style-table))
          (set! newhash #t)))
    (gnc:html-style-table-set! style tag newstyle)
    (if newhash 
        (hash-set! 
         (gnc:html-table-row-styles table) row style))))

(define (gnc:html-table-row-style table row)
  (hash-ref (gnc:html-table-row-styles table) row))

(define (gnc:html-table-col-style table col)
  (hash-ref (gnc:html-table-col-styles table) col))

(define gnc:html-table-num-rows
 (record-accessor <html-table> 'num-rows))

(define gnc:html-table-set-num-rows-internal!
  (record-modifier <html-table> 'num-rows))

(define (gnc:html-table-num-columns table)
  (let ((max 0))
    (for-each 
     (lambda (row)
       (let ((l (length row))) 
         (if (> l max)
             (set! max l))))
     (gnc:html-table-data table))
    max))

(define (gnc:html-table-append-row/markup! table markup newrow)
  (let ((rownum (gnc:html-table-append-row! table newrow)))
    (gnc:html-table-set-row-markup! table (- rownum 1) markup)))

(define (gnc:html-table-prepend-row/markup! table markup newrow)
  (begin
    (gnc:html-table-prepend-row! table newrow)
    (gnc:html-table-set-row-markup! table 0 markup)))
    

(define (gnc:html-table-append-row! table newrow)
  (let* ((dd (gnc:html-table-data table))
	 (current-num-rows (gnc:html-table-num-rows table))
	 (new-num-rows (+ current-num-rows 1)))
    (if (list? newrow)
        (set! dd (cons newrow dd))
        (set! dd (cons (list newrow) dd)))
    (gnc:html-table-set-num-rows-internal! 
     table 
     new-num-rows)
    (gnc:html-table-set-data! table dd)
    new-num-rows))

(define (gnc:html-table-remove-last-row! table)
  (if (> (gnc:html-table-num-rows table) 0)
      (begin
	(gnc:html-table-set-num-rows-internal! 
	 table 
	 (- (gnc:html-table-num-rows table) 1))
	(gnc:html-table-set-data! 
	 table
	 ;; Note that the rows in html-table-data are stored in
	 ;; reverse, i.e. the last appended table row is the first
	 ;; list element.
	 (cdr (gnc:html-table-data table))))
      '()))

(define (gnc:html-table-prepend-row! table newrow)
  (let* ((dd (gnc:html-table-data table))
	(current-num-rows (gnc:html-table-num-rows table))
	(new-num-rows (+ current-num-rows 1)))
    (set! dd (append dd (list newrow)))
    (gnc:html-table-set-num-rows-internal!
     table
     new-num-rows)
    (gnc:html-table-set-data! table dd)
    
    ;; have to bump up the row index of the row styles and row markup
    ;; table on a prepend.  just another reason you probably don't
    ;; want to prepend.
    (let ((new-rowstyles (make-hash-table 21)))
      (hash-fold 
       (lambda (row style prev)
         (hash-set! new-rowstyles (+ 1 row) style)
         #f)
       #f (gnc:html-table-row-styles table))
      (gnc:html-table-set-row-styles! table new-rowstyles))

    (let ((new-rowmarkup (make-hash-table 21)))
      (hash-fold 
       (lambda (row markup prev)
         (hash-set! new-rowmarkup (+ 1 row) markup)
         #f)
       #f (gnc:html-table-row-markup-table table))
      (gnc:html-table-set-row-markup-table! table new-rowmarkup))
    
    new-num-rows))

;; list-set! is 0-based...
;;   (let ((a '(0 1 2))) (list-set! a 1 "x") a)
;;    => (0 "x" 2)
(define (gnc:html-table-get-cell table row col)
  (let* ((row (gnc:html-table-get-row table row)))
    (and row (list-ref-safe row col)))
  )

(define (gnc:html-table-get-row table row)
  (let* ((dd (gnc:html-table-data table))
	 (len (and dd (length dd)))
	 )
    (and len
	 (list-ref-safe dd (- (- len 1) row))
	 )
    ))

;; if the 4th arg is a cell, overwrite the existing cell,
;; otherwise, append all remaining objects to the existing cell
(define (gnc:html-table-set-cell! table row col . objects)
  (let ((rowdata #f)
        (row-loc #f)
        (l (length (gnc:html-table-data table)))
        (objs (length objects))
        )
    ;; ensure the row-data is there 
    (if (>= row l)
        (begin
          (let loop ((i l))
            (gnc:html-table-append-row! table (list))
            (if (< i row)
                (loop (+ i 1))))
          (set! l (gnc:html-table-num-rows table))
          (set! row-loc (- (- l 1) row))
          (set! rowdata (list)))
        (begin
          (set! row-loc (- (- l 1) row))
          (set! rowdata (list-ref (gnc:html-table-data table) row-loc))))
    
    ;; make a table-cell and set the data 
    (let* ((tc (gnc:make-html-table-cell))
           (first (car objects)))
      (if (and (equal? objs 1) (gnc:html-table-cell? first))
          (set! tc first)
          (apply gnc:html-table-cell-append-objects! tc objects)
          )
      (set! rowdata (list-set-safe! rowdata col tc))
      
      ;; add the row-data back to the table 
      (gnc:html-table-set-data! 
       table (list-set-safe! 
              (gnc:html-table-data table) 
              row-loc rowdata)))))

;; if the 4th arg is a cell, overwrite the existing cell,
;; otherwise, append all remaining objects to the existing cell
(define (gnc:html-table-set-cell/tag! table row col tag . objects)
  (let ((rowdata #f)
        (row-loc #f)
        (l (length (gnc:html-table-data table)))
        (num-objs (length objects))
        )
    ;; ensure the row-data is there 
    (if (>= row l)
        (begin
          (let loop ((i l))
            (gnc:html-table-append-row! table (list))
            (if (< i row)
                (loop (+ i 1))))
          (set! l (gnc:html-table-num-rows table))
          (set! row-loc (- (- l 1) row))
          (set! rowdata (list)))
        (begin
          (set! row-loc (- (- l 1) row))
          (set! rowdata (list-ref (gnc:html-table-data table) row-loc))))
    
    ;; make a table-cell and set the data 
    (let* ((tc (gnc:make-html-table-cell))
           (first (car objects)))
      (if (and (equal? num-objs 1) (gnc:html-table-cell? first))
          (set! tc first)
          (apply gnc:html-table-cell-append-objects! tc objects)
          )
      (gnc:html-table-cell-set-tag! tc tag)
      (set! rowdata (list-set-safe! rowdata col tc))
      
      ;; add the row-data back to the table 
      (gnc:html-table-set-data! 
       table (list-set-safe! 
              (gnc:html-table-data table) 
              row-loc rowdata)))))

(define (gnc:html-table-append-column! table newcol)
  (define (maxwidth table-data)
    (if (null? table-data) 0
	(max (length (car table-data)) (maxwidth (cdr table-data)))))
	  
  ;; widen an individual row to the required width and append element
  (define (widen-and-append row element width)
    (let ((current-width (length row))
          (new-suffix (list element)))
      (do 
	  ((i current-width (+ i 1)))
	  ((< i width) #f)
	(set! new-suffix (cons #f new-suffix)))
      (append row new-suffix)))

  ;; append the elements of newcol to each of the existing rows, widening
  ;; to width-to-make if necessary 
  (define (append-to-element newcol existing-data length-to-append 
                             width-to-make)
    (if (= length-to-append 0) 
        (cons '() newcol)
        (let* 
            ((current-new (car newcol))
             (current-existing (car existing-data))
             (rest-new (cdr newcol))
             (rest-existing (cdr existing-data))
             (rest-result (append-to-element rest-new rest-existing 
                                             (- length-to-append 1)
                                             width-to-make)))
          (cons (cons (widen-and-append 
                       current-existing 
                       current-new 
                       width-to-make )
                      (car rest-result))
                (cdr rest-result)))))
  
  (let* ((existing-data (reverse (gnc:html-table-data table)))
	 (existing-length (length existing-data))
	 (width-to-make (+ (maxwidth existing-data) 1))
	 (newcol-length (length newcol)))
    (if (<= newcol-length existing-length)
        (gnc:html-table-set-data! 
         table
         (reverse (car (append-to-element 
                         newcol
                         existing-data
                         newcol-length 
                         width-to-make))))
        (let* ((temp-result (append-to-element
                             newcol
                             existing-data
                             existing-length
                             width-to-make))
               (joined-table-data (car temp-result))
               (remaining-elements (cdr temp-result)))
          ;; Invariant maintained - table data in reverse order
          (gnc:html-table-set-data! table (reverse joined-table-data))
          
          (for-each 
           (lambda (element)
             (gnc:html-table-append-row! table 
                                         (widen-and-append 
                                          '() 
                                          element 
                                          width-to-make)))
           remaining-elements)
          #f))))

(define (gnc:html-table-prepend-column! table newcol)
  ;; returns a pair, the car of which is the prepending of newcol
  ;; and existing-data, and the cdr is the remaining elements of newcol
  (define (prepend-to-element newcol existing-data length-to-append)
    (if (= length-to-append 0) ('() . newcol)
        (let* 
            ((current-new (car newcol))
             (current-existing (car existing-data))
             (rest-new (cdr newcol))
             (rest-existing (cdr existing-data))
             (rest-result (prepend-to-element rest-new rest-existing 
                                              (- length-to-append 1))))
          (cons 
           (cons (cons current-new current-existing) (car rest-result))
           (cdr rest-result)))))
  (let* ((existing-data (reverse (gnc:html-table-data table)))
	 (existing-length (length existing-data))
	 (newcol-length (length newcol)))
    (if (<= newcol-length existing-length)
        (gnc:html-table-set-data! 
         (reverse (car (prepend-to-element 
                        newcol
                        existing-data
                        newcol-length))))
        (let* ((temp-result (prepend-to-element
                             newcol
                             existing-data
                             existing-length))
               (joined-table-data (car temp-result))
               (remaining-elements (cdr temp-result)))
          ;; Invariant maintained - table data in reverse order
          (gnc:html-table-set-data! table (reverse joined-table-data))
          (for-each 
           (lambda (element)
             (gnc:html-table-append-row! table (list element)))
           remaining-elements)
          #f))))

;; 
;; It would be nice to have table row/col/cell accessor functions in here.
;; It would also be nice to have table juxtaposition functions, too.
;; i.e., (gnc:html-table-nth-row table n)
;;  [ CAS: how is that different from gnc:html-table-get-row ? ]

;;       (gnc:html-table-append-table-horizontal table add-table)
;; (An old merge-table used to exist inside balance-sheet.scm/GnuCash 1.8.9.)
;; Feel free to contribute! :-)
;; 

;; This function was moved here from balance-sheet.scm.
;; This function "stacks" the two tables vertically.
(define (gnc:html-table-merge t1 t2)
  (begin 
    (gnc:html-table-set-data! t1
			      (append
			       (gnc:html-table-data t2)
			       (gnc:html-table-data t1)))
    (gnc:html-table-set-num-rows-internal!
     t1 (+ (gnc:html-table-num-rows t1)
           (gnc:html-table-num-rows t2)))))

(define (gnc:html-table-render table doc)
  (let* ((retval '())
         (push (lambda (l) (set! retval (cons l retval)))))
    
    ;; compile the table style to make other compiles faster 
    (gnc:html-style-table-compile 
     (gnc:html-table-style table) (gnc:html-document-style-stack doc))
    
    (gnc:html-document-push-style doc (gnc:html-table-style table))
    (push (gnc:html-document-markup-start doc "table"))
    
    ;; render the caption 
    (let ((c (gnc:html-table-caption table)))
      (if c
          (begin 
            (push (gnc:html-document-markup-start doc "caption"))
            (push (gnc:html-object-render c doc))
            (push (gnc:html-document-markup-end doc "caption")))))
    
    ;; the first row is the column headers.  Columns styles apply.
    ;; compile the col styles with the header style pushed; we'll
    ;; recompile them later, but this will have the benefit of
    ;; compiling in the col-header-style.
    (let ((ch (gnc:html-table-col-headers table))
          (colnum 0))
      (if ch 
          (begin 
            (gnc:html-document-push-style 
             doc (gnc:html-table-col-headers-style table))
            (push (gnc:html-document-markup-start doc "tr"))
            
            ;; compile the column styles just in case there's
            ;; something interesting in the table header cells.
            (hash-fold
             (lambda (col style init)
               (if style
                   (gnc:html-style-table-compile 
                    style (gnc:html-document-style-stack doc)))
               #f)
             #f (gnc:html-table-col-styles table))
            
            ;; render the headers 
            (for-each 
             (lambda (hdr) 
               (gnc:html-document-push-style 
                doc (gnc:html-table-col-style table colnum))
               (if (not (gnc:html-table-cell? hdr))
                   (push (gnc:html-document-markup-start doc "th")))
               (push (gnc:html-object-render hdr doc))
               (if (not (gnc:html-table-cell? hdr))
                   (push (gnc:html-document-markup-end doc "th")))
               (gnc:html-document-pop-style doc)
               (if (not (gnc:html-table-cell? hdr))
                   (set! colnum (+ 1 colnum))
                   (set! colnum (+ (gnc:html-table-cell-colspan hdr)
                                   colnum))))
             ch)
            ;; pop the col header style 
            (gnc:html-document-pop-style doc))))
    
    ;; recompile the column styles.  We won't worry about the row
    ;; styles; if they're there, we may lose, but not much, and they
    ;; will be pretty rare (I think).
    (hash-fold
     (lambda (col style init)
       (if style
           (gnc:html-style-table-compile 
            style (gnc:html-document-style-stack doc)))
       #f)
     #f (gnc:html-table-col-styles table))
    
    ;; now iterate over the rows 
    (let ((rownum 0) (colnum 0))
      (for-each 
       (lambda (row) 
         (let ((rowstyle 
                (gnc:html-table-row-style table rownum))
               (rowmarkup 
                (gnc:html-table-row-markup table rownum)))
           ;; set default row markup
           (if (not rowmarkup)
               (set! rowmarkup "tr"))
           
           ;; push the style for this row and write the start tag, then 
           ;; pop it again.
           (if rowstyle (gnc:html-document-push-style doc rowstyle))
           (push (gnc:html-document-markup-start doc rowmarkup))
           (if rowstyle (gnc:html-document-pop-style doc))
           
           ;; write the column data, pushing the right column style 
           ;; each time, then the row style.  
           (for-each 
            (lambda (datum)
              (let ((colstyle 
                     (gnc:html-table-col-style table colnum)))
                ;; push col and row styles 
                (if colstyle (gnc:html-document-push-style doc colstyle))
                (if rowstyle (gnc:html-document-push-style doc rowstyle))
                
                ;; render the cell contents 
                (if (not (gnc:html-table-cell? datum))
                    (push (gnc:html-document-markup-start doc "td")))
                (push (gnc:html-object-render datum doc))
                (if (not (gnc:html-table-cell? datum))
                    (push (gnc:html-document-markup-end doc "td")))
                
                ;; pop styles 
                (if rowstyle (gnc:html-document-pop-style doc))
                (if colstyle (gnc:html-document-pop-style doc))
                (set! colnum (+ 1 colnum))))
            row)
           
           ;; write the row end tag and pop the row style 
           (if rowstyle (gnc:html-document-push-style doc rowstyle))
           (push (gnc:html-document-markup-end doc rowmarkup))
           (if rowstyle (gnc:html-document-pop-style doc))
           
           (set! colnum 0)
           (set! rownum (+ 1 rownum))))
       (reverse (gnc:html-table-data table))))
    
    ;; write the table end tag and pop the table style
    (push (gnc:html-document-markup-end doc "table"))
    (gnc:html-document-pop-style doc)
    retval))
