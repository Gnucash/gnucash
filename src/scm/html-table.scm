;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-table.scm : generate HTML programmatically, with support
;; for simple style elements. 
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "html-table.scm")

(define <html-table>
  (make-record-type "<html-table>"
                    '(col-headers
                      row-headers
                      caption 
                      data
                      style
                      col-styles
                      row-styles
                      col-headers-style
                      row-headers-style)))

(define gnc:html-table? 
  (record-predicate <html-table>))

(define <html-table-cell>
  (make-record-type "<html-table-cell>"
                    '(row-span col-span data style)))

(define gnc:make-html-table-cell-internal
  (record-constructor <html-table-cell>))

(define (gnc:make-html-table-cell)
  (gnc:make-html-table-cell-internal 1 1 '() (make-hash-table 7)))

(define gnc:html-table-cell? 
  (record-predicate <html-table-cell>))

(define gnc:html-table-cell-row-span
  (record-accessor <html-table-cell> 'row-span))

(define gnc:html-table-cell-set-row-span!
  (record-modifier <html-table-cell> 'row-span))

(define gnc:html-table-cell-col-span
  (record-accessor <html-table-cell> 'col-span))

(define gnc:html-table-cell-set-col-span!
  (record-modifier <html-table-cell> 'col-span))

(define gnc:html-table-cell-data
  (record-accessor <html-table-cell> 'data))

(define gnc:html-table-cell-set-data-internal!
  (record-modifier <html-table-cell> 'data))

(define gnc:html-table-cell-style
  (record-accessor <html-table-cell> 'style))

(define gnc:html-table-cell-set-style-internal!
  (record-modifier <html-table-cell> 'style))

(define (gnc:html-table-cell-set-style! cell tag . rest)
  (let ((newstyle #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (hash-set! (gnc:html-table-cell-style cell) tag newstyle)))

(define (gnc:html-table-cell-append-objects! cell . objects)
  (gnc:html-table-cell-set-data-internal! 
   cell (append (gnc:html-table-cell-data cell) objects)))

(define (gnc:html-table-cell-render cell doc)
  (with-output-to-string
    (lambda ()
      (gnc:html-document-push-style doc (gnc:html-table-cell-style cell))
      (display (gnc:html-document-markup-start doc "td"))
      (for-each-in-order 
       (lambda (child) 
         (display (gnc:html-object-render child doc)))
       (gnc:html-table-cell-data cell))
      (display (gnc:html-document-markup-end doc "td"))
      (gnc:html-document-pop-style doc))))

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
   '()                   ;; data 
   (make-hash-table 7)  ;; style
   '()                   ;; col-styles 
   '()                   ;; row-styles 
   (make-hash-table 7)  ;; col-headers-style
   (make-hash-table 7)  ;; row-headers-style
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

(define gnc:html-table-col-styles
  (record-accessor <html-table> 'col-styles))

(define gnc:html-table-set-col-styles!
  (record-modifier <html-table> 'col-styles))

(define gnc:html-table-col-headers-style
  (record-accessor <html-table> 'col-headers-style))

(define gnc:html-table-set-col-headers-style!
  (record-modifier <html-table> 'col-headers-style))

(define gnc:html-table-row-headers-style
  (record-accessor <html-table> 'row-headers-style))

(define gnc:html-table-set-row-headers-style!
  (record-modifier <html-table> 'row-headers-style))

(define (gnc:html-table-set-style! table tag . rest)
  (let ((newstyle #f))
    (if (and (= (length rest) 2)
             (procedure? (car rest)))
        (set! newstyle 
              (apply gnc:make-html-data-style-info rest))
        (set! newstyle 
              (apply gnc:make-html-markup-style-info rest)))
    (hash-set! (gnc:html-table-style table) tag newstyle)))

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
          (list-ref-safe (gnc:html-table-col-styles table) col))
    (if (not style)
        (begin 
          (set! style (make-hash-table 7))
          (set! newhash #t)))
    (hash-set! style tag newstyle)
    (if newhash 
        (gnc:html-table-set-col-styles! 
         table
         (list-set-safe! (gnc:html-table-col-styles table) col style)))))


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
          (list-ref-safe (gnc:html-table-row-styles table) row))
    (if (not style)
        (begin 
          (set! style (make-hash-table 7))
          (set! newhash #t)))
    (hash-set! style tag newstyle)
    (if newhash 
        (gnc:html-table-set-row-styles! 
         table
         (list-set-safe! (gnc:html-table-row-styles table) row style)))))

(define (gnc:html-table-num-rows table)
  (length (gnc:html-table-data table)))

(define (gnc:html-table-num-columns table)
  (let ((max 0))
    (for-each 
     (lambda (row)
       (let ((l (length row)))
         (if (> l max)
             (set! max l))))
     (gnc:html-table-data table))
    max))

(define (gnc:html-table-append-row! table newrow)
  (let ((dd (gnc:html-table-data table)))
    (set! dd (append dd (list newrow)))
    (gnc:html-table-set-data! table dd)))

(define (gnc:html-table-prepend-row! table newrow)
  (let ((dd (gnc:html-table-data table)))
    (set! dd (cons newrow dd))
    (gnc:html-table-set-data! table dd)))

(define (gnc:html-table-set-cell! table row col . objects)
  (let ((rowdata #f)
        (l (length (gnc:html-table-data table))))
    ;; ensure the row-data is there 
    (if (>= row l)
        (set! rowdata (make-list (+ col 1) #f))
        (set! rowdata (list-ref (gnc:html-table-data table) row)))
    
    ;; make a table-cell and set the data 
    (let ((tc (gnc:make-html-table-cell)))
      (apply gnc:html-table-cell-append-objects! tc objects)
      (set! rowdata (list-set-safe! rowdata col tc)))
    
    ;; add the row-data back to the table 
    (gnc:html-table-set-data! 
     table
     (list-set-safe! (gnc:html-table-data table) row rowdata))))


(define (gnc:html-table-append-column! table newcol)
  (let ((colnum 0)
        (rownum 0)
        (rows (gnc:html-table-data table))
        (this-row #f)
        (new-row #f))
    ;; find out how many cols are already there in the deepest row
    (for-each 
     (lambda (row)
       (let ((l (length row)))
         (if (> l colnum)
             (set! colnum l))))
     rows)

    ;; append the elements of 'newrow' to the rowumns 
    (for-each-in-order
     (lambda (newelt)
       ;; find the row, or append one 
       (if (not (null? rows))
           (begin
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))

       ;; make sure the rowumn is long enough, then append the data 
       (let loop ((l (length this-row))
                  (r (reverse this-row)))
         (if (< l colnum)
             (loop (+ l 1) (cons #f r))
             (set! this-row 
                   (reverse (cons newelt r)))))
       (if new-row
           (gnc:html-table-append-row! table this-row)
           (list-set! (gnc:html-table-data table) rownum this-row))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-table-prepend-column! table newcol)
  (let ((rows (gnc:html-table-data table))
        (this-row #f)
        (new-row #f)
        (rownum 0))
    (for-each-in-order 
     (lambda (elt)
       (if (not (null? rows))
           (begin 
             (set! new-row #f)
             (set! this-row (car rows))
             (if (null? (cdr rows))
                 (set! rows #f)                
                 (set! rows (cdr rows))))
           (begin 
             (set! new-row #t)
             (set! this-row '())))
       (if new-row
           (gnc:html-table-append-row! table (list elt))
           (list-set! (gnc:html-table-data table) rownum
                      (cons elt this-row)))
       (set! rownum (+ 1 rownum)))
     newcol)))

(define (gnc:html-table-render table doc)
  (with-output-to-string 
    (lambda ()
      (gnc:html-document-push-style doc (gnc:html-table-style table))
      (display (gnc:html-document-markup-start doc "table"))
      
      ;; render the caption 
      (let ((c (gnc:html-table-caption table)))
        (if c
            (begin 
              (display (gnc:html-document-markup-start doc "caption"))
              (display (gnc:html-object-render c doc))
              (display (gnc:html-document-markup-end doc "caption")))))
      
      ;; the first row is the column headers 
      (let ((ch (gnc:html-table-col-headers table))
            (colnum 0))
        (if ch 
            (begin 
              (gnc:html-document-push-style 
               doc (gnc:html-table-col-headers-style table))
              (display (gnc:html-document-markup-start doc "tr"))
              (for-each-in-order 
               (lambda (hdr)
                 (gnc:html-document-push-style 
                  doc (list-ref-safe (gnc:html-table-col-styles table) colnum))
                 (display (gnc:html-document-markup-start doc "th"))
                 (display (gnc:html-object-render hdr doc))
                 (display (gnc:html-document-markup-end doc "th"))
                 (gnc:html-document-pop-style doc)
                 (set! colnum (+ 1 colnum)))
               ch)
              (gnc:html-document-pop-style doc)
              )))
      
      ;; now iterate over the rows 
      (let ((rownum 0) (colnum 0))
        (for-each-in-order 
         (lambda (row) 
           ;; push the style for this row and write the start tag
           (gnc:html-document-push-style 
            doc (list-ref-safe (gnc:html-table-row-styles table) rownum))
           (display (gnc:html-document-markup-start doc "tr"))
           
           ;; write the column data, pushing the right column style 
           ;; each time 
           (for-each-in-order 
            (lambda (datum)
              (gnc:html-document-push-style 
               doc (list-ref-safe (gnc:html-table-col-styles table) colnum))
              (if (not (gnc:html-table-cell? datum))
                  (display (gnc:html-document-markup-start doc "td")))
              (display (gnc:html-object-render datum doc))
              (if (not (gnc:html-table-cell? datum))
                  (display (gnc:html-document-markup-end doc "td")))
              (gnc:html-document-pop-style doc)
              (set! colnum (+ 1 colnum)))
            row)
           
           ;; write the row end tag and pop the row style 
           (display (gnc:html-document-markup-end doc "tr"))
           (gnc:html-document-pop-style doc)
           (set! colnum 0)
           (set! rownum (+ 1 rownum)))
         (gnc:html-table-data table)))
      
      ;; write the table end tag and pop the table style
      (display (gnc:html-document-markup-end doc "table"))
      (gnc:html-document-pop-style doc))))

