;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; report-html.scm : generate HTML programmatically, with support
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

(require 'hash-table)
(require 'record)

(gnc:support "report-html.scm")

(gnc:depend "html-document.scm")
(gnc:depend "html-text.scm")
(gnc:depend "html-table.scm")
(gnc:depend "html-piechart.scm")
(gnc:depend "html-barchart.scm")
(gnc:depend "html-scatter.scm")
(gnc:depend "html-style-info.scm")
(gnc:depend "html-style-sheet.scm")
(gnc:depend "html-utilities.scm")

(gnc:depend "report-utilities.scm")

(define (for-each-in-order thunk list)
  (let loop ((ll list))
    (if (pair? ll)
        (let ((e (car ll)))
          (thunk e)
          (if (not (null? (cdr ll)))
              (loop (cdr ll)))))))
    
(define (list-ref-safe list elt)
  (if (> (length list) elt)
      (list-ref list elt)
      #f))

(define (list-set-safe! l elt val)
  (if (and (list? l) (> (length l) elt))
      (list-set! l elt val)
      (let ((filler (list val)))
        (if (not (list? l))
            (set! l '()))
        (let loop ((i (length l)))
          (if (< i elt)
              (begin 
                (set! filler (cons #f filler))
                (loop (+ 1 i)))))
        (set! l (append! l filler))))
  l)



