;;;; $Id$
;;;; HTML Support functions

(gnc:support "html-generator.scm")

(define (html-table-row-manual items)
  (list
   '("<TR>")
   items
   '("</TR>")))
   
(define (html-table-row-guess header? strong? items)
  (html-table-row-manual
   (map
    (lambda (item)
      (html-cell header? strong? item))
    items)))

(define (html-strong cell)
  (string-append
   "<STRONG>"
   cell
   "</STRONG>"))

(define (html-currency-cell header? strong? amount)
  (html-generic-cell
   #t
   header?
   strong?
   (string-append
    "<font face=\"Courier\""
    (if (< amount 0)
	(string-append
	 "color=#ff0000>("
	 (sprintf #f "%.2f" (- amount))
	 ")")
	(string-append
	 ">&nbsp;"
	 (sprintf #f "%.2f" amount)
	 "&nbsp;"))
    "</font>")))

(define (html-generic-cell right-align? header? strong? item)
  (string-append
   (if header? "<TH justify=center" "<TD")
   (if right-align? " align=right>" ">")
   (if strong? "<strong>" "")
   item
   (if strong? "</strong>" "")
   (if header? "</TH>" "</TD>")))

(define (html-number-cell header? strong? format number)
  (html-generic-cell
   #t
   header?
   strong?
   (sprintf #f format number)))

(define (html-add-cell header? strong? cline item)
  (string-append cline (html-cell header? strong? item)))

;; guess at what type they want
(define (html-cell header? strong? item)
  (cond ((string? item) 
	 (html-generic-cell #f header? strong? item))
	((number? item)
	 (html-currency-cell header? strong? item))
	(else "")))

(define (html-cell-header item)
  (html-cell #t #f item))

(define (html-cell-body item)
  (html-cell #f #f item))

(define (html-cell-header-strong item)
  (html-cell #t #t item))

(define (html-cell-body-strong item)
  (html-cell #f #t item))

(define (html-cell-header-right item)
  (html-generic-cell #t #t #f item))

(define (html-start-document)
  (list 
   "<HTML>"
   "<BODY bgcolor=#99ccff>"))

(define (html-end-document)
  (list
   "</BODY>"
   "</HTML>"))

(define (html-start-table)
  (list "<TABLE border=1>"))

(define (html-end-table)
  (list "</table>"))

