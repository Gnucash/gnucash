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

(define (html-para text)
  (string-append "<P>" text "</P>\n"))

(define (html-cell-attributes value attributes)
  (string-append "<TD " attributes ">" value "</TD>"))

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

;;;; Here's functions defined in average-balance.scm
;;;; The point to this is twofold:
;;;; 1.  It doesn't break anything because if the functions get
;;;;     redefined somewhere here, things were *already broken.*
;;;; 2.  It pushes all HTML stuff into *this* file, and encourages
;;;;     fixing any resulting mess.
;;;;;;;;;;;;;;;;;;;;
;; HTML Table
;;;;;;;;;;;;;;;;;;;;

; Convert to string
(define (tostring val) 
  (if (number? val) 
      (sprintf #f "%.2f" val)
      (call-with-output-string 
       (lambda (p)
	 (display val p)))))

; Create a column entry
(define (html-table-col val)
  (sprintf #f "<TD align=right> %s </TD>" (tostring val)))

; Create an html table row from a list of entries
(define (html-table-row lst)
  (cond ((string? lst) lst)
	(else
	 (string-append
	  (sprintf #f "<TR>")
	  (apply string-append (map html-table-col lst))
	  (sprintf #f "</TR>\n")))))

; Create an html table from a list of rows, each containing 
;   a list of column entries
(define (html-table hdrlst llst)
  (string-append
   (html-table-header hdrlst)
   (apply string-append (map html-table-row llst))
   (html-table-footer)))

(define (html-table-headcol val)
  (sprintf #f "<TH justify=center> %s </TH>" (tostring val)))

(define (html-table-header vec)
   (apply string-append "<TABLE cellspacing=10 rules=\"rows\">\n" (map html-table-headcol vec)))

(define (html-table-footer)
   (sprintf #f "</TABLE>"))
