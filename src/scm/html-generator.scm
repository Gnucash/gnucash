;;;; $Id$
;;;; HTML Support functions

(gnc:support "html-generator.scm")

(define (html-table-row header? . items)
  (let loop
      ((cline "<TR>")
       (ilist items))
    (if (pair? ilist)
	(loop (add-html-cell header? cline (car ilist))
	      (cdr ilist))
	(string-append cline "</TR>"))))

(define (html-strong cell)
  (string-append
   "<STRONG>"
   cell
   "</STRONG>"))

(define (add-html-cell header? cline item)
  (string-append cline (make-html-cell header? item)))

(define (make-html-cell header? item)
  (let ((pre    ;;; Opening tag
	 (cond
	      (header? "<TH justify=center>")
	      ((number? item) "<TD ALIGN=RIGHT>")
	      (else "<TD>")))
	(post   ;;; Closing tag
	 (if header? "</TH>" "</TD>")))
    (sprintf #f 
	     (string-append 
	      pre   ;;; Start with opening tag
	      (cond ;;; Body 
	       ((string? item) item) 
	       ((number? item) (sprintf #f "%.2f" item))
	       (else ""))
	      post))))  ;;; closing tag

(define (make-html-cell-header item)
  (make-html-cell #t item))

(define (make-html-cell-body item)
  (make-html-cell #f item))
