;; $Id$
(gnc:support "qifs/dates-qif.scm")
(gnc:depend "substring-search.scm")

;;;;;;;  Date-related code 
(define findspace (substring-search-maker " "))

;;; Replace spaces in date fields with zeros so
;;;  "4/ 7/99" transforms to "4/07/99"
(define (replacespace0 string)
  (let
      ((slen (string-length string))
       (spacepos (findspace string)))
    (if spacepos
	(replacespace0
	 (string-append
	  (substring string 0 spacepos)
	  "0"
	  (substring string (+ 1 spacepos) slen)))
	string)))

(if testing?
    (begin
      (display "Check replacespace0:")
      (let* ((v1 "4/ 7/99")
	     (v1res (replacespace0 v1))
	     (v1exp "4/07/99")
	     (v2 "   1234 ")
	     (v2res (replacespace0 v2))
	     (v2exp "00012340"))
	(display (string-append "Rewrite:" v1 " Expect:" v1exp " Got:" v1res)) 
	(newline)
	(if (string=? v1res v1exp)
	    'ok
	    (begin
	      (display "ERROR - Unexpected results!!!")(newline)))
	(display (string-append "Rewrite:" v2 " Expect:" v2exp " Got:" v2res)) 
	(newline)
	(if (string=? v2res v2exp)
	    'ok
	    (begin
	      (display "ERROR - Unexpected results!!!")(newline))))))
	       
;;;; Check the way the dates look; figure out whether it's
;;;; DD/MM/YY, MM/DD/YY, YY/MM/DD, or whatever...
(define date-low #f)
(define date-med #f)
(define date-high #f)
(define min-date-low #f)
(define min-date-med #f)
(define min-date-high #f)
(define max-date-low #f)
(define max-date-med #f)
(define max-date-high #f)
(define (resetdates)   
  (set! date-low #f) 
  (set! date-med #f)
  (set! date-high #f)
  (set! min-date-low 9999)
  (set! min-date-med 9999)
  (set! min-date-high 9999)
  (set! max-date-low 0)
  (set! max-date-med 0)
  (set! max-date-high 0))

(define (newdatemaxes dpieces)
  (let
      ((p1 (string->number (car dpieces)))
       (p2 (string->number (cadr dpieces)))
       (p3 (string->number (caddr dpieces))))
    (if (< p1 min-date-low)
	(set! min-date-low p1))
    (if (< p2 min-date-med)
	(set! min-date-med p2))
    (if (< p3 min-date-high)
	(set! min-date-high p3))
    (if (> p1 max-date-low)
	(set! max-date-low p1))
    (if (> p2 max-date-med)
	(set! max-date-med p2))
    (if (> p3 max-date-high)
	(set! max-date-high p3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; (checkdatemaxes) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;; This is a fairly "intelligent" routine that examines the date
;;; ranges in min-date-low, max-date-low, min-date-med, max-date-med,
;;; min-date-med, max-date-med, and determines which of these fields
;;; corresponds to Day, Month, and Year.
;;; Results are stored in date-low, date-med, date-high, assigning the
;;; symbols 'mm, 'dd, and 'yy appropriately.
;;; It uses the considerations that:
;;;  - There are a maximum of 12 months in a year
;;;  - There are a maximum of 31 days in a month
;;;  - Year "0" likely indicates "Year 2000."
;;; At the point at which "Problem: Range occurs twice!" is indicated,
;;; it would be a reasonable idea to pop up a dialog to the user
;;; indicating such things as the ranges that were found (e.g. - 1-12,
;;; 2-11, 94-99), provide the "best guess" default of mm/dd/yy, and
;;; allow the user the option of overriding this as desired.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (checkdatemaxes)
  (define (favor min max)
    (cond
     ((> max 31) 'yy)   ;;; [max > 31] --> Year
     ((and (< max 32) (> max 12)) 'dd) ;;; Max in [13,31] --> Day
     ((= min 0) 'yy)    ;;; [min=0] --> Year xx00
     (else 'mm)))
  (let
      ((vl (favor min-date-low max-date-low))
       (vm (favor min-date-med max-date-med))
       (vh (favor min-date-high max-date-high)))
    (begin
      (if (or (eq? vl vm) (eq? vl vh) (eq? vm vh))
	  (begin
	    (display "Problem: Range occurs twice!")
	    (newline)
	    (display "Low Values:(Low Medium High)") 
	    (display (list min-date-low min-date-med min-date-high)) (newline)
	    (display "High Values:(Low Medium High)") 
	    (display (list max-date-low max-date-med max-date-high)) (newline)
	    (display 
	     (string-append 
	      "(VL VM VH) (" 
	      (symbol->string vl) 
	      " " 
	      (symbol->string vm) 
	      " " (symbol->string vh) ")" ))
	    (newline)
	    (display "Assuming common default of MM/DD/YY")
	    (newline)
	    (set! date-low 'mm)
	    (set! date-med 'dd)
	    (set! date-high 'yy)
	    ;; This would be a great place to put a "hook" to allow the 
	    ;; user to interactively set (date-low, date-med, date-high)
	    ;; to their favorite permuatation of ('mm 'dd 'yy)
	    )
	  (begin
	    (set! date-low vl)
	    (set! date-med vm)
	    (set! date-high vh))))))

(define (rewrite-dates txn)
  (cond
   ((atom? txn) txn)
   ((pair? txn)       ; If it's a pair, see if it's a date...
    (if (eq? (car txn) 'date)
	(cons 'date (reformat-date (cdr txn)))
	txn))
   ((list? txn)       ; List? - Split and process pieces
    (cons (rewrite-dates (car txn))
	  (rewrite-dates (cdr txn))))))

(define (date-window year)
  (let ((window-range 80)     ;;;; Date adjustment window
	(first-century 100)   ;;;; First century
	(next-century 2000)   ;;;; Add this to year values that are
	                      ;;;; less than the value of
         		      ;;;; window-range. 
	(this-century 1900))  ;;;; Add this-century to year values
		              ;;;; that are greater than window-range,
		              ;;;; and less than first-century
    
        ;Based on this set of parameters, the following year substitutions
	;would take place:
	      ;YEAR --> New Value
	      ;  00 --> 2000
	      ;  70 --> 2070
	      ;  85 --> 1985
	      ;  99 --> 1999
	      ; 100 -->  100
	      ;1102 --> 1102
	      ;1932 --> 1932
	      ;
	      ; Changing window-range changes the cut-off between last
	      ;  century and this one; somewhere around 100 years from
	      ;  now, it will probably be appropriate to change
	      ;  next-century to 2100, and this-century to 2000.
    (cond
     ((< year window-range)
      (+ year next-century))    
     ((and (> year window-range) (< year first-century))
      (+ year this-century))
     (else   ;;; Otherwise, do nothing to the year.
      year))))

;;; does string contain #\- or #\/ or #\.???
(define date-delimiters-list '(#\- #\/ #\.))

(define (which-delimiter str charlist)
  (let ((len (string-length str)))  ;;; Compute length once
    (let loop ((pos 0))
      (let ((cchar (string-ref str pos)))
	(if (member cchar charlist)
	    cchar
	    (if (< pos len)
		(loop (+ pos 1))))))))

(testing "which-delimiter"
	 "99/01/03"
	 #\/
	 (which-delimiter "99/01/03" date-delimiters-list))

(testing "which-delimiter"
	 "99/01/03"
	 #\/
	 (which-delimiter "99/01/03" date-delimiters-list))

(testing "which-delimiter"
	 "99.02.03"
	 #\.
	 (which-delimiter "99.02.03" date-delimiters-list))

(testing "which-delimiter"
	 "12345-"
	 #\-
	 (which-delimiter "12345-" date-delimiters-list))

(define (reformat-date date-as-string)
  (let*
      ((delimiter (which-delimiter date-as-string date-delimiters-list))
       (datesplitup (split-on-somechar date-as-string delimiter))
       (p1 (string->number (car datesplitup)))
       (p2 (string->number (cadr datesplitup)))
       (p3 (string->number (caddr datesplitup)))
       (YEAR  0)
       (MONTH 0)
       (DAY   0)
       (dropin (lambda (yy-or-mm-or-dd value)
		 (cond   
		  ((eq? yy-or-mm-or-dd 'yy)
		   (set! YEAR value))
		  ((eq? yy-or-mm-or-dd 'mm)
		   (set! MONTH value))
		  ((eq? yy-or-mm-or-dd 'dd)
		   (set! DAY value))))))
    (begin
      (dropin date-low p1)
      (dropin date-med p2)
      (dropin date-high p3)
      (list (date-window YEAR) MONTH DAY))))

	   

(if testing?
    (begin
      (let 
	  ((ambdatelist			; ambiguous; date-versus-month
	    '(("00" "01" "02")		; is not clear, as both are < 12
	      ("97" "02" "03")
	      ("99" "04" "07"))))
	(resetdates)
	(for-each newdatemaxes ambdatelist)
	(display "Testing date conversion based on ambiguous date list:") (newline)
	(display "(ambdatelist ") (display ambdatelist) (display ")") (newline)
	(checkdatemaxes)
	(display "Results: ")
	(display (list date-low date-med date-high)) (newline))
      (let 
	  ((ambdatelist			; also ambiguous
	    '(("13" "02" "02")	
	      ("02" "03" "03")
	      ("03" "04" "07"))))
	(resetdates)
	(for-each newdatemaxes ambdatelist)
	(display "Testing date conversion based on ambiguous date list:") (newline)
	(display "(ambdatelist ") (display ambdatelist) (display ")") (newline)
	(checkdatemaxes)
	(display "Results: ")
	(display (list date-low date-med date-high)) (newline))

      (let 
	  ((datelist			; not ambiguous
	    '(("13" "00" "02")	
	      ("02" "03" "03")
	      ("03" "04" "07"))))
	(resetdates)
	(for-each newdatemaxes datelist)
	(display "Testing date conversion based on ambiguous date list:") (newline)
	(display "(datelist ") (display datelist) (display ")") (newline)
	(checkdatemaxes)
	(display "Results: ")
	(display (list date-low date-med date-high)) (newline))))

      
