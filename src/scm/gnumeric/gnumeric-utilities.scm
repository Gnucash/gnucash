;;;; $Id$
;;;; gnumeric-utilities.scm - Gnumeric spreadsheet generation functions

(use-modules (srfi srfi-19))

;; (gnc:depend "xml-generator.scm") -- this needs to be changed to a
;; use-modules, but since this file doesn't appear to be used right
;; now, that can wait.

;;;; Gnumeric spreadsheet consists of:
;;;; gmr:Workbook
;;;;   gmr:Summary  Done
;;;;   gmr:Geometry Done
;;;;   gmr:Sheets
;;;;    gmr:Sheet
;;;;      gmr:Name - Need the Sheet name
;;;;      gmr:MaxCol - omission OK
;;;;      gmr:MaxRow - omission OK
;;;;      gmr:Zoom - omission OK
;;;;      gmr:PrintInformation - omission OK
;;;;      gmr:Styles - Ok to omit
;;;;        gmr:StyleRegion - optional
;;;;          gmr:Style - optional
;;;;          gmr:Font - optional
;;;;          gmr:StyleBorder - optional
;;;;            gmr:Top - optional
;;;;            gmr:Bottom - optional
;;;;            gmr:Left - optional
;;;;            gmr:Right - optional
;;;;            gmr:Diagonal - optional
;;;;            gmr:Rev-Diagonal - optional
;;;;      gmr:Cols - Optional, but should have this one...
;;;;        gmr:ColInfo (No Unit MarginA MarginB HardSize Hidden)
;;;;      gmr:Rows - Quite Optional 
;;;;        gmr:RowInfo (No Unit MarginA MarginB HardSize Hidden)
;;;;      gmr:Cells - This is the meat of the matter...
;;;;        gmr:Cell (Col Row Style)
;;;;          gmr:Content

;;; Here's a kludgy function that is intended to compute the number of
;;; days since December 31, 1899.  It is only approximate; feel free
;;; to suggest a better function.
;;; The point of this is that Gnumeric uses this as the "native" data
;;; representation. 

(define (exceldate y m d)
  (let 
      ((epoch (encode-julian-day-number 31 12 1899)))
    (- (encode-julian-day-number d m y) epoch)))

;(define (ymd->number y m d)
;  (+
;   1 ;;; Start at 1
;   (* (- y 1900) 365)  ;;; 365 days per year
;   d                   ;;; Add the number of days
;   (vector-ref #(0 31 59 90 120 151 181 212 243 273 304 334) 
;	       (- m 1));;; Add in days associated with month
;   (truncate (/ (- y 1900) 4))  ;;; Add in leap days, valid 'til
;                                ;;; year 2100...
;   (if 
;    (and (= 0 (modulo y 4)) ;;; If a leap year,
;	 (> m 2))           ;;; and month is post-Feb
;    1                       ;;; add an extra day
;    0)))

;;; gmr:Summary appears to be some metadata about who/what generated
;;; the document.
(define (make-gmr-summary)
  (define (make-gmr-item name value)
    (xml-element
     'gmr:Item no-attributes
     (list (xml-element 'gmr:name no-attributes name)
	   (xml-element 'gmr:val-string no-attributes value))))
  (xml-element 
   'gmr:Summary no-attributes
   (list
    (make-gmr-item "application"
                   "gnumeric")
    (make-gmr-item "Author"
                   "GnuCash Generator"))))

;;; This function generates a goodly chunk of the document structure;
;;; gmr:Workbook is the base element for Gnumeric
(define (gnumeric-workbook sheets)
  (xml-element
   'gmr:Workbook '((xmlns:gmr . "http://www.gnome.org/gnumeric/v2"))
   (list
    (make-gmr-summary)
    (xml-element 'gmr:Geometry '((Width . 912) (Height . 720)) no-children)
    (xml-element 'gmr:Sheets no-attributes sheets))))

(define (gnumeric-xml-cell row col contents)
  (xml-element
   'gmr:Cell 
   (xml-attributes (xml-attribute 'Col col) 
		   (xml-attribute 'Row row) 
		   (xml-attribute 'Style 0))
   (list (xml-element 'gmr:Content no-attributes contents))))

;;; Generate a set of style regions for a given Sheet
;;; This ought also to support the notion of named styles, but that
;;; can wait
(define (gnumeric-styles rows colassoc)
  (xml-element 
   'gmr:Styles no-attributes
   (map 
    (lambda (coll)
      (let ((col (car coll)) 
	    (fmt (cdr coll)))
	(gnumeric-style-column rows col fmt)))
    colassoc)))

;;; Generate a StyleRegion for the given column
(define (gnumeric-style-column totalrows col format)
  (xml-element
   'gmr:StyleRegion 
   (xml-attributes (xml-attribute 'startCol col)
		   (xml-attribute 'endCol col) 
		   (xml-attribute 'startRow 0)  
		   (xml-attribute 'endRow totalrows))
   (list (xml-element 'gmr:Style 
		      (xml-attributes 
		       (xml-attribute 'Format format)) 
		      no-children))))

(define (gmr:cell col row cell-value)
  (xml-element 
   'gmr:Cell
   (xml-attributes 
     (xml-attribute 'Col col)
     (xml-attribute 'Row row))
   cell-value))

;;; Each Sheet requires Cols to define the widths of columns.
;;; Don't omit this.
(define (gnumeric-columns collist)
  (xml-element 'gmr:Cols no-attributes
		    (map (lambda (colassoc)
			   (xml-element 'gmr:ColInfo colassoc no-children))
			 collist)))

;;; And here's a function that generates a whole Sheet.
;;; It forces in style info; that's probably not the best thing to do.
(define (gnumeric-sheet name rows cols cells)
  (let ((namelst (xml-element 'gmr:Name no-attributes name))
	(stylelst (gnumeric-styles
		   rows our-style-list))
	(celllst  (xml-element 'gmr:Cells no-attributes cells)))
    (xml-element 'gmr:Sheet no-attributes
		 (list
		  namelst
		  cols
		  stylelst
		  celllst))))

;;; Define some wild accounting-oriented display formats
(define our-style-list 		   
  (let ((acctgstyle "_($*#,##0.00_);_($(#,##0.00);_($*&quot;-&quot;??_);(@_)")
	(coloredstyle "$0.00_);[Red]($0.00)"))
    (list (cons 0 "yyyy-mm-dd")
	  (cons 2 acctgstyle)
	  (cons 3 coloredstyle))))

(define (gen-cells-for-txn txn row)
  (display txn) (newline)
  (apply 
   (lambda (y m d descr amt)
     (list 
      (gmr:cell 0 row (exceldate y m d))
      (gmr:cell 1 row descr)
      (gmr:cell 2 row amt)
      (gmr:cell 3 row (string-append "=D" (number->string row)
				     "+C"
				     (number->string  (+ row 1))))))
   txn))

(define (sample-cells)
  (let loop
      ((txns 
	(sort
	 (append 
	  '((1998 12 31 "Opening Balance" 0))
	  (map (lambda (x) (list 1999 x 1 "Rent" -500)) 
	       '(1 2 3 4 5 6 7 8 9 10 11 12))
	  (map (lambda (x) (list 1999 x 1 "Salary" 1200)) 
	       '(1 2 3 4 5 6 7 8 9 10 11 12))
	  (map (lambda (x) (list 1999 x 15 "Salary" 1200)) 
	       '(1 2 3 4 5 6 7 8 9 10 11 12))
	  (map (lambda (x) (list 1999 x 12 "Phone" -35)) 
	       '(1 2 3 4 5 6 7 8 9 10 11 12)))
	 (lambda (lst1 lst2)
	   (if (= (car lst1) (car lst2))
	       (if (= (cadr lst1) (cadr lst2))
		   (if (= (caddr lst1) (caddr lst2))
		       (if (string=? (cadddr lst1) (cadddr lst2))
			   #t
			   (string<? (cadddr lst1) (cadddr lst2)))
		       (< (caddr lst1) (caddr lst2)))
		   (< (cadr lst1) (cadr lst2)))
	       (< (car lst1) (car lst2))))))
       (row 1)
       (cells '()))
    (if (null? txns)
	cells
	(loop (cdr txns)
	      (+ row 1)
	      (let* ((txn (car txns)))
		(append cells (gen-cells-for-txn txn row)))))))

(define (build-full-sample)
  (let* 
      ((cells (sample-cells))
       (cols 4)
       (collist '(((No . 0) (Unit . 85))
		  ((No . 1) (Unit . 150))
		  ((No . 2) (Unit . 75))
		  ((No . 3) (Unit . 75))))
       (rows (/ (length cells) cols))
       (cols (gnumeric-columns collist))
       (sheet (gnumeric-sheet "Sample" rows cols cells))
       (sheets (list sheet)))
    (gnumeric-workbook sheets)))

;;; This function generates a whole whack of cells and formulae
(define (generate-sampl filename)
  (let ((p (open-output-file filename))
	(ss (build-full-sample)))
    (display "<?xml version=\"1.0\"?>" p)
    (output-xml-element ss p)
    (close-output-port p)))
