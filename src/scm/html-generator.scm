;;;; $Id$
;;;; HTML Support functions 

;; primarily Bryan Larsen (blarsen@ada-works.com) with help from
;; pretty much everybody involved with reports.

(gnc:support "html-generator.scm")

 
;; How this mechanism works:
;;
;; To do a report, first collect all of your results into a list.
;; Each item in the list corresponds to one entry.  One entry may
;; correspond to more than one line in the report, though.
;; 
;; Assemble a list of report-spec-structure's.  Each entry in the
;; report-spec-structure corresponds to one column in the HTML report.
;; Perhaps the most important parameter in the structure is
;; get-value-proc, which extracts the value to print in the column
;; from the entry.
;;
;; If total-proc and total-html-proc are defined, the column is totalled.
;;
;; Subentries are handled several different ways, depending on what
;; function is used to convert the results into an html table.  If
;; subs-list-proc and subentry-html-proc are #f, there are no
;; subentries in this column.  
;;
;; Subsections (which are not yet implemented) are defined in the
;; report-sort-spec-structure.  Define subtotal-html-proc to allow
;; this column to be totalled.
;;
;; Note that pretty much every parameter may be set to #f.  For
;; example, to define a "total column", you may wish to add an entry
;; to the spec list that sets html-proc to #f, but sets
;; total-html-proc and subtotal-html-proc.  This way, subtotals really
;; stand out.
;;
;; 
;; report-spec-structure 
;;  header: string describing the column
;;  get-value-proc:  given the entry, finds the value
;;  html-proc: converts the value into html
;;  total-proc:  usually + or #f
;;  subtotal-html-proc: converts the subtotal into html
;;  total-html-proc: converts the total into html
;;  first-last-preference: #t if, for this column, entries should be
;;                         displayed before subentries.  #f is
;;                         subentries before entries.  This parameter
;;                         may be ignored, depending on the report
;;                         style chosen.
;;  subs-list-proc: a procedure that returns a list of subentry values
;;  subentry-html-proc: converts a subentry value into html
(define report-spec-structure
  (make-record-type
   "report-spec-structure"
   '(header get-value-proc html-proc total-proc
	    subtotal-html-proc total-html-proc
	    first-last-preference subs-list-proc subentry-html-proc)))

;; The proposed sorting mechanism.  Of course, if you just wanted it
;; sorted, you could sort the list before converting it into HTML.
;; However, by doing it this way, we can divide things into
;; subsections as well.
;;
;; To sort, collect a list of report-sort-spec-structure's.  The first
;; item in the list is the primary sort, and so on down.
;;
;; Optionally, one can divide the report into subsections.  To do so,
;; set the subsection-pred.  subsection-pred returns true if two
;; values are in the same subsection.  All values in the subsection
;; must be adjacent for the sort-pred.  For example, one could sort by
;; date, and then supply a subsection-pred that determines whether two
;; dates are within the same month.
;;
;; report-sort-spec-structure
;;  get-value-proc:  given the entry, finds the value.  Required.
;;  sort-pred:  usually <.  Required.
;;  equal-pred: usually =.  Required.  This is used during sorting for
;;              multi-key sorting.
;;  subsection-pred: often = or #f.  Returns #t if both values are in
;;                   the same subsection
;;  subsection-title-proc: returns the title of the subsection given a
;;                         value.  #f indicates no title.
(define report-sort-spec-structure
  (make-record-type
   "report-sort-spec-structure"
   '(get-value-proc sort-pred equal-pred subsection-pred
		    subsection-title-proc)))

(define make-report-sort-spec
  (record-constructor report-sort-spec-structure))

(define report-sort-spec-get-get-value-proc
  (record-accessor report-sort-spec-structure 'get-value-proc))

(define report-sort-spec-get-sort-pred
  (record-accessor report-sort-spec-structure 'sort-pred))

(define report-sort-spec-get-equal-pred
  (record-accessor report-sort-spec-structure 'equal-pred))

(define report-sort-spec-get-subsection-pred
  (record-accessor report-sort-spec-structure 'subsection-pred))

(define report-sort-spec-get-subsection-title-proc
  (record-accessor report-sort-spec-structure 'subsection-title-proc))

(define make-report-spec
  (record-constructor report-spec-structure))

(define report-spec-get-header
  (record-accessor report-spec-structure 'header))

(define report-spec-get-get-value-proc
  (record-accessor report-spec-structure 'get-value-proc))

(define report-spec-get-html-proc
  (record-accessor report-spec-structure 'html-proc))

(define report-spec-get-total-proc
  (record-accessor report-spec-structure 'total-proc))

(define report-spec-get-subtotal-html-proc
  (record-accessor report-spec-structure 'subtotal-html-proc))

(define report-spec-get-total-html-proc
  (record-accessor report-spec-structure 'total-html-proc))

(define report-spec-get-subs-list-proc
  (record-accessor report-spec-structure 'subs-list-proc))

(define report-spec-get-subentry-html-proc
  (record-accessor report-spec-structure 'subentry-html-proc))

(define report-spec-get-first-last-preference
  (record-accessor report-spec-structure 'first-last-preference))

;; convert a list of entries into html
(define (html-table-render-entries entry-list specs sort-specs line-render-proc count-subentries-proc)
  (html-table-do-subsection
   (html-table-sort entry-list sort-specs)
   specs sort-specs line-render-proc count-subentries-proc 1))

;; the next 3 functions can be passed to html-table-render-entries

;; convert an entry into html.  subentries follow entries
(define (html-table-entry-render-entries-first line specs count-subentries-proc)
  (html-table-row-group
   (cons
    (html-table-row-manual (html-table-do-entry line specs))
    (map
     html-table-row-manual
     (html-table-collect-subentries line specs count-subentries-proc)))))

;; convert an entry into html.  first subentry is merged with the entry
(define (html-table-entry-render-subentries-merged line specs count-subentries-proc)
  (let ((subs-lines (html-table-collect-subentries line specs count-subentries-proc)))
    (html-table-row-group
     (if (null? subs-lines)
	 (html-table-row-manual (html-table-do-entry line specs))
	 (list
	  (html-table-row-manual
	   (map
	    (lambda (entry sub)
	      (if (not sub) entry sub))
	    (html-table-do-entry line specs)
	    (car subs-lines)))
	  (map html-table-row-manual (cdr subs-lines)))))))


;; convert an entry into html.  ignore sub entries
(define (html-table-entry-render-entries-only line specs count-subentries-proc)
  (html-table-row-group
   (html-table-row-manual (html-table-do-entry line specs))))

;; convert totals to html
(define (html-table-totals lst specs)
  (html-table-totals-row
   (map
    (lambda (spec)
      (cond ((report-spec-get-total-html-proc spec)
	     ((report-spec-get-total-html-proc spec)
	      (apply
	       (report-spec-get-total-proc spec)
	       (map (report-spec-get-get-value-proc spec) lst))))
	    (else #f)))
    specs)))

;; convert headers to html
(define (html-table-headers specs)
  (html-table-headers-row
   (map
    (lambda (spec) 
      (html-header-cell 
       (report-spec-get-header spec)))
    specs)))

;;;;;;;;;;;;;;;;
;; the rest are just helper functions

;; convert subtotals to html
(define (html-table-subtotals lst sort-spec specs depth)
  (html-table-subtotals-row
   depth
   (map
    (lambda (spec)
      (cond ((report-spec-get-subtotal-html-proc spec)
	     ((report-spec-get-subtotal-html-proc spec)
	      (apply
	       (report-spec-get-total-proc spec)
	       (map (report-spec-get-get-value-proc spec) lst))))
	    (else #f)))
    specs)))



(define (html-table-sort lst sort-specs)
  (sort lst (html-table-make-sort-pred sort-specs)))

(define (html-table-do-subsection lst specs sort-specs line-render-proc count-subentries-proc depth)
  (cond
   ((null? sort-specs)
    (map 
     (lambda (line) (line-render-proc line specs count-subentries-proc))
     lst))
   (else
    (let loop ((lst2 lst))
      (cond 
       ((null? lst2) '())
       (else
	(let* ((front '())
	       (back '())
	       (sort-spec (car sort-specs))
	       (subsection-pred (report-sort-spec-get-subsection-pred sort-spec))
	       (get-value-proc (report-sort-spec-get-get-value-proc sort-spec))
	       (value1 (get-value-proc (car lst2))))
	  (cond 
	   (subsection-pred
	    (set! front
		  (remove-if-not
		   (lambda (line)
		     (subsection-pred value1 (get-value-proc line)))
		   lst2))
	    (set! back (set-difference lst2 front)))
	   (else
	    (set! front lst2)
	    (set! back '())))
	  (list
	   (cond ((report-sort-spec-get-subsection-title-proc sort-spec)
		  (html-table-subsection-title 
		   ((report-sort-spec-get-subsection-title-proc sort-spec)
		    (get-value-proc (car front)))
		   depth))
		 (else '()))
	   (html-table-do-subsection 
	    front specs (cdr sort-specs) line-render-proc count-subentries-proc (+ depth 1))
	   (cond (subsection-pred
		  (html-table-subtotals front sort-spec specs depth))
		 (else '()))
	   (loop back)))))))))
	    
     
	   
(define (html-table-make-sort-pred sort-specs)
  (lambda (entry1 entry2)
    (let loop ((specs sort-specs))
      (cond ((null? specs) #f)
	    (else
	     (let* ((spec (car specs))
		    (gv-proc (report-sort-spec-get-get-value-proc spec))
		    (value1 (gv-proc entry1))
		    (value2 (gv-proc entry2)))
	       (cond (((report-sort-spec-get-sort-pred spec) value1 value2) #t)
		     (((report-sort-spec-get-equal-pred spec) value1 value2)
		      (loop (cdr specs)))
		     (else #f))))))))

;; converts from col order to row order.
;; ex.  ((a b) (c d) (e f)) -> ((a c e) (b d f))
(define (col-list->row-list lst)
  (apply map list lst))

;; converts subentries into html and collects into a list of lists of
;; html cells.
(define (html-table-collect-subentries line specs count-subentries-proc)
  (col-list->row-list
   (map
    (lambda (spec)
      (cond ((report-spec-get-subs-list-proc spec) 
	     (map
	      (report-spec-get-subentry-html-proc spec)
	      ((report-spec-get-subs-list-proc spec) line)))
	    (else (gnc:map-for
		   (lambda (n) #f)
		   0 (count-subentries-proc line) 1))))
    specs)))

;; converts entry into a list of html cells.
(define (html-table-do-entry line specs)
  (map
   (lambda (spec)
     (cond ((report-spec-get-get-value-proc spec)
	    ((report-spec-get-html-proc spec)
	     ((report-spec-get-get-value-proc spec) line)))
	   (else #f)))
   specs))

(define (html-table-headers-row headers)
  (list
   "<TR bgcolor=#ffccff cellspacing=10 rules=\"rows\">"
   headers
   "</TR>\n"))

(define (html-table-totals-row cells)
  (list
   "<TR bgcolor=#ffccff cellspacing=10 rules=\"rows\">"
   (map
    (lambda (cell)
      (cond (cell cell)
	    (else html-blank-cell)))
    cells)
   "</TR>\n"))

(define (html-table-subtotals-row depth cells)
  (list
   "<TR bgcolor=" 
   (number->string (+ #xffff40 (* depth #x30)) 16)
   "cellspacing=10 rules=\"rows\">"
   (map
    (lambda (cell)
      (cond (cell cell)
	    (else html-blank-cell)))
    cells)
   "</TR>\n"))
   

(define (html-table-row-manual items)
  (list
   "<TR bgcolor=" html-table-group-color ">"
   (map
    (lambda (cell)
      (cond (cell cell)
	    (else html-blank-cell)))
    items)
   "</TR>\n"))

(define (html-table-subsection-title title depth)
  (list "<TR bgcolor=#"
	   (number->string (+ #xffff40 (* depth #x30)) 16)
	   "><TH>" title "</TH></TR>"))

;; help! this doesn't work!  I want something to group several rows
;; together so that an "entry" is noticably one unit.
;; netscape & our html widget do not support tbody.
;;(define (html-table-row-group rows)
;;  (list  "</TR><TBODY>" rows  "</TBODY>"))

(define html-table-group-color "#99ccff")

(define (html-table-row-group row)
  (if (string=? html-table-group-color "#99ccff")
      (set! html-table-group-color "#ffffff")
      (set! html-table-group-color "#99ccff"))
  row)

(define (html-strong html)
  (if html 
      (string-append "<STRONG>" html "</STRONG>")
      #f))

(define (html-make-strong proc)
  (lambda (val)
    (html-strong (proc val))))

(define (html-ital html)
  (if html
      (string-append  "<i>"  html "</i>")
      #f))

(define (html-make-ital proc)
  (lambda (val)
    (html-ital (proc val))))

(define (html-currency amount)
  (if amount
      (string-append
       "<font face=\"Courier\""
       (if (< amount 0)
	   (string-append
	    "color=#ff0000>("
	    (gnc:amount->string (- amount) #f #t #f)
	    ")")
	   (string-append
	    ">&nbsp;"
	    (gnc:amount->string amount #f #t #f)
	    "&nbsp;"))
       "</font>")
      #f))

(define (html-left-cell item)
  (if item
      (string-append "<TD>" item "</TD>")
      #f))

(define (html-make-left-cell proc)
  (lambda (val)
    (html-left-cell (proc val))))

(define (html-right-cell item)
  (if item
      (string-append "<TD align=right>" item "</TD>")
      #f))

(define html-blank-cell
  "<TD></TD>")

(define (html-make-right-cell proc)
  (lambda (val)
    (html-right-cell (proc val))))

(define (html-header-cell item)
  (string-append "<TH justify=left>" item "</TH>"))

(define (html-string string)
  (if string string #f))

(define (html-number format number)
  (if number (sprintf #f format number) #f))

(define (html-para text)
  (string-append "<P>" text "</P>\n"))

(define (html-start-document-title title)
  (list 
   "<HTML>"
   "<HEAD>"
   "<TITLE>" title "</TITLE>"
   "</HEAD>"
   "<BODY bgcolor=#99ccff>"))

(define (html-start-document-color color)
  (list 
   "<HTML>"
   "<BODY bgcolor=" color ">"))

(define (html-start-document)
  (list 
   "<HTML>"
   "<BODY bgcolor=#99ccff>"))

(define (html-end-document)
  (list
   "</BODY>"
   "</HTML>"))

(define (html-start-table)
  (list "<TABLE>"))  ;; border=2 rules=\"groups\"

(define (html-end-table)
  (list "</table>"))


;;;;;;;;;;;;;;;;;;;;
;; HTML Table
;; This is used by balance-and-pnl.
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
  (string-append "<TD align=right>" (tostring val) "</TD>"))

(define (html-table-col-align val align)
  (string-append "<TD align=" align ">" (tostring val) "</TD>"))

; Create an html table row from a list of entries
(define (html-table-row lst)
  (cond ((string? lst) lst)
	(else
	 (string-append
	  "<TR>"
	  (apply string-append (map html-table-col lst))
	  "</TR>"))))

; Create an html table row from a list of entries
(define (html-table-row-align lst align-list)
  (cond ((string? lst) lst)
	(else
	 (string-append
	  "<TR>"
	  (apply string-append (map html-table-col-align lst align-list))
	  "</TR>"))))

; Create an html table from a list of rows, each containing 
;   a list of column entries
(define (html-table hdrlst llst)
  (string-append
   (html-table-header hdrlst)
   (apply string-append (map html-table-row llst))
   (html-table-footer)))

(define (html-table-headcol val)
  (string-append "<TH justify=center>" (tostring val) "</TH>"))

(define (html-table-header vec)
   (apply string-append "<TABLE cellspacing=10 rules=\"rows\">\n"
          (map html-table-headcol vec)))

(define (html-table-footer)
  "</TABLE>")
