;;;;;;;;;;;  QIF Parsing  ;;;;;;;;;;;;;;
(define tlist '())
(define atrans '())
(define addresslist '())

(define process-qif-file
  (lambda (file account-group)    
  ; Opens file, rewrites all the lines, closes files
    (display (string-append "rewriting file:" file)) (newline)
    (set! tlist '())   ; Reset the transaction list...
    (set! atrans '())
    (reset-categories)
    (resetdates)  ;  Reset the date checker
    (let*
	;((infile (open-input-file (string-append srcdir file)))
	((infile (open-input-file file))
;	 (outfile (open-output-file (string-append destdir file) 'replace))
	 (outfile (open-output-file (string-append file ".XAC")))
	 (write-to-output-thunk 
	  (lambda (txn) 
	    (write (rewrite-dates txn) outfile) 
	    (newline outfile)))) 
      (begin
	(display (string-append ";;;; Data from " file) outfile)
	(newline outfile)
	(newline outfile))
      (let loop
	  ((line (read-line infile)))
	(if
	 (eof-object? line) #f
	 (let
	     ((newline (rewrite-line line)))
	   (loop (read-line infile)))))
      (if 
       (checkdatemaxes)
       #f
       (begin
	 (display "Problem: Illegal date format!") (newline)
	 (display ";;;; Problem - date format conflict!" outfile)
	 (newline outfile)))
      (display ";;; Transactional data:" outfile)
      (newline outfile)
      (display "(define transactions '(" outfile)
      (newline outfile)
      (for-each write-to-output-thunk tlist)
      (display (string-append 
		"Total transactions: " 
		(number->string (length tlist))))
      (newline)
      (display ")) ;;; End of transaction data" outfile)
      (newline outfile)
      (display "(define categories '" outfile)
      (write kept-categories outfile)
      (display ")" outfile) 
      (newline outfile)
      (display (string-append 
		"Total categories: " 
		(number->string (length kept-categories))))
      (newline)
      (display "(define acclist")
      (display (acclist account-group))
      (display ")")
      (newline)
      (display "(define acclist")
      (display (catlist account-group))
      (display ")")
      (newline)
      (let*
	  ((acclist (acclist account-group))
	   (catlist (catlist account-group))
	   (guesses (guess-corresponding-categories kept-categories catlist acclist)))
	(display "(define cattrans '" outfile)
	(write guesses outfile)
	(display ")" outfile)
	(newline outfile))
	  
      (close-input-port infile)
      (close-output-port outfile))))

;;; Rewrite a line
(define qifstate '())

(define rewrite-line 
  (lambda (line)
    (if
     (string=? (substring line 0 1) "!")   ;;; Starts with a !
     (newstate line))                      ;;; Jump to a new state...
    (if (equal? qifstate 'txn)             ;;; If it's a transaction
	(rewrite-txn-line (striptrailingwhitespace line)))))   ;;; Rewrite it
       ;;; otherwise, do nothing...

(define QIFstates  
  '(("!Type:Cat" . 'category)
    ("!Option:AutoSwitch" . 'accounts)
    ("!Clear:AutoSwitch"  . 'account)
    ("!Account" . 'accounts)
    ("!Type:Memorized" . 'memorized)
    ("!Type:Bank" . 'txn)
    ("!Type:CCard" . 'txn)
    ("!Type:Oth A" . 'txn)))

;;;;   Strip off trailing whitespace
(define (striptrailingwhitespace line)
  (let
      ((stringsize (string-length line)))
    (if
     (< stringsize 1)
     ""
     (let*
	 ((lastchar (string-ref line (- stringsize 1))))
       (if
	(char-whitespace? lastchar)
	(striptrailingwhitespace (substring line 0  (- stringsize 1)))
	line)))))

(define (newstate line)
  (let*
      ((statepair (assoc (striptrailingwhitespace line) QIFstates)))
    (begin
      (if
       (pair? statepair)
       (set! qifstate (car (cddr statepair)))
       #f))))

(define (transnull line)
  #f)  ;  do nothing with line

(define (oops-new-command-type line)
  (write "Oops: New command type!")
  (write line))

(define (rewrite-txn-line line)
  (let*
      ((fchar (substring line 0 1))
       (found (assoc fchar trans-jumptable)))
    (if
     found
     (let 
	 ((tfunction (cdr found)))
       (tfunction line))
     (oops-new-command-type line))))

;;;; Category management
(define kept-categories '())

(define (reset-categories)        ;; reset the list
  (set! kept-categories '()))

;;;;(keep-category-for-summary category)
(define (keep-category-for-summary category)
  (let
      ((found (assoc category kept-categories)))
    (if
     found
     (set-cdr! found (+ (cdr found) 1))
     (set! kept-categories (cons (cons category 1) kept-categories)))))

;;; Is the account a QIF "category"? 
(define (account-category? category)
  (and
   (string=? (substring category 0 1) "[")
   (let
       ((len (string-length category)))
     (string=?
      (substring category (- len 1) len) "]"))))

;;;; "numerizeamount" takes the commaed string that QIF provides,
;;;; removes commas, and turns it into a number.
(define (numerizeamount amount-as-string)
  (let*
      ((commasplit (split-on-somechar amount-as-string #\,))
       (decommaed (apply string-append commasplit))
       (numeric   (string->number decommaed)))
    (if
     numeric    ; did the conversion succeed?
     numeric    ; Yup.  Return the value
     amount-as-string)))   ; Nope.  Return the original value.

;;;; At the end of a transaction, 
;;;; Insert queued material into "atrans" (such as splits, address)
;;;; Add "atrans" to the master list of transactions,
;;;; And then clear stateful variables.
(define (end-of-transaction line)   ; End of transaction
  (if (not (null? addresslist))
      (set! atrans (cons (cons 'address addresslist) atrans)))
  (if splits?
      (begin
	(set! atrans (cons (cons 'splits splitlist) atrans))
	(ensure-split-adds-up)))
  (set! tlist (cons atrans tlist))
  (set! addresslist '())
  (resetsplits)
  (set! atrans '()))

;;;;;;;;;;;  Various "trans" functions for different 
;;;;;;;;;;;  sorts of QIF lines    
(define (transmemo line)
  (let*
      ((linelen (string-length line))
       (memo    (substring line 1 linelen)))
    (set! atrans (cons (cons 'memo memo) atrans))))

(define (transaddress line)
  (let*
      ((linelen (string-length line))
       (addline    (substring line 1 linelen)))
    (set! addresslist (cons addline addresslist))))

(define (transdate line)
  (let*
      ((linelen (string-length line))
       (date    (replacespace0 (substring line 1 linelen)))
       (dpieces (split-on-somechar date #\/)))
    (set! atrans (cons (cons 'date date) atrans))
    (newdatemaxes dpieces))) ; collect info on date field ordering
; so we can guess the date format at
; the end based on what the population
; looks like

(define (transamt line)
  (let*
      ((linelen (string-length line))
       (amount  (numerizeamount (substring line 1 linelen))))
    (set! atrans (cons (cons 'amount amount) atrans))))

(define (transid line)
  (let*
      ((linelen (string-length line))
       (id    (substring line 1 linelen)))
    (set! atrans (cons (cons 'id id) atrans))))

(define (transstatus line)
  (let*
      ((linelen (string-length line))
       (status    (substring line 1 linelen)))
    (set! atrans (cons (cons 'status status) atrans))))

(define (transpayee line)
  (let*
      ((linelen (string-length line))
       (payee    (substring line 1 linelen)))
    (set! atrans (cons (cons 'payee payee) atrans))))

(define (transcategory line)
  (let*
      ((linelen (string-length line))
       (category    (substring line 1 linelen)))
    (keep-category-for-summary category)
    (set! atrans (cons (cons 'category category) atrans))))

(define 
  trans-jumptable
  (list 
   (cons "^"  end-of-transaction) 
   (cons "D"  transdate) 
   (cons "T"  transamt) 
   (cons "N"  transid) 
   (cons "C"  transstatus) 
   (cons "P"  transpayee)
   (cons "L"  transcategory) 
   (cons "M"  transmemo)
   (cons "!"  transnull) 
   (cons "U"  transnull)
   (cons "S"  transsplitcategory) 
   (cons "A"  transaddress) 
   (cons "$" transsplitamt) 
   (cons "%" transsplitpercent)
   (cons "E"  transsplitmemo)))
