;;; $Id$
;;;;;;;;;;;  QIF Parsing  ;;;;;;;;;;;;;;

(use-modules (ice-9 slib))
(require 'hash-table)

(define qif-txn-list '())

(define qif-txn-structure 
  (make-record-type 
   "qif-txn" 
   '(memo date id payee addresslist amount status category splitlist)))

(define thetxn
  ((record-constructor qif-txn-structure)
   #f #f #f #f #f #f #f #f #f))

(define (txnupdate txn field value)
  ((record-modifier qif-txn-structure field) txn value))

(define (txnget txn field)
  ((record-accessor qif-txn-structure field) txn))

(define addresslist '())

(define (read-qif-file file account-group)
  (set! qif-txn-list '())		; Reset the transaction list...
  (set! thetxn   ((record-constructor qif-txn-structure)
		  #f #f #f #f #f #f #f #f #f))
  (resetdates)  ;  Reset the date checker
  (let*
      ((infile (open-input-file file)))
    (let loop
	((line (read-line infile)))
      (if
       (eof-object? line) #f
       (let
	   ((newline (read-qiffile-line line)))
	 (loop (read-line infile)))))
    (if 
     (checkdatemaxes)
     #f   ;;; Do nothing; all is ok
     (begin
       (display "Problem with dating - ambiguous data!")
       (newline)))
      ;;; Now, return results:
    qif-txn-list))

(define (process-qif-file file account-group)
  ; Opens file, rewrites all the lines, closes files
  (display (string-append "rewriting file:" file)) (newline)
  (let*
      ((qif-txn-list (read-qif-file file account-group))
       (category-analysis (analyze-qif-transaction-categories qif-txn-list))
;       (outfile (open-output-file (string-append file ".XAC") 'replace))
       (outfile (open-output-file (string-append file ".XAC")))
       (write-to-output-thunk 
	(lambda (txn) 
	  (write txn outfile)
	  (newline outfile))))

    (display (string-append ";;;; Data from " file) outfile)
    (newline outfile)
    (newline outfile)
    (display ";;; Transactional data:" outfile)
    (newline outfile)
    (display "(define transactions '(" outfile)
    (newline outfile)
    (for-each write-to-output-thunk qif-txn-list)
    (display (string-append 
	      "Total transactions: " 
	      (number->string (length qif-txn-list))))
    (newline)
    (display ")) ;;; End of transaction data" outfile)
    (newline outfile)
    (newline outfile)
    (display "(define acclist")
    (display (gnc:get-account-list account-group))
    (display ")")
    (newline)
    (display "(define acclist")
    (display (gnc:get-incomes-list account-group))
    (display ")")
    (newline)
    (display "(define category-analysis '" outfile)
    (hash-for-each (lambda (key value)
		     (write key outfile)
                     (write value outfile)
		     (newline outfile)) 
		   category-analysis)
    (display ")" outfile)
    (display "(define category-analysis '")
    (hash-for-each (lambda (key value)
		     (write key)
                     (write value)
		     (newline)) 
		   category-analysis)
    (display ")")
    (newline outfile)
    (close-output-port outfile)))

(define (read-qiffile-line line)
;  (display (string-append "Line:" line)) (newline)
  (if
   (char=? (string-ref line 0) #\!)   ;;; Starts with a !
   (newqifstate line))                      ;;; Jump to a new state...
  (cond 
   ((eq? qifstate 'txn)             ;;; If it's a transaction
    (rewrite-txn-line (striptrailingwhitespace line)))
   (else
    (display "Ignoring non-transaction:") (display qifstate)(newline))))
    

(define (transnull line)
  #f)  ;  do nothing with line

(define (oops-new-command-type line)
  (display (string-append "Oops: New command type!" line)) 
  (newline))

(define (rewrite-txn-line line)
  (let*
      ((fchar (string-ref line 0))
       (found (hashv-ref trans-jumptable fchar)))
    (if found
	(found line)
	(oops-new-command-type line))))

;;;; At the end of a transaction, 
;;;; Insert queued material into "thetxn" (such as splits, address)
;;;; Add "thetxn" to the master list of transactions,
;;;; And then clear stateful variables.
(define (end-of-transaction line)   ; End of transaction
  (if (not (null? addresslist))
      (txnupdate thetxn 'addresslist addresslist))
  (if splits?
      (begin
	(txnupdate thetxn 'splitlist splitlist)
	(ensure-split-adds-up)
	(resetsplits)))
  (set! qif-txn-list (cons thetxn qif-txn-list))
  (set! addresslist '())
  (set! thetxn ((record-constructor qif-txn-structure)
		#f #f #f #f #f #f #f #f #f)))

;;;;;;;;;;;  Various "trans" functions for different 
;;;;;;;;;;;  sorts of QIF lines    
(define (transmemo line)
    (txnupdate thetxn 'memo (strip-qif-header line)))

(define (transaddress line)
  (set! addresslist (cons (strip-qif-header line) addresslist)))

(define (transdate line)
  (let*
      ((date    (replacespace0 (strip-qif-header line)))
       (dpieces (split-on-somechar date #\/)))
    (txnupdate thetxn 'date date)
    (newdatemaxes dpieces))) ; collect info on date field ordering
; so we can guess the date format at
; the end based on what the population
; looks like

(define (transamt line)
  (define (numerizeamount amount-as-string)
    (let*
	((commasplit (split-on-somechar amount-as-string #\,))
	 (decommaed (apply string-append commasplit))
	 (numeric   (string->number decommaed)))
      (if
       numeric				; did the conversion succeed?
       numeric				; Yup.  Return the value
       amount-as-string)))		; Nope.  Return the original value.
  (txnupdate thetxn 'amount (numerizeamount (strip-qif-header line))))

(define (transid line)
  (txnupdate thetxn 'id (strip-qif-header line)))

(define (transstatus line)
  (txnupdate thetxn  'status (strip-qif-header line)))

(define (transpayee line)
  (txnupdate thetxn  'payee (strip-qif-header line)))

(define (transcategory line)
  (txnupdate thetxn  'category (strip-qif-header line)))

(define trans-jumptable (initialize-hashtable 37))  ;;; Need not be large

(let* 
    ((ltable
      '((#\^  end-of-transaction) 
	(#\D  transdate) 
	(#\T  transamt) 
	(#\N  transid) 
	(#\C  transstatus) 
	(#\P  transpayee)
	(#\L  transcategory) 
	(#\M  transmemo)
	(#\!  transnull) 
	(#\U  transnull)
	(#\S  transsplitcategory) 
	(#\A  transaddress) 
	(#\$  transsplitamt) 
	(#\%  transsplitpercent)
	(#\E  transsplitmemo)))
       (setter
	(lambda (lst)
	  (let ((command (car lst))
		(function (eval (cadr lst))))
	    (hashv-set! trans-jumptable command function)))))
  (for-each setter ltable))

(display "trans-jumptable")
(display trans-jumptable)
(newline)