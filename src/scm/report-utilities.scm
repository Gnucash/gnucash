;;; $Id$
;;; Reporting utilities
(gnc:support "report-utilities.scm")

(define (gnc:account-separator-char)
  (let ((option (gnc:lookup-option gnc:*options-entries*
                                   "General" "Account Separator")))
    (if option
        (case (gnc:option-value option)
          ((colon) ":")
          ((slash) "/")
          ((backslash) "\\")
          ((dash) "-")
          ((period) ".")
          (else ":"))
        ":")))

;; get a full account name
(define (gnc:account-get-full-name account)
  (let ((separator (gnc:account-separator-char)))
    (cond ((pointer-token-null? account) "")
          (else 
           (let ((parent-name
                  (gnc:account-get-full-name 
                   (gnc:group-get-parent
                    (gnc:account-get-parent account)))))	   
             (if (string=? parent-name "")
                 (gnc:account-get-name account)
                 (string-append
                  parent-name
                  separator
                  (gnc:account-get-name account))))))))

(define (gnc:filter-list the-list predicate)
  (cond ((not (list? the-list))
         (gnc:error("Attempted to filter a non-list object")))
        ((null? the-list) '())
        ((predicate (car the-list))
         (cons (car the-list)
               (gnc:filter-list (cdr the-list) predicate)))
        (else (gnc:filter-list (cdr the-list) predicate))))

;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.
(define (gnc:inorder-map the-list fn)
  (cond ((not (list? the-list))
	 (gnc:error("Attempted to map a non-list object")))
	((not (procedure? fn))
	 (gnc:error("Attempted to map a non-function object to a list")))
	((eq? the-list '()) '())
	(else (cons (fn (car the-list))
		    (gnc:inorder-map (cdr the-list) fn)))))

(define (gnc:for-loop thunk first last step)
  (cond ((< first last) (thunk first) 
	 (gnc:for-loop thunk (+ first step) last step))
	(else #f)))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

(define (gnc:group-map-accounts thunk group)
  (let loop 
      ((num-accounts (gnc:group-get-num-accounts group))
       (i 0))
    (if (= i num-accounts)
        '()
        (cons (thunk (gnc:group-get-account group i))
              (loop num-accounts (+ i 1))))))

; (define (gnc:account-transactions-for-each thunk account)
;   ;; You must call gnc:group-reset-write-flags on the account group
;   ;; before using this...

;   (let loop ((num-splits (gnc:account-get-split-count account))
;              (i 0))
;     (if (< i num-splits)
;         (let* ((split (gnc:account-get-split account i))
;                (transaction (gnc:split-get-parent split)))
;           ;; We don't use the flags just like FileIO does (only 1 pass here).
;           (if (= (gnc:transaction-get-write-flag transaction) 0)
;               (begin
;                 (thunk transaction)
;                 (gnc:transaction-set-write-flag transaction 2)))
;           (loop num-splits (+ i 1))))))

(define (gnc:transaction-map-splits thunk transaction)
  (let loop ((num-splits (gnc:transaction-get-split-count transaction))
             (i 0))
    (if (< i num-splits)
        (cons
         (thunk (gnc:transaction-get-split transaction i))
         (loop num-splits (+ i 1)))
        '())))

(define (makedrcr-collector)
  (let
      ((debits 0)
       (credits 0)
       (totalitems 0))
    (let
	((adder (lambda (amount)
		 (if (> 0 amount)
		     (set! credits (- credits amount))
		     (set! debits (+ debits amount)))
		 (set! totalitems (+ 1 totalitems))))
	 (getdebits (lambda () debits))
	 (getcredits (lambda () credits))
	 (setdebits (lambda (amount)
		      (set! debits amount)))
	 (getitems (lambda () totalitems))
	 (reset-all (lambda ()
		    (set! credits 0)
		    (set! debits 0)
		    (set! totalitems 0))))
      (lambda (action value)
	(case action
	  ('add (adder value))
	  ('debits (getdebits))
	  ('credits (getcredits))
	  ('items (getitems))
	  ('reset (reset-all)))))))

;; Add x to list lst if it is not already in there
(define (addunique lst x)
  (if (null? lst)  
      (list x)		; all checked add it
      (if (equal? x (car lst))
	  lst	; found, quit search and don't add again
	  (cons (car lst) (addunique (cdr lst) x))))) ; keep searching


;; find's biggest number in recursive set of vectors
(define (find-largest-in-vector input)
  (let loop ((i 0)
	     (max 0))  ; fixme: should be most negative number
    (if (= i (vector-length input)) max
	(let subloop ((x (vector-ref input i)))
	  (cond ((vector? x) (subloop (find-largest-in-vector x)))
		((number? x) (if (> x max) (loop (+ i 1) x) (loop (+ i 1) max)))
		(else (loop (+ i 1) max)))))))
		

;; takes in a vector consisting of integers, #f's and vectors (which
;; take integers, #f's and vectors ...)
;; the output vector will contain references to integer N in position N.
;;
;; example:
;;  #(1 #(0 #f 2) 3) ->  #( (1 0) (0) (1 2) (2) )

(define (find-vector-mappings input)
  (let ((outvec (make-vector (+ 1 (find-largest-in-vector input)) #f)))
    (let loop ((i 0)
	       (refs '())
	       (vec input))
      (cond ((= i (vector-length vec)) outvec)
	    (else
	     (let ((item (vector-ref vec i)))
	       (if (vector? item) (loop 0 (cons i refs) item))
	       (if (integer? item)
		   (if (>= item 0)
		       (vector-set! outvec item (reverse (cons i refs)))))
	       (loop (+ i 1) refs vec)))))
    outvec))

;; recursively apply vector-ref
(define (vector-N-ref vector ref-list)
  (cond ((eqv? ref-list '()) vector)
	(else (vector-N-ref (vector-ref vector (car ref-list)) (cdr ref-list)))))

;; map's a recursive vector in a given order (returning a list).  the
;; order is as generated by find-vector-mappings.  
(define (vector-map-in-specified-order proc vector order)
  (let loop ((i 0))
    (cond ((= i (vector-length order)) '())
	  (else
	   (let ((ref-list (vector-ref order i)))
	     (cond ((not ref-list) (loop (+ 1 i)))
		   (else 
		    (cons (proc (vector-N-ref vector ref-list))
			  (loop (+ 1 i))))))))))

;; map's a recursive vector in a given order (returning a list).  the
;; order is as generated by find-vector-mappings.  the procedure is a
;; vector itself, with the same structure as the input vector.
(define (vector-map-in-specified-order-uniquely procvec vector order)
  (let loop ((i 0))
    (cond ((= i (vector-length order)) '())
	  (else
	   (let ((ref-list (vector-ref order i)))
	     (cond ((not ref-list) (loop (+ 1 i)))
		   (else 
		    (cons ((vector-N-ref procvec ref-list)
			   (vector-N-ref vector ref-list))
			  (loop (+ 1 i))))))))))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

;; register a configuration option for the transaction report
(define (trep-options-generator)
  (define gnc:*transaction-report-options* (gnc:new-options))
  (define (gnc:register-trep-option new-option)
    (gnc:register-option gnc:*transaction-report-options* new-option))
  ;; from date
  ;; hack alert - could somebody set this to an appropriate date?
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "From"
    "a" "Report Items from this date" 
    (lambda ()
      (let ((bdtime (localtime (current-time))))
        (set-tm:sec bdtime 0)
        (set-tm:min bdtime 0)
        (set-tm:hour bdtime 0)
        (set-tm:mday bdtime 1)
        (set-tm:mon bdtime 0)
        (let ((time (car (mktime bdtime))))
          (cons time 0))))
    #f))

  ;; to-date
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report items up to and including this date"
    (lambda () (cons (current-time) 0))
    #f))

  ;; account to do report on
  (gnc:register-trep-option
   (gnc:make-account-list-option
    "Report Options" "Account"
    "c" "Do transaction report on this account"
    (lambda ()
      (let ((current-accounts (gnc:get-current-accounts))
            (num-accounts (gnc:group-get-num-accounts (gnc:get-current-group)))
            (first-account (gnc:group-get-account (gnc:get-current-group) 0)))
        (cond ((not (null? current-accounts)) (list (car current-accounts)))
              ((> num-accounts 0) (list first-account))
              (else ()))))
    #f #f))

  ;; primary sorting criterion
  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Primary Key"
     "a" "Sort by this criterion first"
     'date
     (list #(date
	     "Date"
	     "Sort by date")
	   #(time
	     "Time"
	     "Sort by EXACT entry time")
	   #(corresponding-acc
	     "Transfer from/to"
	     "Sort by account transferred from/to's name")
	   #(amount
	     "Amount"
	     "Sort by amount")
	   #(description
	     "Description"
	     "Sort by description")
	   #(number
	     "Number"
	     "Sort by check/transaction number")
	   #(memo
	     "Memo"
	     "Sort by memo"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Primary Sort Order"
    "b" "Order of primary sorting"
    'ascend
    (list #(ascend "Ascending" "smallest to largest, earliest to latest")
	  #(descend "Descending" "largest to smallest, latest to earliest"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Secondary Key"
     "c"
     "Sort by this criterion second"
     'corresponding-acc
     (list #(date
	     "Date"
	     "Sort by date")
	   #(time
	     "Time"
	     "Sort by EXACT entry time")
	   #(corresponding-acc
	     "Transfer from/to"
	     "Sort by account transferred from/to's name")
	   #(amount
	     "Amount"
	     "Sort by amount")
	   #(description
	     "Description"
	     "Sort by description")
	   #(number
	     "Number"
	     "Sort by check/transaction number")
	   #(memo
	     "Memo"
	     "Sort by memo"))))

  (gnc:register-trep-option
   (gnc:make-multichoice-option
    "Sorting" "Secondary Sort Order"
    "d" "Order of Secondary sorting"
    'ascend
    (list #(ascend "Ascending" "smallest to largest, earliest to latest")
	  #(descend "Descending" "largest to smallest, latest to earliest"))))

  gnc:*transaction-report-options*)

