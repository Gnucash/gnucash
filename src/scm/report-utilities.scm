;;; $Id$
;;; Reporting utilities
(gnc:support "report-utilities.scm")

(define (gnc:amount->string amount print_currency_symbol?
                            print_separators? shares_value?)
  (gnc:amount->string-helper (exact->inexact amount)
                             print_currency_symbol?
                             print_separators?
                             shares_value?))

(define (gnc:amount->formatted-string amount shares_value?)
  (gnc:amount->string amount #t #t shares_value?))

(define (gnc:account-has-shares? account)
  (let ((type (gnc:account-type->symbol (gnc:account-get-type account))))
    (member type '(STOCK MUTUAL CURRENCY))))

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
    (if (pointer-token-null? account) 
	""
	(let ((parent-name
	       (gnc:account-get-full-name 
		(gnc:group-get-parent
		 (gnc:account-get-parent account)))))
	  (if (string=? parent-name "")
	      (gnc:account-get-name account)
	      (string-append
	       parent-name
	       separator
	       (gnc:account-get-name account)))))))

(define (gnc:filter-list the-list predicate)
  (cond 
   ((not (list? the-list))
    (gnc:error("Attempted to filter a non-list object")))
   ((null? the-list) 
    '())
   ((predicate (car the-list))
    (cons (car the-list)
	  (gnc:filter-list (cdr the-list) predicate)))
   (else (gnc:filter-list (cdr the-list) predicate))))

;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.
(define (gnc:inorder-map the-list fn)
  (cond 
   ((not (list? the-list))
    (gnc:error("Attempted to map a non-list object")))
   ((not (procedure? fn))
    (gnc:error("Attempted to map a non-function object to a list")))
   ((eq? the-list '()) '())
   (else (cons (fn (car the-list))
	       (gnc:inorder-map (cdr the-list) fn)))))

(define (gnc:for-loop thunk first last step)
  (if (< first last) 
      (begin
	(thunk first)
	(gnc:for-loop thunk (+ first step) last step))
      #f))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
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
;
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

;;; Here's a statistics collector...  Collects max, min, total, and makes
;;; it easy to get at the mean.

;;; It would be a logical extension to throw in a "slot" for x^2 so
;;; that you could also extract the variance and standard deviation

(define (make-stats-collector)
  (let ;;; values
      ((value 0)
       (totalitems 0)
       (max -10E9)
       (min 10E9))
    (let ;;; Functions to manipulate values
	((adder (lambda (amount)
		  (if (number? amount) 
		      (begin
			(set! value (+ amount value))
			(if (> amount max)
			    (set! max amount))
			(if (< amount min)
			    (set! min amount))
			(set! totalitems (+ 1 totalitems))))))
	 (gettotal (lambda () value))
	 (getaverage (lambda () (/ value totalitems)))
	 (getmax (lambda () max))
	 (getmin (lambda () min))
	 (reset-all (lambda ()
		    (set! value 0)
		    (set! max -10E9)
		    (set! min 10E9)
		    (set! totalitems 0))))
      (lambda (action value)  ;;; Dispatch function
	(case action
	  ('add (adder value))
	  ('total (gettotal))
	  ('average (getaverage))
	  ('getmax (getmax))
	  ('getmin (getmin))
	  ('reset (reset-all))
          (else (gnc:warn "bad stats-collector action: " action)))))))

(define (makedrcr-collector)
  (let ;;; values
      ((debits 0)
       (credits 0)
       (totalitems 0))
    (let ;;; Functions to manipulate values
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
      (lambda (action value)  ;;; Dispatch function
	(case action
	  ('add (adder value))
	  ('debits (getdebits))
	  ('credits (getcredits))
	  ('items (getitems))
	  ('reset (reset-all))
          (else (gnc:warn "bad dr-cr-collector action: " action)))))))

;; Add x to list lst if it is not already in there
(define (addunique lst x)
  (if (null? lst)  
      (list x)		; all checked add it
      (if (equal? x (car lst))
	  lst	; found, quit search and don't add again
	  (cons (car lst) (addunique (cdr lst) x))))) ; keep searching

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

(define (gnc:split-list-total splits)
  (let ((num-splits (gnc:count-splits splits)))
    (let loop
        ((total 0)
         (index 0))
      (if (>= index num-splits)
          total
          (loop (+ total (gnc:split-get-value (gnc:ith-split splits index)))
                (+ index 1))))))

(define (gnc:split-list-balance splits)
  (if (= (gnc:count-splits splits) 0)
      0
      (+ (gnc:split-list-total splits)
         (gnc:split-get-balance (gnc:ith-split splits 0)))))
