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

;; returns a list contains elements of the-list for which predicate is true
(define (gnc:filter-list the-list predicate)
  (let loop ((rest the-list)
             (collected '()))
    (cond ((null? rest) (reverse collected))
          (else (loop (cdr rest)
                      (if (predicate (car rest))
                          (cons (car rest) collected)
                          collected))))))

;; like map, but restricted to one dimension, and
;; guaranteed to have inorder semantics.
(define (gnc:inorder-map the-list fn)
  (let loop ((rest the-list)
             (collected '()))
    (cond ((null? rest) (reverse collected))
          (else (loop (cdr rest)
                      (cons (fn (car rest)) collected))))))

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
  (let ((num-accounts (gnc:group-get-num-accounts group)))
    (let loop 
        ((i 0)
         (collected '()))
      (if (>= i num-accounts)
          (reverse collected)
          (loop (+ i 1)
                (cons (thunk (gnc:group-get-account group i)) collected))))))

;; Pull a scheme list of splits from a C array
(define (gnc:convert-split-list split-array)
  (let loop ((index 0)
             (split (gnc:ith-split split-array 0))
             (slist '()))
    (if (pointer-token-null? split)
        (reverse slist)
        (loop (+ index 1) 
              (gnc:ith-split split-array (+ index 1))
              (cons split slist)))))

;; Pull a scheme list of accounts (including subaccounts) from group grp
(define (gnc:group-get-account-list grp)
  (if (pointer-token-null? grp)
      '()
      (let ((account-array (gnc:get-accounts grp)))
        (let loop ((index 0)
                   (account (gnc:account-nth-account account-array 0))
                   (account-list '()))
          (if (pointer-token-null? account)
              (reverse account-list)
              (loop (+ index 1)
                    (gnc:account-nth-account account-array (+ index 1))
                    (cons account account-list)))))))

;; map over all accounts (including subaccounts) in a group
(define (gnc:group-map-all-accounts thunk group)
  (map thunk (gnc:group-get-account-list group)))

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
      (let ((first-split (gnc:ith-split splits 0)))
        (+ (gnc:split-list-total splits)
           (gnc:split-get-balance first-split)
           (- (gnc:split-get-value first-split))))))

;; get transaction date from split - needs to be done indirectly
;; as it's stored in the parent transaction
(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

;; ditto descriptions
(define (gnc:split-get-description-from-parent split)
  (gnc:transaction-get-description (gnc:split-get-parent split)))

;; get the account name of a split
(define (gnc:split-get-account-name split)  
  (gnc:account-get-name (gnc:split-get-account split)))

;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
(define (gnc:account-get-balance-at-date account date include-children?)
  (let ((children-balance
         (if include-children?
             (gnc:group-get-balance-at-date
              (gnc:account-get-children account) date)
             0)))
    (let loop ((index 0)
               (balance 0)
               (split (gnc:account-get-split account 0)))
      (if (pointer-token-null? split)
          (+ children-balance balance)
          (if (gnc:timepair-lt date (gnc:split-get-transaction-date split))
              (+ children-balance balance)
              (loop (+ index 1)
                    (gnc:split-get-balance split)
                    (gnc:account-get-split account (+ index 1))))))))

;; get the balance of a group of accounts at the specified date.
;; all children are included in the calculation
(define (gnc:group-get-balance-at-date group date)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (gnc:account-get-balance-at-date account date #t)) group)))

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (gnc:account-get-balance-interval account from to include-children?)
  (- (gnc:account-get-balance-at-date account to include-children?)
     (gnc:account-get-balance-at-date account from include-children?)))

(define (gnc:group-get-balance-interval group from to)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (gnc:account-get-balance-interval account from to #t)) group)))
