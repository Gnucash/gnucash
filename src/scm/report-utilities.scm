;; report-utilities.scm -- Reporting utilities
;;
;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

(gnc:support "report-utilities.scm")
(gnc:depend "engine-utilities.scm")

(define (gnc:amount->string amount info)
  (gnc:amount->string-helper (exact->inexact amount) info))

(define (gnc:account-has-shares? account)
  (let ((type (gw:enum-GNCAccountType-val->sym
               (gnc:account-get-type account)
               #f)))
    (member type '(stock mutual-fund currency))))

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
    (if (not account)
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
;; note: map-in-order is in a SRFI.
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

(define (gnc:map-for thunk first last step)
  (if (< first last)
      (cons
	(thunk first)
	(gnc:map-for thunk (+ first step) last step))
      '()))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) 
		  (thunk (gnc:account-get-split account x)))
                0 (gnc:account-get-split-count account) 1))

;;; applies thunk to each split in account account
(define (gnc:map-splits-in-account thunk account)
  (gnc:map-for (lambda (x) 
		 (thunk (gnc:account-get-split account x)))
	       0 (gnc:account-get-split-count account) 1))

;; Pull a scheme list of splits from a C array
(define (gnc:convert-split-list split-array)
  (let loop ((index 0)
             (split (gnc:ith-split split-array 0))
             (slist '()))
    (if (not split)
        (reverse slist)
        (loop (+ index 1)
              (gnc:ith-split split-array (+ index 1))
              (cons split slist)))))

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

;; This is a collector of values -- works similar to the stats-collector but
;; has much less overhead. It is used by the currency-collector (see below).
(define (make-value-collector)
  (let ;;; values
      ((value 0))
    (lambda (action amount)  ;;; Dispatch function
      (case action
	('add (if (number? amount) 
		  (set! value (+ amount value))))
	('total value)
	(else (gnc:warn "bad value-collector action: " action))))))

;; A currency collector. This is intended to handle multiple currencies' 
;; amounts. The amounts are accumulated via 'add, the result can be 
;; fetched via 'format.
;; Example: (define a (make-currency-collector)) ... (a 'add 'USD 12) ...
;; (a 'format (lambda(x y)(list x y)) #f) 
;; gives you something like ((USD 123.4) (DEM 12.21) (FRF -23.32))
;;
;; The functions:
;;   'add <currency> <amount>: Add the given amount to the 
;;       appropriate currencies' total amount.
;;   'format <fn> #f: Call the function <fn> (where fn takes two arguments) for 
;;       each currency with the arguments <currency> and the corresponding 
;;       total <amount>. The results is a list of each call's result.
;;   'merge <currency-collector> #f: Merge the given other currency-collector into
;;       this one, adding all currencies' amounts, respectively.
;;   'minusmerge <currency-collector> #f: Merge the given other 
;;       currency-collector into this one (like above) but subtract the other's
;;       currencies' amounts from this one's amounts, respectively.
;;   'reset #f #f: Delete everything that has been accumulated 
;;       (even the fact that any currency showed up at all).
;;   (internal) 'list #f #f: get the association list of currency->value-collector

(define (make-currency-collector)
  (let 
      ;; the association list of (currency -> value-collector) pairs.
      ((currencylist '()))
    
    ;; helper function to add a currency->value pair to our list. 
    ;; If no pair with this currency exists, we will create one.
    (define (add-currency-value currency value)
      ;; lookup the corresponding pair
      (let ((pair (assoc currency currencylist)))
	(if (not pair)
	    (begin
	      ;; create a new pair, using the value-collector
	      (set! pair (list currency (make-value-collector)))
	      ;; and add it to the alist
	      (set! currencylist (cons pair currencylist))))
	;; add the value
	((cadr pair) 'add value)))
    
    ;; helper function to walk an association list, adding each
    ;; (currency -> collector) pair to our list
    (define (add-currency-clist clist)
      (cond ((null? clist) '())
	    (else (add-currency-value (caar clist) 
				      ((cadar clist) 'total #f))
		  (add-currency-clist (cdr clist)))))

    (define (minus-currency-clist clist)
      (cond ((null? clist) '())
	    (else (add-currency-value (caar clist) 
				      (* -1 
					 ((cadar clist) 'total #f)))
		  (minus-currency-clist (cdr clist)))))

    ;; helper function walk the association list doing a callback on
    ;; each key-value pair.
    (define (process-currency-list fn clist)
      (cond ((null? clist) '())
	    (else (cons (fn (caar clist) ((cadar clist) 'total #f))
			(process-currency-list fn (cdr clist))))))

    ;; Dispatch function
    (lambda (action currency amount)
      (case action
	('add (add-currency-value currency amount))
	('merge (add-currency-clist (currency 'list #f #f)))
	('minusmerge (minus-currency-clist (currency 'list #f #f)))
	('format (process-currency-list currency currencylist))
	('reset (set! currencylist '()))
	('list currencylist) ; this one is only for internal use
	(else (gnc:warn "bad currency-collector action: " action))))))

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

;; get transaction date from split - needs to be done indirectly
;; as it's stored in the parent transaction
(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

;; get the account balance at the specified date. if include-children?
;; is true, the balances of all children (not just direct children)
;; are included in the calculation.
(define (d-gnc:account-get-balance-at-date account date include-children?)
  (let ((children-balance
         (if include-children?
             (d-gnc:group-get-balance-at-date
              (gnc:account-get-children account) date)
             0)))
    (let loop ((index 0)
               (balance 0)
               (split (gnc:account-get-split account 0)))
      (if (not split)
          (+ children-balance balance)
          (if (gnc:timepair-lt date (gnc:split-get-transaction-date split))
              (+ children-balance balance)
              (loop (+ index 1)
                    (d-gnc:split-get-balance split)
                    (gnc:account-get-split account (+ index 1))))))))

;; This works similar as above but returns a currency-collector, 
;; thus takes care of children accounts with different currencies.
(define (gnc:account-get-curr-balance-at-date account 
					      date include-children?)
  (let ((balance-collector
         (if include-children?
             (gnc:group-get-curr-balance-at-date
              (gnc:account-get-children account) date)
             (make-currency-collector))))
    (let loop ((index 0)
	       (balance 0)
	       (split (gnc:account-get-split account 0)))
      (if (not split)
	  (balance-collector 'add (gnc:account-get-currency account)
			     balance)
	  (if (gnc:timepair-lt date (gnc:split-get-transaction-date split))
	      (balance-collector 'add (gnc:account-get-currency account)
				 balance)
	      (loop (+ index 1)
		    (d-gnc:split-get-balance split)
		    (gnc:account-get-split account (+ index 1))))))
    balance-collector))

;; get the balance of a group of accounts at the specified date.
;; all children are included in the calculation
(define (d-gnc:group-get-balance-at-date group date)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (d-gnc:account-get-balance-at-date account date #t)) group)))

;; returns a currency-collector
(define (gnc:group-get-curr-balance-at-date group date)
  (let ((this-collector (make-currency-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-accounts
	       (lambda (account)
		 (gnc:account-get-curr-balance-at-date account date #t)) group))
      this-collector))

;; get the change in balance from the 'from' date to the 'to' date.
;; this isn't quite as efficient as it could be, but it's a whole lot
;; simpler :)
(define (d-gnc:account-get-balance-interval account from to include-children?)
  (- (d-gnc:account-get-balance-at-date account to include-children?)
     (d-gnc:account-get-balance-at-date account from include-children?)))

;; the version which returns a currency-collector
(define (gnc:account-get-curr-balance-interval 
	 account from to include-children?)
  (let ((this-collector (gnc:account-get-curr-balance-at-date 
			 account to include-children?)))
    (this-collector 'minusmerge (gnc:account-get-curr-balance-at-date 
				 account from include-children?) #f)
    this-collector))

(define (d-gnc:group-get-balance-interval group from to)
  (apply +
         (gnc:group-map-accounts
          (lambda (account)
            (d-gnc:account-get-balance-interval account from to #t)) group)))

;; the version which returns a currency-collector
(define (gnc:group-get-curr-balance-interval group from to)
  (let ((this-collector (make-currency-collector)))
    (for-each (lambda (x) (this-collector 'merge x #f))
	      (gnc:group-map-accounts
	       (lambda (account)
		 (gnc:account-get-curr-balance-interval 
		  account from to #t)) group))
    this-collector))

(define (gnc:transaction-get-splits transaction)
  (let* ((num-splits (gnc:transaction-get-split-count transaction)))
    (let loop ((index 0))
      (if (= index num-splits)
	  '()
	  (cons
	   (gnc:transaction-get-split transaction index)
	   (loop (+ index 1)))))))

;; given one split, return the other splits in a transaction
(define (gnc:split-get-other-splits split)
  (let loop ((splits 
	      (gnc:transaction-get-splits (gnc:split-get-parent split))))
    (if (null? splits) 
	'()
	(if (equal? (car splits) split)
	    (loop (cdr splits))
	    (cons (car splits) (loop (cdr splits)))))))
