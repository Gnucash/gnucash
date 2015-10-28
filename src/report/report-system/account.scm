(define-module (gnucash report report-system account))
(use-modules (gnucash gnc-module))
(use-modules (gnucash gnc-module))
(use-modules (ice-9 hash-table))

(use-modules (srfi srfi-1)
             (srfi srfi-13))

(cond-expand
  (guile-2
    (eval-when
      (compile load eval expand)
      (gnc:module-load "gnucash/engine" 0)))
  (else
    (gnc:module-load "gnucash/engine" 0)))

(export account-same?)
(export account-in-list?)
(export account-in-list-pred)
(export account-in-alist)
(export account-full-name<?)
(export account-list-predicate)
(export accounts-get-children-depth)

;; is account in list of accounts?
(define (account-same? a1 a2)
  (string=? (gncAccountGetGUID a1) (gncAccountGetGUID a2)))

(define account-in-list?
  (lambda (account accounts)
    (cond
     ((null? accounts) #f)
     ((account-same? (car accounts) account) #t)
     (else (account-in-list? account (cdr accounts))))))

;; Optimized version of accout-in-list if we know
;; the list in advance.
(define (account-in-list-pred accounts)
  (define (my-assoc str alist)
    (find (lambda (pair) (account-same? str (car pair))) alist))
  (define (my-hash acc size)
    (remainder (string-hash (gncAccountGetGUID acc)) size))
  (let* ((acc-alist (map (lambda (x) (cons x #t)) accounts))
	(hash-table (alist->hashx-table my-hash my-assoc acc-alist)))
    (lambda (account)
      (hashx-ref my-hash my-assoc hash-table account))))

(define account-in-alist
  (lambda (account alist)
    (cond
     ((null? alist) #f)
     ((account-same? (caar alist) account) (car alist))
     (else (account-in-alist account (cdr alist))))))

;; helper for sorting of account list
(define (account-full-name<? a b)
  (string<? (gnc-account-get-full-name a) (gnc-account-get-full-name b)))

;; return maximum depth over accounts and their children, if any
(define (accounts-get-children-depth accounts)
  (apply max
	 (map (lambda (acct)
		(let ((acct-depth (gnc-account-get-current-depth acct)))
		  (+ acct-depth (- (gnc-account-get-tree-depth acct) 1))))
	      accounts)))

(define (account-list-predicate accounts)
  (let ((hashset (alist->hashq-table (map (lambda (x) (cons x #t)) accounts))))
    (lambda (account)
      (hashq-ref hashset account #f))))



