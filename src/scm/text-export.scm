;;; $Id$

(gnc:support "text-export.scm")

(require 'pretty-print)
(define (gnc:group-map-accounts thunk group)
  (let loop ((num-accounts (gnc:group-get-num-accounts group))
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
;           ;; We don't use the flags just like FileIO does (only 1 pass here)...
;           (if (= (gnc:transaction-get-write-flag transaction) 0)
;               (begin
;                 (thunk transaction)
;                 (gnc:transaction-set-write-flag transaction 2)))
;           (loop num-splits (+ i 1))))))




(define (gnc:main-win-export-data-as-text win) 
  (let ((account-group (gnc:get-current-group)))
    (if (not account-group)
        (gnc:error-dialog "No account group available for text export.")
        (gnc:account-group-export-as-text account-group))))


(define (gnc:account->output-form a)
  (list
   'account
   (gnc:account-get-id a)
   (gnc:account-get-name a)
   (gnc:account-get-flags a)
   (gnc:account-type->symbol (gnc:account-get-type a))
   (gnc:account-get-code a)
   (gnc:account-get-description a)
   (gnc:account-get-notes a)
   (gnc:account-get-currency a)
   (gnc:account-get-security a)
   (let* ((accinfo (gnc:account-get-acc-info a))
          (invacct (gnc:cast-to-inv-acct accinfo)))
     (if (not (pointer-token-null? invacct))
         (gnc:inv-acct-get-price-src invacct)
         #f))
   (list 'children
         (gnc:group-map-accounts 
          gnc:account->output-form
          (gnc:account-get-children a)))))

(define (gnc:account-group-export-as-text account-group)
  (let ((file-name (gnc:file-selection-dialog
		    "Select file for text export" "")))
    (if file-name
        (begin
          (gnc:debug "Running text exporting to (not really) " file-name)
          (pretty-print 'gnucash-data-file)
          (pretty-print '(version "1.0"))
          (display "\n\n;;; Account information\n")
          ;; Print all the accounts
          (pretty-print
           (gnc:group-map-accounts
            gnc:account->output-form
            account-group))
          (display "\n\n;;; Transactions\n\n")
          ;; Now print all the transactions
          (gnc:group-begin-staged-transaction-traversals account-group)
          (gnc:group-map-accounts gnc:account-transactions-export-as-text
				  account-group)))))

(define (gnc:account-transactions-export-as-text account)
  (gnc:account-staged-transaction-traversal
   account
   1
   (lambda (t) (pretty-print (gnc:transaction->output-form t)) #f)))

(define (gnc:transaction->output-form transaction)
  (list
   'transaction
   (gnc:transaction-get-num transaction)
   (gnc:transaction-get-date-posted transaction)
   (gnc:transaction-get-date-entered transaction)
   (gnc:transaction-get-description transaction)
   (gnc:transaction-get-docref transaction)
   (gnc:transaction-map-splits gnc:split->output-form transaction)))

(define (gnc:transaction-map-splits thunk transaction)
  (let loop ((num-splits (gnc:transaction-get-split-count transaction))
             (i 0))
    (if (< i num-splits)
        (cons
         (thunk (gnc:transaction-get-split transaction i))
         (loop num-splits (+ i 1)))
        '())))

(define (gnc:split->output-form split)
  (list
   'split
   (gnc:split-get-memo split)
   (gnc:split-get-action split)
   (gnc:split-get-reconcile-state split)
   (gnc:split-get-reconciled-date split)
   (gnc:split-get-docref split)
   (gnc:split-get-share-amount split)
   (gnc:split-get-share-price split)
   (gnc:split-get-share-price split)
   (let ((xfer-account (gnc:split-get-account split))
         (xfer-account-id #f))
     (if (not (pointer-token-null? xfer-account))
         (set! xfer-account-id (gnc:account-get-id xfer-account)))
     xfer-account-id)))
