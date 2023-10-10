;; this file is meant to be run via the gnucash-cli interface: --script simple-book-add-txn.scm
;;
;; gnucash-cli book.gnucash --script simple-book-add-txn.scm
;;
;; the book will be considered "valid" if it has a basic hierarchy such as the following
;; Assets
;; |-Current
;; | |- Bank
;; Expenses
;; |-Govt
;; | |- Taxes
;; |-Personal
;; |-Medical

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (ice-9 rdelim))
(use-modules (ice-9 match))

(define (get-line prompt)
  (format #t "\x1b[1;33m~a:\x1b[m " prompt)
  (let ((rv (read-line)))
    (if (eof-object? rv) "" rv)))

(define (get-amount prompt)
  (let ((amount (gnc-numeric-from-string (get-line prompt))))
    (if (number? amount)
        amount
        (get-amount prompt))))

(define (get-item-from-list lst elt->string prompt)
  (define (get-amount-line) (get-amount prompt))
  (let lp ((idx 1) (lst lst))
    (unless (null? lst)
      (format #t "~a. ~a\n" idx (elt->string (car lst)))
      (lp (1+ idx) (cdr lst))))
  (let lp ((idx (get-amount-line)))
    (cond
     ((and (integer? idx) (positive? idx))
      (let lp1 ((idx (1- idx)) (lst lst))
        (cond
         ((null? lst) (lp (get-amount-line)))
         ((zero? idx) (car lst))
         (else (lp1 (1- idx) (cdr lst))))))
     (else (lp (get-amount-line))))))

(define (get-account prompt parent)
  (define descendants (gnc-account-get-descendants-sorted parent))
  (get-item-from-list descendants gnc-account-get-full-name "Select account by index"))

(define (get-binary-response prompt)
  (match (get-line prompt)
    ((or "Y" "y") #t)
    ((or "N" "n") #f)
    (else (get-binary-response prompt))))

(define (add-to-transaction book txn account amount memo)
  (let ((split (xaccMallocSplit book)))
    (xaccSplitSetAccount split account)
    (xaccSplitSetAmount split amount)
    (xaccSplitSetValue split amount)
    (xaccSplitSetMemo split memo)
    (xaccSplitSetParent split txn)))

(define (quit-program exitlevel)
  (gnc-clear-current-session)
  (exit exitlevel))

(define (get-new-uri session)
  (define filepath (get-line "please input correct path, or leave blank to abort"))
  (gnc-clear-current-session)
  (cond
   ((string-null? filepath) (quit-program 1))
   ((qof-session-load-quiet filepath SESSION-NORMAL-OPEN) #f) ;success
   (else (get-new-uri session))))

(define session (gnc-get-current-session))
(define root (gnc-get-current-root-account))

(let check-book-loop ()
  (cond
   ((or (null? (gnc-account-lookup-by-full-name root "Assets:Current:Bank"))
        (null? (gnc-account-lookup-by-full-name root "Expenses"))
        (null? (gnc-account-lookup-by-full-name root "Expenses:Govt:Taxes")))
    (display "\n\n\nWARNING: It doesn't seem the correct book is loaded.\n")
    (get-new-uri session)
    (check-book-loop))))

(define book (gnc-get-current-book))
(define acc-BANK (gnc-account-lookup-by-full-name root "Assets:Current:Bank"))
(define acc-EXP (gnc-account-lookup-by-full-name root "Expenses"))
(define acc-EXP-TAX (gnc-account-lookup-by-full-name root "Expenses:Govt:Taxes"))
(define acc-EXP-LEAF (get-account "Expense leaf account" acc-EXP))

(define (accounts-action action-fn)
  (action-fn acc-BANK)
  (action-fn acc-EXP-LEAF)
  (action-fn acc-EXP-TAX))

(define description (get-line "Description"))

(let lp ()
  (define txn (xaccMallocTransaction book))
  (define net-amount (get-amount "Amount, without tax"))
  (define tax-amount (* net-amount 1/10))
  (define total-amount (+ tax-amount net-amount))

  (xaccTransBeginEdit txn)
  (xaccTransSetCurrency txn (xaccAccountGetCommodity acc-BANK))
  (xaccTransSetDatePostedSecsNormalized txn (current-time))
  (xaccTransSetDescription txn description)
  (add-to-transaction book txn acc-BANK (- total-amount) "from bank")
  (add-to-transaction book txn acc-EXP-LEAF net-amount "expense net")
  (add-to-transaction book txn acc-EXP-TAX tax-amount "tax paid")
  (newline)
  (gnc:dump-transaction txn)

  (cond
   ((not (xaccTransIsBalanced txn))
    (display "WARNING: transaction is not balanced. Try again.\n")
    (xaccTransRollbackEdit txn)
    (xaccTransDestroy txn)
    (lp))
   ((get-binary-response "Please confirm transaction [YN]")
    (accounts-action xaccAccountBeginEdit)
    (xaccTransCommitEdit txn)
    (accounts-action xaccAccountCommitEdit))
   (else
    (xaccTransRollbackEdit txn)
    (xaccTransDestroy txn))))

;; (gnc:dump-book)
(when (qof-book-session-not-saved book)
  (display "Saving book...\n")
  (qof-session-save-quiet))

(quit-program 0)
