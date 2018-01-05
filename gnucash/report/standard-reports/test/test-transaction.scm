(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports transaction))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (sxml simple))

;; copied from transaction.scm
(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")

(define (run-test)
  (and
   (test null-test)
   (test trep-test-accountlist-generator)
   (test trep-test-split-generator)
   (test trep-test-filtering-splits)
   (test trep-test-table-generator)
   ))

(define constructor (record-constructor <report>))

(define (set-option! report section name value)
  (gnc:option-set-value (gnc:lookup-option (gnc:report-options report) section name) value))

(define structure
  (list "Root" (list (cons 'type ACCT-TYPE-ASSET))
        (list "Asset"
              (list "Bank")
              (list "Wallet"))
        (list "Income" (list (cons 'type ACCT-TYPE-INCOME)))
        (list "Expenses" (list (cons 'type ACCT-TYPE-EXPENSE)))))

(define (null-test)
  ;; The proper tests. This null-test tests for the presence of report.
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (template (gnc:find-report-template trep-uuid))
         (options (gnc:make-report-options trep-uuid))
         (report (constructor trep-uuid "bar" options #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template)))
    (let ((doc (renderer report)))
      (gnc:html-document-set-style-sheet! doc (gnc:report-stylesheet report))
      (let ((result (gnc:html-document-render doc #f)))
        (not (equal? result #f))))))

(define (trep-test-accountlist-generator)
  ;; This test will create an environment, add the account structure
  ;; specified
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (asset (cdr (assoc "Asset" account-alist)))
         (bank (cdr (assoc "Bank" account-alist)))
         (wallet (cdr (assoc "Wallet" account-alist)))
         (income (cdr (assoc "Income" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (result #f))
    (and
     ;; options tested:
     ;;  accounts/accounts (list)
     ;;  filter/account name filter (str)
     ;;  filter/account name regex? (bool)
     (let* ((options (gnc:make-report-options trep-uuid))
            (report (constructor trep-uuid "bar" options #t #t #f #f "")))
       (set-option! report "Accounts" "Accounts"
                    (list asset bank wallet income expense))
       (set-option! report "Filter" "Account Name Filter" "As.et")
       (set-option! report "Filter" "Use regular expressions for account name filter" #t)
       (set! result (renderer:options->accountlist options))
       (equal? result (list asset bank wallet)))
     (let* ((options (gnc:make-report-options trep-uuid))
            (report (constructor trep-uuid "bar" options #t #t #f #f "")))
       (set-option! report "Accounts" "Accounts"
                    (list asset bank wallet income expense))
       (set-option! report "Filter" "Account Name Filter" "As.et")
       (set-option! report "Filter" "Use regular expressions for account name filter" #f)
       (set! result (renderer:options->accountlist options))
       (equal? result '())))
    ))

(define (trep-test-split-generator)
  ;; This test will create an environment, add the account structure,
  ;; add daily transactions for this month, and create a split list
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (account-list (list bank))
         (begin-month (gnc:get-start-this-month))
         (end-month (gnc:get-end-this-month))
         (template (gnc:find-report-template trep-uuid))
         (options (gnc:make-report-options trep-uuid))
         (report (constructor trep-uuid "bar" options #t #t #f #f ""))
         (result #f))
    (env-create-daily-transactions env begin-month end-month bank expense)
    (let ((txn (env-create-transaction env begin-month expense bank 50)))
      (xaccTransVoid txn "test-voiding"))
    ;; this test environment creates daily txns
    ;; as well as 1 voided txn dated begin-month
    (set-option! report "Accounts" "Accounts" account-list)
    (set-option! report "General" "Start Date" (cons 'relative 'start-this-month))
    (set-option! report "General" "End Date" (cons 'relative 'end-this-month))
    (and
     (begin
       (set-option! report "Sorting" "Primary Key" 'account-name)
       (set-option! report "Sorting" "Primary Subtotal" #t)
       (set-option! report "Sorting" "Secondary Key" 'date)
       (set-option! report "Sorting" "Secondary Subtotal for Date Key" 'none)
       (and
        (begin
          (set-option! report "Sorting" "Secondary Sort Order" 'descend)
          (set-option! report "Filter" "Void Transactions" 'non-void-only)
          (set! result (renderer:accountlist->splits options account-list))
          (and
           (not (= (length result) 1))
           (sorted? (map (lambda (s) (xaccTransGetDate (xaccSplitGetParent s)))
                         result) >)))
        (begin
          (set-option! report "Sorting" "Secondary Sort Order" 'ascend)
          (set-option! report "Filter" "Void Transactions" 'non-void-only)
          (set! result (renderer:accountlist->splits options account-list))
          (and
           (sorted? (map (lambda (s) (xaccTransGetDate (xaccSplitGetParent s)))
                         result) <)))
        (begin
          (set-option! report "Filter" "Void Transactions" 'void-only)
          (set! result (renderer:accountlist->splits options account-list))
          (and (= (length result) 1)
               (string=? (xaccTransGetVoidReason (xaccSplitGetParent (car result))) "test-voiding")
               )))))))

(define (trep-test-filtering-splits)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (account-list (list bank))
         (begin-month (gnc:get-start-this-month))
         (end-month (gnc:get-end-this-month))
         (template (gnc:find-report-template trep-uuid))
         (options (gnc:make-report-options trep-uuid))
         (report (constructor trep-uuid "bar" options #t #t #f #f ""))
         (result #f))
    (env-create-daily-transactions env begin-month end-month bank expense)
    (set-option! report "Accounts" "Accounts" account-list)
    (set-option! report "General" "Start Date" (cons 'relative 'start-this-month))
    (set-option! report "General" "End Date" (cons 'relative 'end-this-month))
    (set-option! report "Filter" "Transaction Filter" "pon.es")
    (and
     (begin
       ;; this tests pon.es regex filter enabled
       (set-option! report "Filter" "Use regular expressions for transaction filter" #t)
       (set! result (renderer:accountlist->splits options account-list))
       (set! result (renderer:splits->filteredsplits options result #f))
       (not (equal? '() result)))
     (begin
       ;; this tests pon.es regex filter disabled, should return empty list
       (set-option! report "Filter" "Use regular expressions for transaction filter" #f)
       (set! result (renderer:accountlist->splits options account-list))
       (set! result (renderer:splits->filteredsplits options result #f))
       (equal? '() result)))))

(define (trep-test-table-generator)
  (let* ((env (create-test-env))
         (account-alist (env-create-account-structure-alist env structure))
         (bank (cdr (assoc "Bank" account-alist)))
         (expense (cdr (assoc "Expenses" account-alist)))
         (account-list (list bank))
         (begin-month (gnc:get-start-this-month))
         (end-month (gnc:get-end-this-month))
         (template (gnc:find-report-template trep-uuid))
         (options (gnc:make-report-options trep-uuid))
         (report (constructor trep-uuid "bar" options #t #t #f #f ""))
         (splits #f)
         (table #f)
         (render #f))
    (env-create-daily-transactions env begin-month end-month bank expense)
    (set-option! report "Accounts" "Accounts" account-list)
    (set-option! report "General" "Start Date" (cons 'relative 'start-this-month))
    (set-option! report "General" "End Date" (cons 'relative 'end-this-month))
    (set! splits (renderer:accountlist->splits options account-list))
    (set! splits (renderer:splits->filteredsplits options splits #f))
    (and
     (let ((document (gnc:make-html-document)))
       ;; switch off all display options
       (set-option! report "Display" "Date" #f)
       (set-option! report "Display" "Reconciled Date" #f)
       (set-option! report "Display" "Num" #f)
       (set-option! report "Display" "Description" #f)
       (set-option! report "Display" "Memo" #f)
       (set-option! report "Display" "Notes" #f)
       (set-option! report "Display" "Account Name" #f)
       (set-option! report "Display" "Other Account Name" #f)
       (set-option! report "Display" "Shares" #f)
       (set-option! report "Display" "Price" #f)
       (set-option! report "Display" "Running Balance" #f)
       (set-option! report "Display" "Totals" #f)
       (set-option! report "Display" "Amount" 'none)
       (set! table (make-split-table splits options #f))
       (gnc:html-document-add-object! document table)
       (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
       (set! render (gnc:html-document-render document))
       (and
        (and-map
         (lambda (str)
           (not (string-contains render str)))
         (list "Date" "Reconciled Date" "Num" "Description" "Memo"
               "Notes" "Grand Total" "Account" "Transfer from/to"
               "Shares" "Price" "Running Balance" "Amount")))
       #t)
     (let ((document (gnc:make-html-document)))
       ;; switch on all display options, and test the headings do appear in render
       (set-option! report "Display" "Date" #t)
       (set-option! report "Display" "Reconciled Date" #t)
       (set-option! report "Display" "Num" #t)
       (set-option! report "Display" "Description" #t)
       (set-option! report "Display" "Memo" #t)
       (set-option! report "Display" "Notes" #t)
       (set-option! report "Display" "Account Name" #t)
       (set-option! report "Display" "Other Account Name" #t)
       (set-option! report "Display" "Shares" #t)
       (set-option! report "Display" "Price" #t)
       (set-option! report "Display" "Running Balance" #t)
       (set-option! report "Display" "Totals" #t)
       (set-option! report "Display" "Amount" 'single)
       (set! table (make-split-table splits options #f))
       (gnc:html-document-add-object! document table)
       (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
       (set! render (gnc:html-document-render document))
       (and
        (and-map
         (lambda (str)
           (string-contains render str))
         (list "Date" "Reconciled Date" "Num" "Description" "Memo"
               "Notes" "Grand Total" "Account" "Transfer from/to"
               "Shares" "Price" "Running Balance" "Amount"))
        #t))
     (let ((document (gnc:make-html-document)))
       ;; test dual-columns debit/credit headings appear
       (set-option! report "Display" "Amount" 'double)
       (set! table (make-split-table splits options #f))
       (gnc:html-document-add-object! document table)
       (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
       (set! render (gnc:html-document-render document))
       (and
        (string-contains render "Debit")
        (string-contains render "Credit")
        #t))
     )))
