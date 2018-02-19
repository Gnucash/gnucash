(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report standard-reports transaction))
(use-modules (gnucash report stylesheets))
(use-modules (gnucash report report-system))
(use-modules (gnucash report report-system test test-extras))
(use-modules (srfi srfi-64))
(use-modules (sxml simple))
(use-modules (sxml xpath))

;; copied from transaction.scm
(define trep-uuid "2fe3b9833af044abb929a88d5a59620f")

(define num-passed 0)
(define num-failed 0)
(define (my-runner)
  (let ((runner (test-runner-null)))
    (test-runner-on-test-end! runner
      (lambda (runner)
        (format #t "[~a] line:~a, test: ~a\n"
                (test-result-ref runner 'result-kind)
                (test-result-ref runner 'source-line)
                (test-runner-test-name runner))
        (case (test-result-kind runner)
          ((pass xpass) (set! num-passed (+ num-passed 1)))
          ((fail xfail)
           (if (test-result-ref runner 'expected-value)
               (format #t " -> expected: ~s\n -> obtained: ~s\n"
                       (test-result-ref runner 'expected-value)
                       (test-result-ref runner 'actual-value)))
           (set! num-failed (+ num-failed 1)))
          (else #t))))
    (test-runner-on-final! runner
      (lambda (runner)
        (format #t "Source:~a\npass = ~a, fail = ~a\n"
                (test-result-ref runner 'source-file) num-passed num-failed)))
    runner))

(define (run-test)  
  (test-runner-factory my-runner)
  (test-begin "test-transaction")
  (null-test)
  (trep-test-accountlist-generator)
  (trep-test-split-generator)
  (trep-test-filtering-splits)
  (trep-test-table-generator)
  (test-end)
  (zero? num-failed))

;;
;; CANDIDATES FOR INCLUSION IN TEST-EXTRAS.SCM
;;
(define (str->sxml str elem)
  (let ((start (string-contains str (format #f "<~a" elem))))
    (and start
         (xml->sxml (substring str start)))))

(define (get-row-col sxml row col)
  ;; sxml (SXML tree), row & col (numbers or #f)
  ;; -> list-of-str
  ;;
  ;; from an SXML table tree with tr/th/td elements, retrieve row/col
  ;; if col=#f retrieve whole <tr> row specified
  ;; if row=#f retrieve whole <td> col specified (excludes <th>)
  ;; if both #f retrieve all text elements (useless!)
  (let ((xpath (cond
                ((not (or row col)) '(// *text*))
                ((and (eqv? row 1)
                      (not col))
                 '(// (tr 1) // th // *text*))
                ((not col)  `(// (tr ,row) // *text*))
                ((not row)  `(// (td ,col) // *text*))
                ((= row 1)  `(// (tr ,row) // (th ,col) // *text*))
                (else       `(// (tr ,row) // (td ,col) // *text*)))))
    ((sxpath xpath) sxml)))
;;
;; END CANDIDATES
;;

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
        (test-assert "null-test" result)))))

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
       (test-equal "account name regex filter on" result (list asset bank wallet)))
     (let* ((options (gnc:make-report-options trep-uuid))
            (report (constructor trep-uuid "bar" options #t #t #f #f "")))
       (set-option! report "Accounts" "Accounts"
                    (list asset bank wallet income expense))
       (set-option! report "Filter" "Account Name Filter" "As.et")
       (set-option! report "Filter" "Use regular expressions for account name filter" #f)
       (set! result (renderer:options->accountlist options))
       (test-equal "account name regex filter off" result '())))
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
           (test-assert "descending date sort"
             (sorted? (map (lambda (s) (xaccTransGetDate (xaccSplitGetParent s)))
                           result) >))))
        (begin
          (set-option! report "Sorting" "Secondary Sort Order" 'ascend)
          (set-option! report "Filter" "Void Transactions" 'non-void-only)
          (set! result (renderer:accountlist->splits options account-list))
          (test-assert "ascending date sort"
            (sorted? (map (lambda (s) (xaccTransGetDate (xaccSplitGetParent s)))
                          result) <)))
        (begin
          (set-option! report "Filter" "Void Transactions" 'void-only)
          (set! result (renderer:accountlist->splits options account-list))
          (test-assert "void-txn filter test"
            (and
             (= (length result) 1)
             (string=? (xaccTransGetVoidReason (xaccSplitGetParent (car result))) "test-voiding")
             ))))))))

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
       (test-assert "txn filter regex on" (not (equal? '() result))))
     (begin
       ;; this tests pon.es regex filter disabled, should return empty list
       (set-option! report "Filter" "Use regular expressions for transaction filter" #f)
       (set! result (renderer:accountlist->splits options account-list))
       (set! result (renderer:splits->filteredsplits options result #f))
       (test-equal "txn filter regex off" '() result)))))

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
         (render #f)
         )
    ;;(env-create-daily-transactions env begin-month end-month bank expense)
    (env-create-transaction env begin-month expense bank 50)
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
       (let* ((render-sxml (str->sxml render "table"))
              (sxml-headers (get-row-col render-sxml 1 #f)))
         (test-equal "Display headers disabled"
           (list " ")
           sxml-headers)))
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
       (let* ((render-sxml (str->sxml render "table"))
              (sxml-headers (get-row-col render-sxml 1 #f)))
         (test-equal "Display headers enabled"
           (list " " "Date" "Reconciled Date" "Num"
                 "Description" "Memo/Notes" "Account"
                 "Transfer from/to" "Shares" "Price"
                 "Amount" "Running Balance")
           sxml-headers
           )))
     (let ((document (gnc:make-html-document)))
       ;; test dual-columns debit/credit headings appear
       (set-option! report "Display" "Amount" 'double)
       (set! table (make-split-table splits options #f))
       (gnc:html-document-add-object! document table)
       (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
       (set! render (gnc:html-document-render document))
       (let* ((render-sxml (str->sxml render "table"))
              (sxml-headers (get-row-col render-sxml 1 #f)))
         (and
          (test-assert "dual-col Debit header" (member "Debit" sxml-headers))
          (test-assert "dual-col Credit header" (member "Credit" sxml-headers))
          (test-assert "dual-col no Amount header" (not (member "Amount" sxml-headers)))
          #t)))
     )))
