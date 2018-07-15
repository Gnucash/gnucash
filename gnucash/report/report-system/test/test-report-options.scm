(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-options") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                         ;; will create Testing/Temporary/test-report-option.log
    (test-assert "Create and Validate Option Vector" (test-check1))
    (test-assert "Create and Validate String Option" (test-check2))
    (test-assert "Create and Validate Text Option" (test-check3))
    (test-assert "Create and Validate Font Option" (test-check4))
    (test-assert "Create and Validate Currency Option" (test-check5))
    (test-assert "Create and Validate Budget Option" (test-check6))
    (test-assert "Create and Validate Commodity Option" (test-check7))
    (test-assert "Create and Validate Complex Boolean Option" (test-check8))
    (test-assert "Create and Validate Pixmap Option" (test-check9))
    (test-assert "Create and Validate Date Option" (test-check10))
    (test-assert "Create and Validate Account List Option" (test-check11))
    (test-assert "Create and Validate Selective Account List Option" (test-check12))
    (test-assert "Create and Validate Multi-Choice Option" (test-check13))
    (test-assert "Create and Validate Radio Button Option" (test-check14))
    (test-assert "Create and Validate List Option" (test-check15))
    (test-assert "Create and Validate Number Range Option" (test-check16))
    (test-assert "Create and Validate Plot Size Option" (test-check17))
    (test-assert "Create and Validate Internal Option" (test-check18))
    (test-assert "Create and Validate Query Option" (test-check19))
    (test-assert "Create and Validate Color Option" (test-check20))
    (test-assert "Create and Validate Date Format Option" (test-check21))
    (test-end "Testing/Temporary/test-report-options")
)

;; -----------------------------------------------------------------------

(define (create-branch-simple account-type account-structure)

  (define (create-account-simple account-type account-name parent-account)
    (let ((new-account (xaccMallocAccount (gnc-get-current-book))))
      (xaccAccountSetName new-account account-name)
      (xaccAccountSetType new-account account-type)
      (xaccAccountSetCommodity new-account (gnc-default-report-currency))
      (if parent-account
        (gnc-account-append-child parent-account new-account)
      )
     new-account
    )
  )

  (let* (
          (parent-name (car account-structure))
          (parent-account (create-account-simple account-type parent-name #f))
          (children (cdr account-structure))
        )
    (append
      (list (cons parent-name parent-account))
      (map
        (lambda (child-name)
          (cons child-name (create-account-simple account-type child-name parent-account))
        )
        children
      )
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check1)
  (let (
         (test-option  (gnc:make-option                       ;; Create a vector with dummy values
                          "Option Section"                    ;;0 - section
                          "Option Name"                       ;;1 - name
                          "Option Sort-Tag"                   ;;2 - sort-tag
                          "Option Type"                       ;;3 - type
                          "Option Docu-String"                ;;4 - documentation-string
                          (lambda () "Option Getter")         ;;5 - getter
                          (lambda (x) x)                      ;;6 - setter
                          (lambda () "Option Default-Getter") ;;7 - default-getter
                          "Option Generator-Restore-Form"     ;;8 - generate-restore-form
                          "Option SCM->KVP"                   ;;9 - scm->kvp
                          "Option KVP->SCM"                   ;;10 - kvp->scm
                          "Option Value-Validator"            ;;11 - value-validator
                          "Option Data"                       ;;12 - option-data
                          "Option Data Fns"                   ;;13 - option-data-fns
                                                              ;;14 - (lambda (callback) (set! changed-callback callback))
                          "Option Strings-Getter"             ;;15 - strings-getter
                          "Option Widget-Changed"             ;;16 - option-widget-changed-proc
                        )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Section")
      (string=? (gnc:option-name test-option) "Option Name")
      (string=? (gnc:option-sort-tag test-option) "Option Sort-Tag")
      (string=? (gnc:option-type test-option) "Option Type")
      (string=? (gnc:option-documentation test-option) "Option Docu-String")
      (string=? ((gnc:option-getter test-option)) "Option Getter")
      ((gnc:option-setter test-option) "Option Setter") ;; return value is #t
      (string=? ((gnc:option-default-getter test-option)) "Option Default-Getter")
      (string=? (gnc:option-generate-restore-form test-option) "Option Generator-Restore-Form")
      (string=? (gnc:option-scm->kvp test-option) "Option SCM->KVP")
      (string=? (gnc:option-kvp->scm test-option) "Option KVP->SCM")
      (string=? (gnc:option-value-validator test-option) "Option Value-Validator")
      (string=? (gnc:option-data test-option) "Option Data")
      (string=? (gnc:option-data-fns test-option) "Option Data Fns")
      (string=? (gnc:option-strings-getter test-option) "Option Strings-Getter")
      (string=? (gnc:option-widget-changed-proc test-option) "Option Widget-Changed")
      (string=? ((gnc:restore-form-generator (lambda () "foo"))) "(lambda (option) (if option ((gnc:option-setter option) foo)))")

      ;; validate modifications
      (gnc:set-option-scm->kvp test-option "Option SCM->KVP Modified")
      (string=? (gnc:option-scm->kvp test-option) "Option SCM->KVP Modified")

      (gnc:set-option-kvp->scm test-option "Option KVP->SCM Modified")
      (string=? (gnc:option-kvp->scm test-option) "Option KVP->SCM Modified")

      (gnc:option-set-changed-callback test-option (lambda () "Option Setter Modified"))
      (string=? (gnc:option-set-value test-option "Option Setter") "Option Setter Modified")
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check2)
  (let (
         (test-option (gnc:make-string-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test string option."
                        "option-default-string"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'string)
      (string=? (gnc:option-documentation test-option) "This is a test string option.")
      (string=? ((gnc:option-getter test-option)) "option-default-string")
      ((gnc:option-setter test-option) "new-string-value")
      (string=? ((gnc:option-getter test-option)) "new-string-value")
      (string=? (gnc:option-default-value test-option) "option-default-string")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-string-value\")))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test String"))
      (not (car ((gnc:option-value-validator test-option) 'test-string)))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check3)
  (let (
         (test-option (gnc:make-text-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test text option."
                        "option-default-text"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'text)
      (string=? (gnc:option-documentation test-option) "This is a test text option.")
      (string=? ((gnc:option-getter test-option)) "option-default-text")
      ((gnc:option-setter test-option) "new-text-value")
      (string=? ((gnc:option-getter test-option)) "new-text-value")
      (string=? (gnc:option-default-value test-option) "option-default-text")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-text-value\")))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Text"))
      (not (car ((gnc:option-value-validator test-option) 'test-text)))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check4)
  (let (
         (test-option (gnc:make-font-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test font option."
                        "option-default-font"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'font)
      (string=? (gnc:option-documentation test-option) "This is a test font option.")
      (string=? ((gnc:option-getter test-option)) "option-default-font")
      ((gnc:option-setter test-option) "new-font-value")
      (string=? ((gnc:option-getter test-option)) "new-font-value")
      (string=? (gnc:option-default-value test-option) "option-default-font")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-font-value\")))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Font"))
      (not (car ((gnc:option-value-validator test-option) 'test-font)))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check5)
  (let (
         (test-option (gnc:make-currency-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test currency option."
                        "option-default-currency"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'currency)
      (string=? (gnc:option-documentation test-option) "This is a test currency option.")
      (null? ((gnc:option-getter test-option)))
      ((gnc:option-setter test-option) "new-currency-value")
      (null? ((gnc:option-getter test-option)))
      (null? (gnc:option-default-value test-option))
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-currency-value\")))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Currency"))
      (car ((gnc:option-value-validator test-option) 'test-font))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check6)
  (let (
         (test-option (gnc:make-budget-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test budget option."
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'budget)
      (string=? (gnc:option-documentation test-option) "This is a test budget option.")
      (null? ((gnc:option-getter test-option)))
      ((gnc:option-setter test-option) "new-budget-value")
      (string=? ((gnc:option-getter test-option)) "new-budget-value")
      ((gnc:option-setter test-option) '())
      (not (gnc:option-default-value test-option))
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) (gnc-budget-lookup #f (gnc-get-current-book)))))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Budget"))
      (car ((gnc:option-value-validator test-option) 'test-budget))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check7)
  (let (
         (test-option (gnc:make-commodity-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test commodity option."
                        "option-default-commodity"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'commodity)
      (string=? (gnc:option-documentation test-option) "This is a test commodity option.")
      (null? ((gnc:option-getter test-option)))
      ((gnc:option-setter test-option) "new-commodity-value")
      (null? ((gnc:option-getter test-option)))
      (string=? (gnc:option-default-value test-option) "option-default-commodity") ;;BUG? should return (scm->commodity value) instead
      ;;(null? (gnc:option-default-value test-option)) ;; if (scm->commodity value) would be returned
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(commodity-scm \"CURRENCY\" \"new-commodity-value\"))))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Commodity"))
      (car ((gnc:option-value-validator test-option) 'test-commodity))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check8)
  (let (
         (test-option (gnc:make-complex-boolean-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test complex boolean option."
                        #t
                        (lambda (x) (string-append "setter-cb-" x))
                        (lambda (x) (string-append "changed-cb-" x))
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'boolean)
      (string=? (gnc:option-documentation test-option) "This is a test complex boolean option.")
      ((gnc:option-getter test-option))
      ((gnc:option-setter test-option) "new-complex-value")
      (string=? ((gnc:option-getter test-option)) "new-complex-value")
      (gnc:option-default-value test-option)
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-complex-value\")))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (not (car ((gnc:option-value-validator test-option) "Test Boolean")))
      (car ((gnc:option-value-validator test-option) #t))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (string=? ((gnc:option-widget-changed-proc test-option) "callback") "changed-cb-callback")
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check9)
  (let (
         (test-option (gnc:make-pixmap-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test pixmap option."
                        "option-default-pixmap"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'pixmap)
      (string=? (gnc:option-documentation test-option) "This is a test pixmap option.")
      (string=? ((gnc:option-getter test-option)) "option-default-pixmap")
      ((gnc:option-setter test-option) "new-pixmap-value")
      (string=? ((gnc:option-getter test-option)) "new-pixmap-value")
      (string=? (gnc:option-default-value test-option) "option-default-pixmap")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) \"new-pixmap-value\")))")
      (not (gnc:option-scm->kvp test-option))
      (not (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) "Test Pixmap"))
      (not (car ((gnc:option-value-validator test-option) 'test-pixmap)))
      (not (procedure? (gnc:option-data test-option)))
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check10)
  (let (
         (test-option (gnc:make-date-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test date option."
                        (lambda () "Option Default-Getter")
                        "Show Time"
                        "Sub Type"
                        '("Tomorrow" "Next Week" "Next Month") ;;"Relative Date List"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'date)
      (string=? (gnc:option-documentation test-option) "This is a test date option.")
      (string=? ((gnc:option-getter test-option)) "Option Default-Getter")
      ((gnc:option-setter test-option) (cons 'absolute 1))
      (and (eq? (car ((gnc:option-getter test-option))) 'absolute)
           (= (cdr ((gnc:option-getter test-option))) 1)
      )
      ((gnc:option-setter test-option) (cons 'absolute (cons 2 3)))
      (and (eq? (car ((gnc:option-getter test-option))) 'absolute)
           (= (cdr ((gnc:option-getter test-option))) 2)
      )
      ((gnc:option-setter test-option) (cons 'relative 'relval))
      (and (eq? (car ((gnc:option-getter test-option))) 'relative)
           (eq? (cdr ((gnc:option-getter test-option))) 'relval)
      )
      (string=? (gnc:option-default-value test-option) "Option Default-Getter")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(relative . relval))))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (procedure? (gnc:option-value-validator test-option))
      (and (string=? (vector-ref (gnc:option-data test-option) 0) "Sub Type")
           (string=? (vector-ref (gnc:option-data test-option) 1) "Show Time")
      )
      (and (= ((vector-ref (gnc:option-data-fns test-option) 0)) 3)
           (string=? ((vector-ref (gnc:option-data-fns test-option) 1) 0) "Tomorrow")
      )
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check11)
  (let* (
          (interest-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-INCOME
              (list
                "Interest"  ;; parent
                "Source"    ;; child
              )
            )
          )
          (bank-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-BANK
              (list
                "Bank"      ;; parent
                "Source"    ;; child
              )
            )
          )
          (test-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-ASSET
              (list
                "Asset"            ;; parent
                "Contributions"    ;; child
                "GainsAndLosses"   ;; child
              )
            )
          )
          (expense-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-EXPENSE
              (list
                "Expense"   ;; parent
                "Costs"      ;; child
              )
            )
          )
          (test-option (gnc:make-account-list-limited-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test account list option."
                        (lambda () ;; all parents
                          (list
                            (gncAccountGetGUID (assoc-ref test-account-branch-alist "Asset"))
                            (gncAccountGetGUID (assoc-ref bank-account-branch-alist "Bank"))
                            (gncAccountGetGUID (assoc-ref expense-account-branch-alist "Expense"))
                            (gncAccountGetGUID (assoc-ref interest-account-branch-alist "Interest"))
                          )
                        )
                        #f ;; value-validator
                        #t
                        '()
                       )
          )
        )
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref test-account-branch-alist "Asset"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref bank-account-branch-alist "Bank"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref expense-account-branch-alist "Expense"))
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'account-list)
      (string=? (gnc:option-documentation test-option) "This is a test account list option.")
      (and
        (= (length ((gnc:option-getter test-option))) 4) ;; all parents
        (string=? (gnc-account-get-full-name (car ((gnc:option-getter test-option)))) "Asset")
      )
      ((gnc:option-setter test-option)
        (list ;; all child-accounts
          (gncAccountGetGUID (assoc-ref bank-account-branch-alist "Source"))
          (gncAccountGetGUID (assoc-ref expense-account-branch-alist "Costs"))
          (gncAccountGetGUID (assoc-ref interest-account-branch-alist "Source"))
          (gncAccountGetGUID (assoc-ref test-account-branch-alist "Contributions"))
          (gncAccountGetGUID (assoc-ref test-account-branch-alist "GainsAndLosses"))
        )
      )
      (and
        (= (length ((gnc:option-getter test-option))) 5) ;; all child-accounts
        (string=? (gnc-account-get-full-name (car ((gnc:option-getter test-option)))) "Bank.Source")
      )
      (and
        (= (length (gnc:option-default-value test-option)) 4) ;; all parents
        (string=? (gnc-account-get-full-name (car (gnc:option-default-value test-option))) "Asset")
      )
      (string? ((gnc:option-generate-restore-form test-option)))
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) '()))
      (and (car (gnc:option-data test-option))
           (null? (cdr (gnc:option-data test-option)))
      )
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check12)
  (let* (
          (interest-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-INCOME
              (list
                "Interest"  ;; parent
                "Source"    ;; child
              )
            )
          )
          (bank-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-BANK
              (list
                "Bank"      ;; parent
                "Source"    ;; child
              )
            )
          )
          (test-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-ASSET
              (list
                "Asset"            ;; parent
                "Contributions"    ;; child
                "GainsAndLosses"   ;; child
              )
            )
          )
          (expense-account-branch-alist
            (create-branch-simple
              ACCT-TYPE-EXPENSE
              (list
                "Expense"   ;; parent
                "Costs"      ;; child
              )
            )
          )
          (test-option (gnc:make-account-sel-limited-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a selective test account list option."
                        (lambda ()
                          (gncAccountGetGUID(assoc-ref interest-account-branch-alist "Source"))
                        )
                        #f ;; value-validator
                        '()
                       )
          )
        )
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref test-account-branch-alist "Asset"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref bank-account-branch-alist "Bank"))
    (gnc-account-append-child (gnc-get-current-root-account) (assoc-ref expense-account-branch-alist "Expense"))
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'account-sel)
      (string=? (gnc:option-documentation test-option) "This is a selective test account list option.")
      (string=? (gnc-account-get-full-name ((gnc:option-getter test-option))) "Source")
      ((gnc:option-setter test-option)
          (gncAccountGetGUID (assoc-ref interest-account-branch-alist "Interest"))
      )
      (string=? (gnc-account-get-full-name ((gnc:option-getter test-option))) "") ;; BUG? why an emmpty string?
      (string=? (gnc-account-get-full-name (gnc:option-default-value test-option)) "Source")
      (string? ((gnc:option-generate-restore-form test-option)))
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) '()))
      (and (not (car (gnc:option-data test-option)))
           (null? (cdr (gnc:option-data test-option)))
      )
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check13)
  (let (
         (test-option (gnc:make-multichoice-callback-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test multichoice option."
                        "option-default-multichoice"
                        (list
                          (vector 'val1-tag "Val1 Name" "Val1 Description")
                          (vector 'val2-tag "Val2 Name" "Val2 Description")
                        )
                        (lambda (x) x)
                        (lambda (y) y)
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'multichoice)
      (string=? (gnc:option-documentation test-option) "This is a test multichoice option.")
      (string=? ((gnc:option-getter test-option)) "option-default-multichoice")
      ((gnc:option-setter test-option) 'val1-tag)
      (equal? ((gnc:option-getter test-option)) 'val1-tag)
      ((gnc:option-setter test-option) 'val2-tag)
      (equal? ((gnc:option-getter test-option)) 'val2-tag)
      ((gnc:option-setter test-option) 'val3-tag)
      (not (equal? ((gnc:option-getter test-option)) 'val3-tag))
      (string=? (gnc:option-default-value test-option) "option-default-multichoice")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) 'val2-tag)))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) 'val1-tag))
      (not (car ((gnc:option-value-validator test-option) 'test-multichoice)))
      (= (length (gnc:option-data test-option)) 2)
      (= ((vector-ref (gnc:option-data-fns test-option) 0)) 2)
      (equal? ((vector-ref (gnc:option-data-fns test-option) 1) 0) 'val1-tag)
      (string=? ((vector-ref (gnc:option-data-fns test-option) 2) 0) "Val1 Name")
      (string=? ((vector-ref (gnc:option-data-fns test-option) 3) 0) "Val1 Description")
      ((vector-ref (gnc:option-data-fns test-option) 4) 'val2-tag)
      ;;((vector-ref (gnc:option-data-fns test-option) 4) 'val3-tag) ;; BUG: this results in a crash
      (= (length ((gnc:option-strings-getter test-option))) 4) ;; ("Val1 Name" "Val1 Description" "Val2 Name" "Val2 Description")
      (string=? ((gnc:option-widget-changed-proc test-option) "Haleluja") "Haleluja")
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check14)
  (let (
         (test-option (gnc:make-radiobutton-callback-option
         ;; BUG?: Radion button option is a 1:1 copy from the multichoice option, currently nowhere in use. Needed?
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test radiobutton option."
                        "option-default-radiobutton"
                        (list
                          (vector 'val1-tag "Val1 Name" "Val1 Description")
                          (vector 'val2-tag "Val2 Name" "Val2 Description")
                        )
                        (lambda (x) x)
                        (lambda (y) y)
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'radiobutton)
      (string=? (gnc:option-documentation test-option) "This is a test radiobutton option.")
      (string=? ((gnc:option-getter test-option)) "option-default-radiobutton")
      ((gnc:option-setter test-option) 'val1-tag)
      (equal? ((gnc:option-getter test-option)) 'val1-tag)
      ((gnc:option-setter test-option) 'val2-tag)
      (equal? ((gnc:option-getter test-option)) 'val2-tag)
      ((gnc:option-setter test-option) 'val3-tag)
      (not (equal? ((gnc:option-getter test-option)) 'val3-tag))
      (string=? (gnc:option-default-value test-option) "option-default-radiobutton")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) 'val2-tag)))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) 'val1-tag))
      (not (car ((gnc:option-value-validator test-option) 'test-multichoice)))
      (= (length (gnc:option-data test-option)) 2)
      (= ((vector-ref (gnc:option-data-fns test-option) 0)) 2)
      (equal? ((vector-ref (gnc:option-data-fns test-option) 1) 0) 'val1-tag)
      (string=? ((vector-ref (gnc:option-data-fns test-option) 2) 0) "Val1 Name")
      (string=? ((vector-ref (gnc:option-data-fns test-option) 3) 0) "Val1 Description")
      ((vector-ref (gnc:option-data-fns test-option) 4) 'val2-tag)
      ;;((vector-ref (gnc:option-data-fns test-option) 4) 'val3-tag) ;; BUG: this results in a crash
      (= (length ((gnc:option-strings-getter test-option))) 4) ;; ("Val1 Name" "Val1 Description" "Val2 Name" "Val2 Description")
      (string=? ((gnc:option-widget-changed-proc test-option) "Haleluja") "Haleluja")
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check15)
  (let (
         (test-option (gnc:make-list-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test list option."
                        "option-default-list"
                        (list
                          (vector 'val1-tag "Val1 Name" "Val1 Description")
                          (vector 'val2-tag "Val2 Name" "Val2 Description")
                        )
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'list)
      (string=? (gnc:option-documentation test-option) "This is a test list option.")
      (string=? ((gnc:option-getter test-option)) "option-default-list")
      ((gnc:option-setter test-option) (list 'val1-tag 'val2-tag))
      (equal? (car ((gnc:option-getter test-option))) 'val1-tag)
      ((gnc:option-setter test-option) (list 'val2-tag 'val3-tag))
      (equal? (car ((gnc:option-getter test-option))) 'val1-tag)
      (string=? (gnc:option-default-value test-option) "option-default-list")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(val1-tag val2-tag))))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (car ((gnc:option-value-validator test-option) (list 'val1-tag)))
      (car ((gnc:option-value-validator test-option) '()))
      (not (car ((gnc:option-value-validator test-option) (list 'list-option))))
      (= (length (gnc:option-data test-option)) 2)
      (= ((vector-ref (gnc:option-data-fns test-option) 0)) 2)
      (equal? ((vector-ref (gnc:option-data-fns test-option) 1) 0) 'val1-tag)
      (string=? ((vector-ref (gnc:option-data-fns test-option) 2) 0) "Val1 Name")
      ;;(string=? ((vector-ref (gnc:option-data-fns test-option) 3) 0) "Val1 Description") ;; BUG: unbound symbol "ref"
      ((vector-ref (gnc:option-data-fns test-option) 4) 'val2-tag) ;; BUG?: not translated to list, still based on multichoice
      ;;((vector-ref (gnc:option-data-fns test-option) 4) 'val3-tag) ;; BUG: this results in a crash, if above bug is fixed
      (= (length ((gnc:option-strings-getter test-option))) 4) ;; ("Val1 Name" "Val1 Description" "Val2 Name" "Val2 Description")
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check16)
  (let (
         (test-option (gnc:make-number-range-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test number-range option."
                        "option-default-number-range"
                        0
                        1
                        1
                        0.5
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'number-range)
      (string=? (gnc:option-documentation test-option) "This is a test number-range option.")
      (string=? ((gnc:option-getter test-option)) "option-default-number-range")
      ((gnc:option-setter test-option) 3.3)
      (= ((gnc:option-getter test-option)) 3.3) ;; BUG: 3.3 is out of range, should be rejected
      (string=? (gnc:option-default-value test-option) "option-default-number-range")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) 3.3)))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (not (car ((gnc:option-value-validator test-option) 'number-range-option)))
      (not (car ((gnc:option-value-validator test-option) 1.6))) ;; BUG: 1.6 is not used, but 3.3 instead
      (= (length (gnc:option-data test-option)) 4)
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check17)
  (let (
         (test-option (gnc:make-number-plot-size-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test plot-size option."
                        "option-default-plot-size"
                        0
                        1
                        1
                        0.5
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'plot-size)
      (string=? (gnc:option-documentation test-option) "This is a test plot-size option.")
      (string=? ((gnc:option-getter test-option)) "option-default-plot-size")
      ((gnc:option-setter test-option) 3.3)
      (eq? (car ((gnc:option-getter test-option))) 'pixels) ;; BUG: 3.3 is out of range, should be rejected
      (string=? (gnc:option-default-value test-option) "option-default-plot-size")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(pixels . 3.3))))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (not (car ((gnc:option-value-validator test-option) (list 'plot-size-option))))
      (not (car ((gnc:option-value-validator test-option) (cons 'plot-size 101))))
      (car ((gnc:option-value-validator test-option) (cons 'plot-size 100)))
      (not (car ((gnc:option-value-validator test-option) (cons 'pixels 'no-number))))
      (not (car ((gnc:option-value-validator test-option) (cons 'pixels 3.3))))
      (car ((gnc:option-value-validator test-option) (cons 'pixels 1)))
      (= (length (gnc:option-data test-option)) 4)
      (not (procedure? (gnc:option-data-fns test-option)))
      (not (procedure? (gnc:option-strings-getter test-option)))
      (not (procedure? (gnc:option-widget-changed-proc test-option)))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check18)
  (let (
         (test-option (gnc:make-internal-option
                        "Option Page General"
                        "Report Name Test"
                        "option-default-internal"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "")
      (equal? (gnc:option-type test-option) 'internal)
      (not (gnc:option-documentation test-option))
      (string=? ((gnc:option-getter test-option)) "option-default-internal")
      ((gnc:option-setter test-option) 3.3)
      (eq? ((gnc:option-getter test-option)) 3.3)
      (string=? (gnc:option-default-value test-option) "option-default-internal")
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '3.3)))")
      (not (procedure? (gnc:option-scm->kvp test-option)))
      (not (procedure? (gnc:option-kvp->scm test-option)))
      (car ((gnc:option-value-validator test-option) 'any))
      (not(gnc:option-data test-option))
      (not (gnc:option-data-fns test-option))
      (not (gnc:option-strings-getter test-option))
      (not (gnc:option-widget-changed-proc test-option))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check19)
  (let (
         (test-option (gnc:make-query-option
                        "Option Page General"
                        "Report Name Test"
                        (list 66 7.7);;"option-default-query"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "")
      (equal? (gnc:option-type test-option) 'query)
      (not (gnc:option-documentation test-option))
      (= (length ((gnc:option-getter test-option))) 2)
      ((gnc:option-setter test-option) (list 4.4 5.5 6.6))
      (and (eq? (car ((gnc:option-getter test-option))) 4.4)
           (= (length ((gnc:option-getter test-option))) 3)
      )
      (and (eq? (car (gnc:option-default-value test-option)) 66)
           (= (length (gnc:option-default-value test-option)) 2)
      )
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(4.4 5.5 6.6))))")
      (not (procedure? (gnc:option-scm->kvp test-option)))
      (not (procedure? (gnc:option-kvp->scm test-option)))
      (car ((gnc:option-value-validator test-option) 'any))
      (not(gnc:option-data test-option))
      (not (gnc:option-data-fns test-option))
      (not (gnc:option-strings-getter test-option))
      (not (gnc:option-widget-changed-proc test-option))
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check20)
  (let (
         (test-option (gnc:make-color-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test color option."
                        (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
                        10
                        12
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'color)
      (string=? (gnc:option-documentation test-option) "This is a test color option.")
      (= (length ((gnc:option-getter test-option))) 15)
      ((gnc:option-setter test-option) (list 33))
      (= (car ((gnc:option-getter test-option))) 33)
      (= (length (gnc:option-default-value test-option)) 15)
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) '(33.0))))")
      (not (procedure? (gnc:option-scm->kvp test-option)))
      (not (procedure? (gnc:option-kvp->scm test-option)))
      (not (car ((gnc:option-value-validator test-option) 'any)))
      (not (car ((gnc:option-value-validator test-option) (list 1 2 3 4 5))))
      (not (car ((gnc:option-value-validator test-option) (list 11 12 13 14))))
      (car ((gnc:option-value-validator test-option) (list 1 2 3 4)))
      (and (= (car (gnc:option-data test-option)) 10)
           (= (cadr (gnc:option-data test-option)) 12)
      )
      (not (gnc:option-data-fns test-option))
      (not (gnc:option-strings-getter test-option))
      (not (gnc:option-widget-changed-proc test-option))
      ((gnc:option-setter test-option) (list 1 2 3 4))
      (and (string=? (gnc:color-option->html test-option) "#19334c")
           (string=? (gnc:color-option->hex-string test-option) "19334c")
      )
    )
  )
)

;; -----------------------------------------------------------------------

(define (test-check21)
  (let (
         (test-option (gnc:make-dateformat-option
                        "Option Page General"
                        "Report Name Test"
                        "0a"
                        "This is a test dateformat option."
                        "option-default-dateformat"
                      )
         )
       )
    ;; field validation
    (and
      (string=? (gnc:option-section test-option) "Option Page General")
      (string=? (gnc:option-name test-option) "Report Name Test")
      (string=? (gnc:option-sort-tag test-option) "0a")
      (equal? (gnc:option-type test-option) 'dateformat)
      (string=? (gnc:option-documentation test-option) "This is a test dateformat option.")
      (= (length ((gnc:option-getter test-option))) 4)
      ((gnc:option-setter test-option) 'any)
      (eq? ((gnc:option-getter test-option)) 'any)
      (= (length (gnc:option-default-value test-option)) 4)
      (string=? ((gnc:option-generate-restore-form test-option))
                "(lambda (option) (if option ((gnc:option-setter option) 'any)))")
      (procedure? (gnc:option-scm->kvp test-option))
      (procedure? (gnc:option-kvp->scm test-option))
      (not (car ((gnc:option-value-validator test-option) 'any)))
      (not (car ((gnc:option-value-validator test-option) (list 1 2 3 4 5))))
      (not (car ((gnc:option-value-validator test-option) (list 1 2 3 4))))
      (not (car ((gnc:option-value-validator test-option) (list 'any 2 3 4))))
      (not (car ((gnc:option-value-validator test-option) (list 'any 'any 3 4))))
      (car ((gnc:option-value-validator test-option) (list 'any 'any 3 "any")))
      (not(gnc:option-data test-option))
      (not (gnc:option-data-fns test-option))
      (not (gnc:option-strings-getter test-option))
      (not (gnc:option-widget-changed-proc test-option))
      ;;this one is never used: (export gnc:dateformat-get-format) -> test skipped
    )
  )
)
