(use-modules (gnucash app-utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))
(use-modules (gnucash qif-import))
(use-modules (gnucash string))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-qif-imp")
  (test-qif-parse:fix-year)
  (test-qif-parse:parse-acct-type)
  (test-qif-parse:parse-cleared-field)
  (test-qif-parse:parse-action-field)
  (test-qif-parse:check-date-format)
  (test-qif-parse:check-iso-date-format)
  (test-qif-parse:parse-date/format)
  (test-qif-parse:check-number-format)
  (test-qif-parse:parse-number/format)
  (test-qif-parse:check-number-formats)
  (test-qif-parse:parse-numbers/format)
  (test-qif-split:parse-category)
  (test-end "test-qif-imp"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-parse.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; the following isn't exported but can be tested anyway!
(define qif-parse:fix-year
  (@@ (gnucash qif-import) qif-parse:fix-year))
(define qif-parse:parse-acct-type
  (@@ (gnucash qif-import) qif-parse:parse-acct-type))
(define qif-parse:parse-cleared-field
  (@@ (gnucash qif-import) qif-parse:parse-cleared-field))
(define qif-split:parse-category
  (@@ (gnucash qif-import) qif-split:parse-category))
(define qif-parse:parse-action-field
  (@@ (gnucash qif-import) qif-parse:parse-action-field))
(define qif-parse:check-date-format
  (@@ (gnucash qif-import) qif-parse:check-date-format))
(define qif-parse:parse-date/format
  (@@ (gnucash qif-import) qif-parse:parse-date/format))
(define qif-parse:check-number-format
  (@@ (gnucash qif-import) qif-parse:check-number-format))
(define qif-parse:parse-number/format
  (@@ (gnucash qif-import) qif-parse:parse-number/format))
(define qif-parse:check-number-formats
  (@@ (gnucash qif-import) qif-parse:check-number-formats))
(define qif-parse:parse-numbers/format
  (@@ (gnucash qif-import) qif-parse:parse-numbers/format))


(define (test-qif-parse:fix-year)

  (test-equal "qif-parse:fix-year 1998"
    1998
    (qif-parse:fix-year "1998" 50))

  (test-equal "qif-parse:fix-year ' 0 = 2000"
    2000
    (qif-parse:fix-year "' 0" 50))

  (test-equal "qif-parse:fix-year 98>50 = 1998"
    1998
    (qif-parse:fix-year "98" 50))

  (test-equal "qif-parse:fix-year 48<50 = 2048"
    2048
    (qif-parse:fix-year "48" 50))

  (test-equal "qif-parse:fix-year 19134 = 2034"
    2034
    (qif-parse:fix-year "19134" 50))

  (test-equal "qif-parse:fix-year 102 = 2002"
    2002
    (qif-parse:fix-year "102" 50)))





(define (test-qif-parse:parse-acct-type)
  (test-equal "qif-parse:parse-acct-type ccard"
    (list 3)
    (qif-parse:parse-acct-type "ccard" #f #f))

  (test-equal "qif-parse:parse-acct-type oth s"
    (list 2 0 1)
    (qif-parse:parse-acct-type "oth s" #f #f))

  (test-equal "qif-parse:parse-acct-type zzz"
    (list 0)
    (qif-parse:parse-acct-type "zzz" (const #f) #f)))





(define (test-qif-parse:parse-cleared-field)
  (test-equal "qif-parse:parse-cleared-field xx = reconciled"
    'reconciled
    (qif-parse:parse-cleared-field "xx" (const #f) #f))

  (test-equal "qif-parse:parse-cleared-field cc = cleared"
    'cleared
    (qif-parse:parse-cleared-field "cc" (const #f) #f))

  (test-equal "qif-parse:parse-cleared-field !! = budgeted"
    'budgeted
    (qif-parse:parse-cleared-field "!!" (const #f) #f))

  (test-equal "qif-parse:parse-cleared-field qq = #f"
    #f
    (qif-parse:parse-cleared-field "qq" (const #f) #f)))





(define (test-qif-parse:parse-action-field)
  (test-equal "qif-parse:parse-action-field BuY"
    'buy
    (qif-parse:parse-action-field "BuY" (const #f) #f))

  
  (test-equal "qif-parse:parse-action-field WithDrwX"
    'xout
    (qif-parse:parse-action-field "WithDrwX" (const #f) #f))

  (test-equal "qif-parse:parse-action-field k.gewspx"
    'cgshortx
    (qif-parse:parse-action-field "k.gewspx" (const #f) #f)))





(define (test-qif-parse:check-date-format)

  (test-equal "qif-parse:check-date-format 20/02/1981"
    '(d-m-y)
    (qif-parse:check-date-format
     "20/02/1981"
     '(d-m-y y-m-d y-d-m m-d-y)))

  (test-equal "qif-parse:check-date-format 12/02/1981"
    '(d-m-y m-d-y)
    (qif-parse:check-date-format
     "12/02/1981"
     '(d-m-y y-m-d y-d-m m-d-y)))

  (test-equal "qif-parse:check-date-format 1979/03/03"
    '(y-m-d)
    (qif-parse:check-date-format
     "1979/03/03"
     '(d-m-y y-m-d m-d-y y-d-m)))

  (for-each
   (lambda (delimiter)
     (test-equal
      (string-append "qif-parse:check-date-format 2024" delimiter "01" delimiter "02")
      '(y-m-d)
      (qif-parse:check-date-format
       (string-append "2024" delimiter "01" delimiter "02")
       '(m-d-y d-m-y y-m-d y-d-m))))
   '("-" "/" "." "'"))

  (test-equal "qif-parse:check-date-format rejects leading-year y-d-m"
    '()
    (qif-parse:check-date-format
     "2024-13-01"
     '(m-d-y d-m-y y-m-d y-d-m)))

  (test-equal "qif-parse:check-date-format 03/03/79"
    '(d-m-y m-d-y)
    (qif-parse:check-date-format
     "03/03/79"
     '(d-m-y y-m-d m-d-y y-d-m)))

  (test-equal "qif-parse:check-date-format 03121984"
    '(d-m-y m-d-y)
    (qif-parse:check-date-format
     "03121984"
     '(d-m-y y-m-d m-d-y y-d-m)))

  (test-equal "qif-parse:check-date-format 19790303"
    '(y-m-d y-d-m)
    (qif-parse:check-date-format
     "19790303"
     '(d-m-y y-m-d m-d-y y-d-m))))




(define (test-qif-parse:parse-date/format)

  (test-equal "qif-parse:parse-date/format ok"
    (list 31 01 1981)
    (qif-parse:parse-date/format "31/01/81" 'd-m-y))

  (test-equal "qif-parse:parse-date/format error"
    #f
    (qif-parse:parse-date/format "31/01/81" 'm-d-y))

  (test-equal "qif-parse:parse-date/format ISO"
    (list 2 1 2024)
    (qif-parse:parse-date/format "2024-01-02" 'y-m-d)))


(define (test-qif-parse:check-iso-date-format)
  (define (pad-two-digits number)
    (let ((text (number->string number)))
      (if (= 1 (string-length text))
          (string-append "0" text)
          text)))

  (define (check date expected)
    (test-equal (string-append "ISO date detected: " date)
      expected
      (qif-parse:check-date-format date '(m-d-y d-m-y y-m-d y-d-m))))

  ;; Exercise the month/day boundaries for every delimiter accepted by
  ;; qif-date-compiled-rexp. The parser deliberately does not check leap years
  ;; or the number of days in a particular month.
  (for-each
   (lambda (delimiter)
     (for-each
      (lambda (month)
        (for-each
         (lambda (day)
           (let ((date (string-append "2024" delimiter
                                      (pad-two-digits month) delimiter
                                      (pad-two-digits day))))
             (check date
                    (if (and (<= 1 month 12) (<= 1 day 31))
                        '(y-m-d)
                        '()))))
         '(0 1 30 31 32)))
      '(0 1 11 12 13)))
   '("-" "/" "." "'"))

  (for-each
   (lambda (date) (check date '(y-m-d)))
   '("1931-02-28" "1970/07/15" "2000.11.30" "2024'12'31"
     "2099-02-30"))

  (for-each
   (lambda (date) (check date '()))
   '("2024-00-15" "2024/13/01" "2099.00.15" "2100'12'32")))




(define (test-qif-parse:check-number-format)

  (test-equal "test-qif-parse:check-number-format 1,00"
    '(comma)
    (qif-parse:check-number-format "1,00" '(comma integer decimal)))

  (test-equal "test-qif-parse:check-number-format 999"
    '(comma integer decimal)
    (qif-parse:check-number-format "999" '(comma integer decimal)))
  
  (test-equal "test-qif-parse:check-number-format 999.20"
    '(decimal)
    (qif-parse:check-number-format "999.20" '(comma integer decimal)))

  (test-equal "test-qif-parse:check-number-format 9.200,99"
    '(comma)
    (qif-parse:check-number-format "9.200,99" '(comma integer decimal)))

  (test-equal "test-qif-parse:check-number-format $1000"
    '(comma integer decimal)
    (qif-parse:check-number-format "$1000" '(comma integer decimal))))




(define (test-qif-parse:parse-number/format)
  (test-equal "qif-parse:parse-number/format 1,23"
    123/100
    (qif-parse:parse-number/format "1,23" 'comma))

  (test-equal "qif-parse:parse-number/format 1,234.00"
    1234
    (qif-parse:parse-number/format "1,234.00" 'decimal))

  (test-equal "qif-parse:parse-number/format -1234"
    -1234
    (qif-parse:parse-number/format "-1234" 'integer))

  (test-equal "qif-parse:parse-number/format 1234-"
    -1234
    (qif-parse:parse-number/format "1234-" 'integer))

  (test-equal "qif-parse:parse-number/format 1234"
    1234
    (qif-parse:parse-number/format "1234" 'integer))

  )



(define (test-qif-parse:check-number-formats)
  (test-equal "qif-parse:check-number-formats 1,000 2,000 300"
    '(comma)
    (qif-parse:check-number-formats '("1,00" "2,00" "300,00")
                                    '(decimal comma integer)))

  (test-equal "qif-parse:check-number-formats 10.50 20.54"
    '(decimal)
    (qif-parse:check-number-formats '("10.50" "20.54")
                                    '(decimal comma integer)))

  (test-equal "qif-parse:check-number-formats 1234.00 #f"
    '(comma)
    (qif-parse:check-number-formats '("123,45" #f)
                                    '(decimal comma integer)))

  (test-equal "qif-parse:check-number-formats 1234 4567"
    '(decimal comma integer)
    (qif-parse:check-number-formats '("1234" "4567")
                                    '(decimal comma integer))))

(define (test-qif-parse:parse-numbers/format)
  (test-equal "qif-parse:parse-numbers/format 1,00 2,00 300,00"
    '(1 2 300)
    (qif-parse:parse-numbers/format '("1,00" "2,00" "300,00")
                                    'comma))

  (test-equal "qif-parse:parse-numbers/format 1,00 2,50 3,99"
    '(1 5/2 399/100)
    (qif-parse:parse-numbers/format '("1,00" "2,50" "3,99")
                                    'comma))

  (test-equal "qif-parse:parse-numbers/format 1.00 2.00 300.00"
    '(1 2 300)
    (qif-parse:parse-numbers/format '("1.00" "2.00" "300.00")
                                    'decimal))

  (test-equal "qif-parse:parse-numbers/format 1 2 300"
    '(1 2 300)
    (qif-parse:parse-numbers/format '("1" "2" "300")
                                    'integer))
  
  (test-equal "qif-parse:parse-numbers/format 1 * 300"
    '(1 0 300)
    (qif-parse:parse-numbers/format '("1" "*" "300")
                                    'integer))

  (test-equal "qif-parse:parse-numbers/format 1 #f 300"
    '(1 0 300)
    (qif-parse:parse-numbers/format '("1" #f "300")
                                    'integer)))

(define (test-qif-split:parse-category)
  
  (test-equal "qif-split:parse-category [Transfer]/Class"
    '("Transfer" #t "Class" #f #f #f)
    (qif-split:parse-category #f "[Transfer]/Class"))

  (test-equal "qif-split:parse-category Category/Class"
    '("Category" #f "Class" #f #f #f)
    (qif-split:parse-category #f "Category/Class"))

  (test-equal "qif-split:parse-category Category"
    '("Category" #f "" #f #f #f)
    (qif-split:parse-category #f "Category"))

  (test-equal "qif-split:parse-category [Transfer]"
    '("Transfer" #t "" #f #f #f)
    (qif-split:parse-category #f "[Transfer]"))

  (test-equal "qif-split:parse-category Category/|miscx-category"
    '("Category" #f "" "miscx-category" #f "")
    (qif-split:parse-category #f "Category/|miscx-category"))

  (test-equal "qif-split:parse-category Category/Class|miscx-category"
    '("Category" #f "Class" "miscx-category" #f "")
    (qif-split:parse-category #f "Category/Class|miscx-category"))

  (test-equal "qif-split:parse-category [Transfer]/Class|miscx-category"
    '("Transfer" #t "Class" "miscx-category" #f "")
    (qif-split:parse-category #f "[Transfer]/Class|miscx-category"))

  (test-equal "qif-split:parse-category [Transfer]/Class|miscx-category/miscx-class"
    '("Transfer" #t "Class" "miscx-category" #f "miscx-class")
    (qif-split:parse-category #f "[Transfer]/Class|miscx-category/miscx-class"))

  (test-equal "qif-split:parse-category Category/|[miscx-account]"
    '("Category" #f "" "miscx-account" #t "")
    (qif-split:parse-category #f "Category/|[miscx-account]"))

  (test-equal "qif-split:parse-category Category/|miscx-category/miscx-class"
    '("Category" #f "" "miscx-category" #f "miscx-class")
    (qif-split:parse-category #f "Category/|miscx-category/miscx-class"))

  (test-equal "qif-split:parse-category Category/|[miscx-account]/miscx-class"
    '("Category" #f "" "miscx-account" #t "miscx-class")
    (qif-split:parse-category #f "Category/|[miscx-account]/miscx-class")))
