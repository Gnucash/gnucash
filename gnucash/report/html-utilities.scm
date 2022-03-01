;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-utilities.scm: Useful functions when using the HTML generator.
;; 
;; Modified slightly by David Montenegro 2004.06.18.
;; 
;; Copyright 2001 Christian Stimming <stimming@tu-harburg.de>
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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report html-utilities))

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash app-utils))
(use-modules (gnucash html))
(use-modules (gnucash report report-core))
(use-modules (gnucash report report-utilities))
(use-modules (gnucash report html-style-info))
(use-modules (gnucash report html-text))
(use-modules (gnucash report html-table))
(use-modules (ice-9 match))
(use-modules (web uri))

(export gnc:html-make-empty-cell)
(export gnc:html-make-empty-cells)
(export gnc:account-anchor-text)
(export gnc:split-anchor-text)
(export gnc:transaction-anchor-text)
(export gnc:transaction-doclink-anchor-text)
(export gnc:report-anchor-text)
(export gnc:make-report-anchor)
(export gnc:html-account-anchor)
(export gnc:html-split-anchor)
(export gnc:html-transaction-anchor)
(export gnc:html-transaction-doclink-anchor)
(export gnc:html-invoice-doclink-anchor)
(export gnc:html-price-anchor)
(export gnc:customer-anchor-text)
(export gnc:job-anchor-text)
(export gnc:vendor-anchor-text)
(export gnc:invoice-anchor-text)
(export gnc:owner-anchor-text)
(export gnc:owner-report-text)
(export gnc:assign-colors)
(export gnc:html-table-append-ruler!)
(export gnc:html-make-exchangerates)
(export gnc:html-make-rates-table)
(export gnc:html-render-options-changed)
(export gnc:html-make-generic-warning)
(export gnc:html-make-no-account-warning)
(export gnc:html-make-generic-budget-warning)
(export gnc:html-make-generic-options-warning)
(export gnc:html-make-generic-simple-warning)
(export gnc:html-make-empty-data-warning)
(export gnc:html-make-options-link)
(export gnc:html-js-include)
(export gnc:html-css-include)

;; returns a list with n #f (empty cell) values 
(define (gnc:html-make-empty-cell) #f)
(define (gnc:html-make-empty-cells n)
  (if (> n 0)
      (cons #f (gnc:html-make-empty-cells (- n 1)))
      (list)))

(define (gnc:register-guid type guid)
  (gnc-build-url URL-TYPE-REGISTER (string-append type guid) ""))

(define (gnc:account-anchor-text acct)
  (gnc:register-guid "acct-guid=" (gncAccountGetGUID acct)))

(define (gnc:split-anchor-text split)
  (gnc:register-guid "split-guid=" (gncSplitGetGUID split)))

(define (gnc:transaction-anchor-text trans)
  (gnc:register-guid "trans-guid=" (gncTransGetGUID trans)))

(define (gnc:transaction-doclink-anchor-text trans)
  (gnc:register-guid "trans-doclink-guid=" (gncTransGetGUID trans)))

(define (gnc:invoice-doclink-anchor-text invoice)
  (gnc:register-guid "invoice-doclink-guid=" (gncInvoiceReturnGUID invoice)))

(define (gnc:report-anchor-text report-id)
  (gnc-build-url URL-TYPE-REPORT
		      (string-append "id=" (number->string report-id))
		      ""))

(define (gnc:price-anchor-text price)
  (gnc-build-url URL-TYPE-PRICE
		      (string-append "price-guid=" (gncPriceGetGUID price))
		      ""))

(define (guid-ref idstr type guid)
  (gnc-build-url type (string-append idstr guid) ""))

(define (gnc:customer-anchor-text customer)
  (guid-ref "customer=" URL-TYPE-CUSTOMER (gncCustomerReturnGUID customer)))

(define (gnc:job-anchor-text job)
  (guid-ref "job=" URL-TYPE-JOB (gncJobReturnGUID job)))

(define (gnc:vendor-anchor-text vendor)
  (guid-ref "vendor=" URL-TYPE-VENDOR (gncVendorReturnGUID vendor)))

(define (gnc:employee-anchor-text employee)
  (guid-ref "employee=" URL-TYPE-EMPLOYEE (gncEmployeeReturnGUID employee)))

(define (gnc:invoice-anchor-text invoice)
  (guid-ref "invoice=" URL-TYPE-INVOICE (gncInvoiceReturnGUID invoice)))

(define (gnc:owner-anchor-text owner)
  (let ((type (gncOwnerGetType (gncOwnerGetEndOwner owner))))
    (cond
      ((eqv? type GNC-OWNER-CUSTOMER)
       (gnc:customer-anchor-text (gncOwnerGetCustomer owner)))

      ((eqv? type GNC-OWNER-VENDOR)
       (gnc:vendor-anchor-text (gncOwnerGetVendor owner)))

      ((eqv? type GNC-OWNER-EMPLOYEE)
       (gnc:employee-anchor-text (gncOwnerGetEmployee owner)))

      ((eqv? type GNC-OWNER-JOB)
       (gnc:job-anchor-text (gncOwnerGetJob owner)))

      (else
       ""))))

(define* (gnc:owner-report-text owner acc #:optional date)
  (let* ((end-owner (gncOwnerGetEndOwner owner))
         (type (gncOwnerGetType end-owner)))
    (gnc-build-url
     URL-TYPE-OWNERREPORT
     (string-append
      (cond ((eqv? type GNC-OWNER-CUSTOMER) "owner=c:")
            ((eqv? type GNC-OWNER-VENDOR) "owner=v:")
            ((eqv? type GNC-OWNER-EMPLOYEE) "owner=e:")
            (else "unknown-type="))
      (gncOwnerReturnGUID end-owner)
      (if date (format #f "&enddate=~a" date) "")
      (if (null? acc) "" (string-append "&acct=" (gncAccountGetGUID acc))))
     "")))

;; Make a new report and return the anchor to it. The new report of
;; type 'reportname' will have the option values copied from
;; 'src-options', and additionally this function sets all options
;; according to 'optionlist'. Each element of optionlist is a list of
;; section, name, and value of the function.
(define (gnc:make-report-anchor reportname src-report
				optionlist)
  (let ((src-options (gnc:report-options src-report))
	(options (gnc:make-report-options reportname)))
    (if options
	(begin
	  (gnc:options-copy-values src-options options)
	  (for-each
	   (lambda (l)
	     (let ((o (gnc:lookup-option options (car l) (cadr l))))
	       (if o
		   (gnc:option-set-value o (caddr l))
		   (warn "gnc:make-report-anchor:" reportname
			 " No such option: " (car l) (cadr l)))))
	   optionlist)
	  (let ((id (gnc:make-report reportname options)))
	    (gnc:report-anchor-text id)))
	(warn "gnc:make-report-anchor: No such report: " reportname))))


;; returns the account name as html-text and anchor to the register.
(define (gnc:html-account-anchor acct)
  (gnc:make-html-text (if (and acct (not (null? acct)))
                          (gnc:html-markup-anchor
                           (gnc:account-anchor-text acct)
                           (xaccAccountGetName acct))
                          "")))

(define (gnc:html-split-anchor split text)
  (gnc:make-html-text (if (not (null? (xaccSplitGetAccount split)))
                          (gnc:html-markup-anchor
                           (gnc:split-anchor-text split)
                           text)
                          text)))

(define (gnc:html-transaction-anchor trans text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:transaction-anchor-text trans)
                       text)))

(define (gnc:html-transaction-doclink-anchor trans text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:transaction-doclink-anchor-text trans)
                       text)))

(define (gnc:html-invoice-doclink-anchor invoice text)
  (gnc:make-html-text (gnc:html-markup-anchor
                       (gnc:invoice-doclink-anchor-text invoice)
                       text)))

(define (gnc:html-price-anchor price value)
  (gnc:make-html-text (if price
                          (gnc:html-markup-anchor
                           (gnc:price-anchor-text price)
			   (if value
			       value
			       (gnc-price-get-value price)))
                          value)))

(define (gnc:assign-colors num-colors)
  ;; default CSS colours
  ;; (define base-colors '("red" "orange" "yellow" "green"
  ;;                       "cyan" "blue" "purple" "magenta"
  ;;                       "orchid" "khaki" "gold" "orange"
  ;;                       "red3" "orange3" "yellow3" "green3"
  ;;                       "cyan3" "blue3" "purple3" "magenta3"
  ;;                       "orchid3" "khaki3" "gold3" "orange3"))

  ;; new base-colors from http://clrs.cc/ and flatuicolors.com
  (define base-colors (list "#FF4136" "#FF851B" "#FFDC00" "#2ECC40"
                            "#0074D9" "#001f3f" "#85144b" "#7FDBFF"
                            "#F012BE" "#3D9970" "#39CCCC" "#f39c12"
                            "#e74c3c" "#e67e22" "#9b59b6" "#8e44ad"
                            "#16a085" "#d35400"))
  (let lp ((i 0) (result '()) (colors base-colors))
    (cond
     ((<= num-colors i) (reverse result))
     ((null? colors)    (lp (1+ i) (cons (car base-colors) result) (cdr base-colors)))
     (else              (lp (1+ i) (cons (car colors) result) (cdr colors))))))

(define (gnc:html-table-append-ruler! table colspan)
  (gnc:html-table-append-row!
   table (list (gnc:make-html-table-cell/size
                1 colspan (gnc:make-html-text (gnc:html-markup-hr))))))

;; Create a html-table of all exchange rates. The report-commodity is
;; 'common-commodity', the exchange rates are given through the
;; function 'exchange-fn' and the 'accounts' determine which
;; commodities to show. Returns a html-object, a <html-table>.
(define (gnc:html-make-exchangerates common-commodity exchange-fn accounts)
  (issue-deprecation-warning
   "gnc:html-make-exchangerates is deprecated. use gnc:html-make-rates-table instead.")
  (let* ((comm-list (gnc:accounts-get-commodities accounts common-commodity))
         (entries (length comm-list))
         (markup (lambda (c) (gnc:make-html-table-cell/markup "number-cell" c)))
         (table (gnc:make-html-table)))
    (unless (= 0 entries)
      (for-each
       (lambda (commodity)
         (let* ((orig-amt (gnc:make-gnc-monetary commodity 1))
                (exchanged (exchange-fn orig-amt common-commodity))
                (conv-amount (gnc:gnc-monetary-amount exchanged)))
           (gnc:html-table-append-row!
            table (list (markup orig-amt)
                        (markup (gnc:default-price-renderer common-commodity
                                                            conv-amount))))))
       comm-list)
      (gnc:html-table-set-col-headers!
       table (list (gnc:make-html-table-header-cell/size
                    1 2 (NG_ "Exchange rate" "Exchange rates" entries)))))
    table))

;; Create a html-table of all prices. The report-currency is
;; 'currency', The prices are given through the function 'price-fn'
;; and the 'accounts' determine which commodities to show. Returns a
;; html-object, a <html-table>. price-fn is easily obtained from
;; gnc:case-price-fn
(define (gnc:html-make-rates-table currency price-fn accounts)
  (define (cell c) (gnc:make-html-table-cell/markup "number-cell" c))
  (define table (gnc:make-html-table))
  (let lp ((comm-list (gnc:accounts-get-commodities accounts currency)) (entries 0))
    (match comm-list
      (()
       (unless (zero? entries)
         (gnc:html-table-set-col-headers!
          table (list (gnc:make-html-table-header-cell/size
                       1 2 (NG_ "Exchange rate" "Exchange rates" entries)))))
       table)
      ((comm . rest)
       (gnc:html-table-append-row!
        table
        (list (cell (gnc:make-gnc-monetary comm 1))
              (cell (gnc:default-price-renderer currency (price-fn comm)))))
       (lp rest (1+ entries))))))


(define (gnc:html-make-generic-budget-warning report-title-string)
  (gnc:html-make-generic-simple-warning
    report-title-string
    (G_ "No budgets exist. You must create at least one budget.")))


(define (gnc:html-make-generic-simple-warning report-title-string message)
  (gnc:make-html-text
   (gnc:html-markup-h3 (string-append report-title-string ":"))
   (gnc:html-markup-h3 "")
   (gnc:html-markup-p message)))


(define (gnc:html-make-options-link report-id)
   (if report-id
    (gnc:html-markup-p
     (gnc:html-markup-anchor
      (gnc-build-url URL-TYPE-OPTIONS
       (string-append "report-id=" (format #f "~a" report-id))
       "")
      (G_ "Edit report options")))))

(define* (gnc:html-render-options-changed options #:optional plaintext?)
  ;; options -> html-object or string, depending on plaintext?.  This
  ;; summarises options that were changed by the user. Set plaintext?
  ;; to #t for unit-tests only.
  (define (disp d)
    ;; option-value -> string.  The option is passed to various
    ;; scm->string converters; ultimately a generic stringify
    ;; function handles symbol/string/other types.
    (define (try proc)
      ;; Try proc with d as a parameter, catching 'wrong-type-arg
      ;; exceptions to return #f to the or evaluator.
      (catch 'wrong-type-arg
        (lambda () (proc d))
        (const #f)))
    (or (and (boolean? d) (if d (G_ "Enabled") (G_ "Disabled")))
        (and (null? d) "null")
        (and (list? d) (string-join (map disp d) ", "))
        (and (pair? d) (format #f "~a . ~a"
                               (car d)
                               (if (eq? (car d) 'absolute)
                                   (qof-print-date (cdr d))
                                   (disp (cdr d)))))
        (try gnc-commodity-get-mnemonic)
        (try xaccAccountGetName)
        (try gnc-budget-get-name)
        (format #f "~a" d)))
  (let ((render-list '())
        (report-list (and=> (gnc:lookup-option options "__general" "report-list")
                            gnc:option-value)))
    (define (add-option-if-changed option)
      (let* ((section (gnc:option-section option))
             (name (gnc:option-name option))
             (default-value (gnc:option-default-value option))
             (value (gnc:option-value option))
             (retval (cons (format #f "~a / ~a" section name)
                           (disp value))))
        (if (not (or (equal? default-value value)
                     (char=? (string-ref section 0) #\_)))
            (addto! render-list retval))))
    (define (name-fn name) (if plaintext? name (gnc:html-markup-b name)))
    (define br (if plaintext? "\n" (gnc:html-markup-br)))
    (for-each
     (lambda (child)
       (let ((report (gnc-report-find (car child))))
         (addto! render-list (cons "Embedded Report" (gnc:report-name report)))))
     (or report-list '()))
    (gnc:options-for-each add-option-if-changed options)
    (let lp ((render-list (reverse render-list)) (acc '()))
      (match render-list
        (() (if plaintext? (string-concatenate acc) (apply gnc:make-html-text acc)))
        (((name . val) . rest) (lp rest (cons* (name-fn name) ": " val br acc)))))))

(define (gnc:html-make-generic-warning
         report-title-string report-id
         warning-title-string warning-string)
  (gnc:make-html-text
   (gnc:html-markup-h3 (string-append (G_ report-title-string) ":"))
   (gnc:html-markup-h3 warning-title-string)
   (gnc:html-markup-p warning-string)
   (gnc:html-make-options-link report-id)))

(define (gnc:html-make-generic-options-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    ""
    (G_ "This report requires you to specify certain report options.")))

(define (gnc:html-make-no-account-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    (G_ "No accounts selected")
    (G_ "This report requires accounts to be selected in the report options.")))

(define (gnc:html-make-empty-data-warning
         report-title-string report-id)
  (gnc:html-make-generic-warning
    report-title-string
    report-id
    (G_ "No data")
    (G_ "The selected accounts contain no data/transactions (or only zeroes) for the selected time period")))

(define unreserved-chars-rfc3986
  (char-set-union
   (string->char-set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")
   (string->char-set ":/?[]@")         ;gen-delims
   (string->char-set "-._~")))

;;path must be absolute. On Windows an absolute path begins with a
;;drive letter followed by a colon.
(define (make-uri path)
  (let ((uri-path (uri-encode (gnc:substring-replace path "\\" "/")
               #:unescaped-chars unreserved-chars-rfc3986)))
  (string-append
   (if (char=? (string-ref uri-path 0) #\/) "file://" "file:///")
   uri-path)))

(define (gnc:html-js-include file)
  (format #f
          "<script language=\"javascript\" type=\"text/javascript\" src=~s></script>\n"
          (make-uri (gnc-resolve-file-path file))))

(define (gnc:html-css-include file)
  (format #f
          "<link rel=\"stylesheet\" type=\"text/css\" href=~s />\n"
          (make-uri (gnc-resolve-file-path file))))



