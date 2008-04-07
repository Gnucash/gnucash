;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-file.scm
;;;
;;;  Read a QIF file into a <qif-file> object.
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash core-utils))
(use-modules (ice-9 regex))
(use-modules (srfi srfi-13))

(cond
 ((or (string=? "1.3.4" (version))
      (string=? "1.4" (substring (version) 0 3))) #f)
 (else (use-modules (ice-9 rdelim))))

(define qif-bad-numeric-rexp
  (make-regexp "^\\.\\.\\."))

(define (not-bad-numeric-string? input)
  (let ((match (regexp-exec qif-bad-numeric-rexp input)))
    (if match #f #t)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:read-file
;;
;;  Suck in all the lines. Don't do any string interpretation,
;;  just store the fields "raw".
;;
;; FIXME: This function really should be able to return multiple
;;        errors and warnings rather than a single one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:read-file self path ticker-map window)
  (let ((progress-dialog '())
        (retval #f))

  ;; This procedure does all the work. We'll define it, then call it safely.
  (define (private-read)
    (let ((qstate-type #f)
          (current-xtn #f)
          (current-split #f)
          (current-account-name #f)
          (last-seen-account-name #f)
          (default-split #f)
          (first-xtn #f)
          (ignore-accounts #f)
          (return-val #t)
          (line #f)
          (tag #f)
          (value #f)
          (heinous-error #f)
          (missing-date-warned #f)
          (delimiters (string #\cr #\nl))
          (file-stats #f)
          (file-size 0)
          (bytes-read 0))

      (qif-file:set-path! self path)
      (if (not (access? path R_OK))
          ;; A UTF-8 encoded path won't succeed on some systems, such as
          ;; Windows XP. Try encoding the path according to the locale.
          (set! path (gnc-locale-from-utf8 path)))
      (set! file-stats (stat path))
      (set! file-size (stat:size file-stats))


      (if (> file-size 10000)
          (begin
            (set! progress-dialog (gnc-progress-dialog-new window #f))
            (gnc-progress-dialog-set-title progress-dialog (_ "Progress"))
            (gnc-progress-dialog-set-heading progress-dialog
                                             (_ "Loading QIF file..."))))

      (with-input-from-file path
        (lambda ()
          ;; loop over lines
          (let line-loop ()
            (set! line (read-delimited delimiters))
            (if (and (not (eof-object? line))
                     (not (string=? line "")))
                (begin
                  ;; Add to the bytes-read tally.
                  (set! bytes-read
                        (+ bytes-read 1 (string-length line)))

                  ;; Pick the 1-char tag off from the remainder of the line.
                  (set! tag (string-ref line 0))
                  (set! value (substring line 1))

                  ;; If the line doesn't conform to UTF-8, try a default
                  ;; character set conversion based on the locale. If that
                  ;; fails, remove any invalid characters.
                  (if (not (gnc-utf8? value))
                      (let ((converted-value (gnc-locale-to-utf8 value)))
                        (if (or (string=? converted-value "")
                                (not (gnc-utf8? converted-value)))
                            (begin
                              (set! value (gnc-utf8-strip-invalid-strdup value))
                              (set! return-val
                                    (list #t (string-append
                               (_ "This file is not encoded in UTF-8 or ASCII.")
                               " "
                               (_ "Some characters have been discarded."))))
                              (gnc:warn "qif-file:read-file:"
                                        " stripping invalid characters."
                                        "\nAfter: [" value "]"))
                            (begin
                              (set! return-val
                                    (list #t (string-append
                               (_ "This file is not encoded in UTF-8 or ASCII.")
                               " "
                               (_ "Some characters have been converted according to your locale."))))
                              (gnc:warn "qif-file:read-file:"
                                        " converting characters by locale."
                                        "\nBefore: [" value "]"
                                        "\nAfter:  [" converted-value "]")
                              (set! value converted-value)))))

                  (if (eq? tag #\!)
                      ;; The "!" tag has the highest precedence and is used
                      ;; to switch between different sections of the file.
                      (let ((old-qstate qstate-type))
                        (set! qstate-type (qif-parse:parse-bang-field value))
                        (case qstate-type
                          ;; Transaction list for a particular account
                          ((type:bank type:cash type:ccard type:invst type:port
                                      #{type:oth\ a}#  #{type:oth\ l}#)
                           (if ignore-accounts
                               (set! current-account-name
                                     last-seen-account-name))
                           (set! ignore-accounts #f)
                           (set! current-xtn (make-qif-xtn))
                           (set! default-split (make-qif-split))
                           (set! first-xtn #t))

                          ;; Class list
                          ((type:class)
                           (set! current-xtn (make-qif-class)))

                          ;; Category list
                          ((type:cat)
                           (set! current-xtn (make-qif-cat)))

                          ;; Account list
                          ((account)
                           (set! current-xtn (make-qif-acct)))

                          ;; Security list
                          ((type:security)
                           (set! current-xtn (make-qif-stock-symbol)))

                          ;; Memorized transaction list
                          ((type:memorized)
                           ;; Not supported. We really should warn the user.
                           #f)

                          ;; Security price list
                          ((type:prices)
                           ;; Not supported. We really should warn the user.
                           #f)

                          ((option:autoswitch)
                           (set! ignore-accounts #t))

                          ((clear:autoswitch)
                           (set! ignore-accounts #f))

                          (else
                           ;; Ignore any other "option:" identifiers and
                           ;; just return to the previously known !type
                           (if (string-match "^option:"
                                             (symbol->string qstate-type))
                               (begin
                                 (gnc:warn "qif-file:read-file:"
                                           " ignoring '" qstate-type
                                           "' option.")
                                 (set! qstate-type old-qstate))))))


                      ;; It's not a "!" tag, so the meaning depends on what
                      ;; type of section we are currently working on.
                      (case qstate-type

                        ;;;;;;;;;;;;;;;;;;;;;;
                        ;; Transaction list ;;
                        ;;;;;;;;;;;;;;;;;;;;;;

                        ((type:bank type:cash type:ccard type:invst type:port
                                    #{type:oth\ a}#  #{type:oth\ l}#)
                         (case tag
                           ;; D : transaction date
                           ((#\D)
                            (qif-xtn:set-date! current-xtn value))

                           ;; T : total amount
                           ((#\T)
                            (if (and default-split
                                    (not-bad-numeric-string? value))
                                (qif-split:set-amount! default-split value)))

                           ;; P : payee
                           ((#\P)
                            (qif-xtn:set-payee! current-xtn value))

                           ;; A : address
                           ;; multiple "A" lines are appended together with
                           ;; newlines; some Quicken files have a lot of
                           ;; A lines.
                           ((#\A)
                            (qif-xtn:set-address!
                             current-xtn
                             (let ((current (qif-xtn:address current-xtn)))
                               (if (not (string? current))
                                   (set! current ""))
                               (string-append current "\n" value))))

                           ;; N : For transactions involving a security, this
                           ;; is the investment action. For all others,  this
                           ;; is a check number or transaction number.
                           ((#\N)
                            (if (or (eq? qstate-type 'type:invst)
                                    (eq? qstate-type 'type:port))
                                (qif-xtn:set-action! current-xtn value)
                                (qif-xtn:set-number! current-xtn value)))

                           ;; C : cleared flag
                           ((#\C)
                            (qif-xtn:set-cleared! current-xtn value))

                           ;; M : memo
                           ((#\M)
                            (if default-split
                                (qif-split:set-memo! default-split value)))

                           ;; I : share price (stock transactions)
                           ((#\I)
                            (qif-xtn:set-share-price! current-xtn value))

                           ;; Q : number of shares (stock transactions)
                           ((#\Q)
                            (qif-xtn:set-num-shares! current-xtn value))

                           ;; Y : name of security (stock transactions)
                           ((#\Y)
                            (qif-xtn:set-security-name! current-xtn value))

                           ;; O : commission (stock transactions)
                           ((#\O)
                            (qif-xtn:set-commission! current-xtn value))

                           ;; L : category
                           ((#\L)
                            (if default-split
                                (qif-split:set-category! default-split value)))

                           ;; S : split category
                           ;; At this point we are ignoring the default-split
                           ;; completely, but save it for later -- we need it
                           ;; to determine whether to reverse the split values.
                           ((#\S)
                            (set! current-split (make-qif-split))
                            (if default-split
                                (qif-xtn:set-default-split! current-xtn
                                                            default-split))
                            (set! default-split #f)
                            (qif-split:set-category! current-split value)
                            (qif-xtn:set-splits!
                               current-xtn
                               (cons current-split
                                     (qif-xtn:splits current-xtn))))

                           ;; E : split memo
                           ((#\E)
                            (if current-split
                                (qif-split:set-memo! current-split value)))

                           ;; $ : split amount (if there are splits)
                           ((#\$)
                            (if (and current-split
                                     (not-bad-numeric-string? value))
                                (qif-split:set-amount! current-split value)))

                           ;; ^ : end-of-record
                           ((#\^)
                            (if (null? (qif-xtn:splits current-xtn))
                                (qif-xtn:set-splits! current-xtn
                                                     (list default-split)))
                            (if first-xtn
                                (let ((opening-balance-payee
                                       (qif-file:process-opening-balance-xtn
                                        self current-account-name current-xtn
                                        qstate-type)))
                                  (if (not current-account-name)
                                      (set! current-account-name
                                            opening-balance-payee))
                                  (set! first-xtn #f)))

                            (if (and (or (eq? qstate-type 'type:invst)
                                         (eq? qstate-type 'type:port))
                                     (not (qif-xtn:security-name current-xtn)))
                                (qif-xtn:set-security-name! current-xtn ""))

                            (qif-xtn:set-from-acct! current-xtn
                                                    current-account-name)

                            (if (qif-xtn:date current-xtn)
                                (qif-file:add-xtn! self current-xtn)
                                ;; The date is missing! Warn the user if they
                                ;; haven't been warned already.
                                (if (not missing-date-warned)
                                    (begin
                                      (set! missing-date-warned #t)
                                      (gnc-warning-dialog '() (string-append
                               (_ "One or more transactions is missing a date.")
                               "\n"
                               (_ "Some transactions may be discarded."))))))

                            ;;(write current-xtn) (newline)
                            (set! current-xtn (make-qif-xtn))
                            (set! current-split #f)
                            (set! default-split (make-qif-split)))))


                        ;;;;;;;;;;;;;;;;
                        ;; Class list ;;
                        ;;;;;;;;;;;;;;;;

                        ((type:class)
                         (case tag
                           ;; N : name
                           ((#\N)
                            (qif-class:set-name! current-xtn value))

                           ;; D : description
                           ((#\D)
                            (qif-class:set-description! current-xtn value))

                           ;; R : tax copy designator (ignored for now)
                           ((#\R)
                            #t)

                           ;; end-of-record
                           ((#\^)
                            (qif-file:add-class! self current-xtn)
                            (set! current-xtn (make-qif-class)))

                           (else
                            (gnc:warn "qif-file:read-file:"
                                      " ignoring class '" tag "' line."
                                      "\nLine content: [" line "]"))))


                        ;;;;;;;;;;;;;;;;;;
                        ;; Account List ;;
                        ;;;;;;;;;;;;;;;;;;

                        ((account)
                         (case tag
                           ((#\N)
                            (qif-acct:set-name! current-xtn value)
                            (set! last-seen-account-name value))
                           ((#\D)
                            (qif-acct:set-description! current-xtn value))
                           ((#\T)
                            (qif-acct:set-type! current-xtn value))
                           ((#\L)
                            (qif-acct:set-limit! current-xtn value))
                           ((#\B)
                            (qif-acct:set-budget! current-xtn value))
                           ((#\^)
                            (if (not ignore-accounts)
                                (set! current-account-name
                                      (qif-acct:name current-xtn)))
                            (qif-file:add-account! self current-xtn)
                            (set! current-xtn (make-qif-acct)))))


                        ;;;;;;;;;;;;;;;;;;;
                        ;; Category list ;;
                        ;;;;;;;;;;;;;;;;;;;

                        ((type:cat)
                         (case tag
                           ;; N : category name
                           ((#\N)
                            (qif-cat:set-name! current-xtn value))

                           ;; D : category description
                           ((#\D)
                            (qif-cat:set-description! current-xtn value))

                           ;; T : is this a taxable category?
                           ((#\T)
                            (qif-cat:set-taxable! current-xtn #t))

                           ;; E : is this an expense category?
                           ((#\E)
                            (qif-cat:set-expense-cat! current-xtn #t))

                           ;; I : is this an income category?
                           ((#\I)
                            (qif-cat:set-income-cat! current-xtn #t))

                           ;; R : tax form/line designator
                           ((#\R)
                            (qif-cat:set-tax-class! current-xtn value))

                           ;; B : budget amount.  not really supported.
                           ((#\B)
                            (qif-cat:set-budget-amt! current-xtn value))

                           ;; end-of-record
                           ((#\^)
                            (qif-file:add-cat! self current-xtn)
                            (set! current-xtn (make-qif-cat)))

                           (else
                            (gnc:warn "qif-file:read-file:"
                                      " ignoring category '" tag "' line."
                                      "\nLine content: [" line "]"))))


                        ;;;;;;;;;;;;;;;;;;;
                        ;; Security list ;;
                        ;;;;;;;;;;;;;;;;;;;

                        ((type:security)
                         (case tag
                           ;; N : stock name
                           ((#\N)
                            (qif-stock-symbol:set-name! current-xtn value))

                           ;; S : ticker symbol
                           ((#\S)
                            (qif-stock-symbol:set-symbol! current-xtn value))

                           ;; T : type
                           ((#\T)
                            (qif-stock-symbol:set-type! current-xtn value))

                           ;; G : asset class (ignored)
                           ((#\G)
                            #t)

                           ;; end-of-record
                           ((#\^)
                            (qif-ticker-map:add-ticker! ticker-map current-xtn)
                            (set! current-xtn (make-qif-stock-symbol)))

                           (else
                            (gnc:warn "qif-file:read-file:"
                                      " ignoring security '" tag "' line."
                                      "\nLine content: [" line "]"))))


                        ;; trying to sneak one by, eh?
                        (else
                          (if (and (not qstate-type)
                                   (not (string=? (string-trim line) "")))
                              (begin
                                (gnc:warn "qif-file:read-file:"
                                          " file does not appear to be a QIF."
                                          "\nLine content: [" line "]")
                                (set! return-val
                                      (list #f "File does not appear to be a QIF file."))
                                (set! heinous-error #t))))))

                  ;; Update the progress bar for each line read.
                  (if (not (null? progress-dialog))
                      (begin
                        (gnc-progress-dialog-set-value
                         progress-dialog (/ bytes-read file-size))
                        (gnc-progress-dialog-update progress-dialog)))

                  ;; This is if we read a normal (non-null, non-eof) line...
                  (if (not heinous-error)
                      (line-loop)))

                ;; ...and this is if we read a null or eof line.
                (if (and (not heinous-error)
                         (not (eof-object? line)))
                    (line-loop))))))

      ;; Reverse the transaction list so xtns are in the same order that
      ;; they appeared in the file.  This is important in a few cases.
      (qif-file:set-xtns! self (reverse (qif-file:xtns self)))

      return-val))


    ;; Safely read the file.
    (set! retval (gnc:backtrace-if-exception private-read))

    ;; Get rid of the progress dialog (if any).
    (if (not (null? progress-dialog))
        (gnc-progress-dialog-destroy progress-dialog))

    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:process-opening-balance-xtn
;;
;;  This gets called for the first transaction after a !Type: tag.
;;
;;  If the first transaction after a !Type: tag has a payee of
;;  "Opening Balance", we have to massage the transaction a little.
;;  The meaning of an OB transaction is "transfer from Equity to the
;;  account specified in the L line." idiomatically, ms-money and some
;;  others use this transaction instead of an Account record to
;;  specify "this" account (the from-account for all following
;;  transactions), so we have to allow for that.
;;
;;  Even if the payee isn't "Opening Balance", we know that if there's
;;  no default from-account by this time, we need to set one.  In that
;;  case, we set the default account based on the file name.
;;
;;  If we DO know the account already, and this is a tranfer to it,
;;  it's also an opening balance regardless of the payee.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:process-opening-balance-xtn self acct-name xtn type)
  (let ((payee (qif-xtn:payee xtn))
        (category (qif-split:category (car (qif-xtn:splits xtn))))
        (cat-is-acct? (qif-split:category-is-account?
                       (car (qif-xtn:splits xtn))))
        (security (qif-xtn:security-name xtn)))
    (if (or (and (not acct-name)
                 (not security)
                 payee (string? payee)
                 (string=? (string-remove-trailing-space payee)
                           "Opening Balance")
                 cat-is-acct?)
            (and acct-name (string? acct-name)
                 (string=? acct-name category)
                 (not security)))
        ;; this is an explicit "Opening Balance" transaction.  we need
        ;; to change the category to point to the equity account that
        ;; the opening balance comes from.
        (begin
          (qif-split:set-category-private! (car (qif-xtn:splits xtn))
                                           (default-equity-account))
          (qif-split:set-category-is-account?! (car (qif-xtn:splits xtn)) #t)
          (set! acct-name category)))
    acct-name))

;; return #t if all xtns have a non-#f from-acct otherwise, we will
;; need to ask for an explicit account.
(define (qif-file:check-from-acct self)
  (let ((retval #t))
    (for-each
     (lambda (xtn)
       (if (not (qif-xtn:from-acct xtn))
           (set! retval #f)))
     (qif-file:xtns self))
    retval))

;; if the date format was ambiguous, this will get called to reparse.
(define (qif-file:reparse-dates self new-format)
  (check-and-parse-field
   qif-xtn:date qif-xtn:set-date! equal?
   qif-parse:check-date-format (list new-format)
   qif-parse:parse-date/format
   (qif-file:xtns self)
   qif-parse:print-date
   'error-on-ambiguity
   (lambda (t e) e) 'date))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:parse-fields-results
;;
;;  Take the results from qif-file:parse fields and find the
;;  results for a particular type of parse.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-fields-results results type)
  (define (test-results results)
    (if (null? results) #f
        (let* ((this-res (car results))
               (this-type (car this-res)))
          (if (eq? this-type type)
              (cdr this-res)
              (test-results (cdr results))))))

  (if results (test-results results) #f))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-file:parse-fields
;;
;;  Take a previously-read qif file and convert fields from
;;  strings to the appropriate type.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-file:parse-fields self)
;  (false-if-exception
   (let* ((error #f)
          (all-ok #f)
          (set-error
           (lambda (t e)
             (if (not error)
                 (set! error (list (cons t e)))
                 (set! error (cons (cons t e) error)))))
          (errlist-to-string
           (lambda (lst)
             (with-output-to-string
               (lambda ()
                 (for-each
                  (lambda (elt)
                    (display elt))
                  lst))))))
     (and
      ;; fields of categories.
      (check-and-parse-field
       qif-cat:tax-class qif-cat:set-tax-class! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:cats self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'tax-class)

      (check-and-parse-field
       qif-cat:budget-amt qif-cat:set-budget-amt! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:cats self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'budget-amt)

      ;; fields of accounts
      (check-and-parse-field
       qif-acct:limit qif-acct:set-limit! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:accounts self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'limit)

      (check-and-parse-field
       qif-acct:budget qif-acct:set-budget! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:accounts self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'budget)

      (parse-field
       qif-acct:type qif-acct:set-type!
       qif-parse:parse-acct-type (qif-file:accounts self)
       set-error)

      ;; fields of transactions
      (check-and-parse-field
       qif-xtn:date qif-xtn:set-date! equal?
       qif-parse:check-date-format '(m-d-y d-m-y y-m-d y-d-m)
       qif-parse:parse-date/format
       (qif-file:xtns self)
       qif-parse:print-date
       'error-on-ambiguity
       set-error 'date)

      (parse-field
       qif-xtn:cleared qif-xtn:set-cleared!
       qif-parse:parse-cleared-field (qif-file:xtns self) set-error)

      (parse-field
       qif-xtn:action qif-xtn:set-action!
       qif-parse:parse-action-field (qif-file:xtns self) set-error)

      (check-and-parse-field
       qif-xtn:share-price qif-xtn:set-share-price! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:xtns self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'share-price)

      (check-and-parse-field
       qif-xtn:num-shares qif-xtn:set-num-shares! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:xtns self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'num-shares)

      (check-and-parse-field
       qif-xtn:commission qif-xtn:set-commission! gnc-numeric-equal
       qif-parse:check-number-format '(decimal comma)
       qif-parse:parse-number/format (qif-file:xtns self)
       qif-parse:print-number
       'guess-on-ambiguity
       set-error 'commission)

      ;; this one's a little tricky... it checks and sets all the
      ;; split amounts for the transaction together.
      (check-and-parse-field
       qif-xtn:split-amounts qif-xtn:set-split-amounts! gnc-numeric-equal
       qif-parse:check-number-formats '(decimal comma)
       qif-parse:parse-numbers/format (qif-file:xtns self)
       qif-parse:print-numbers
       'guess-on-ambiguity
       set-error 'split-amounts)

      (begin
        (set! all-ok #t)
        #t))

     (cond (error
            (cons all-ok error))
           (#t #t))))
;)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  parse-field
;;
;;  A simplified version of check-and-parse-field which just
;;  calls the parser on every instance of the field in the set
;;  of objects.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (parse-field getter setter parser objects errormsg)
  (for-each
   (lambda (obj)
     (let ((unparsed (getter obj)))
       (if (and unparsed (string? unparsed))
           (setter obj (parser unparsed)))))
   objects)
  #t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  check-and-parse-field
;;
;;  This is a semi-generic routine to apply a format check and
;;  parsing routine to fields that can have multiple possible
;;  formats.  In this case, any amount field cam be decimal or
;;  comma radix and the date field can be any of several possible
;;  types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (check-and-parse-field getter setter equiv-thunk checker
                               formats parser objects printer
                               on-error errormsg errortype)
  ;; first find the right format for the field
  (let ((do-parsing #f)
        (retval #t)
        (format #f))
    ;; loop over objects.  If the formats list ever gets down
    ;; to 1 element, we can stop right there.
    (if (not (null? objects))
        (let loop ((current (car objects))
                   (rest (cdr objects)))
          (let ((val (getter current)))
            (if val
                (begin
                  (set! do-parsing #t)
                  (set! formats (checker val formats)))))
          (if (and (not (null? formats))
                   ;; (not (null? (cdr formats)))
                   (not (null? rest)))
              (loop (car rest) (cdr rest)))))

    ;; if there's nothing left in formats, there's no format that will
    ;; fit all the values for a given field.  We have to give up at
    ;; that point.

    ;; If there are multiple items in formats, we look at the on-error
    ;; arg.  If it's 'guess-on-ambiguity, we take the default (first)
    ;; item in the list.  This is not super great.  if it's
    ;; 'fail-on-ambiguity (or anything else, actually) we return the
    ;; list of acceptable formats.

    (cond
     ((or (not formats)
          (null? formats))
      (errormsg errortype "Data for number or date does not match a known format.")
      (set! retval #f)
      (set! do-parsing #f))
     ((and (not (null? (cdr formats))) do-parsing)
      ;; there are multiple formats that fit.  If they all produce the
      ;; same interpretation for every data point in the set, then
      ;; just ignore the format ambiguity.  Otherwise, it's really an
      ;; error.  ATM since there's no way to correct the error let's
      ;; just leave it be.
      (if (or (eq? on-error 'guess-on-ambiguity)
              (all-formats-equivalent? getter parser equiv-thunk formats
                                       objects printer errormsg errortype))
          (set! format (car formats))
          (begin
            (errormsg errortype formats)
            (set! do-parsing #f)
            (set! retval #t))))
     (#t
      (set! format (car formats))))

    ;; do-parsing is false if there were no objects with non-#f values
    ;; in the field, or the data format is ambiguous and
    ;; 'fail-on-ambiguity was passed.  We would have had to look at
    ;; all of them once, but at least not twice.
    (if do-parsing
        (for-each
         (lambda (current)
           (let ((val (getter current))
                 (parsed #f))
             (if val
                 (begin
                   (set! parsed (parser val format))
                   (if parsed
                       (setter current parsed)
                       (begin
                         (set! retval #f)
                         (errormsg errortype
                          "Data format inconsistent in QIF file.")))))))
         objects))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  all-formats-equivalent?
;;
;;  This predicate checks for the off chance that even though
;;  there are multiple possible interpretations they are all the
;;  same. (i.e. the numbers "1000 2000 3000 4000" could be
;;  interpreted as decimal or comma radix, but who cares?  The
;;  values will be the same).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (all-formats-equivalent? getter parser equiv-thunk formats objects
                                 printer errormsg errortype)
  (let ((all-ok #t))
    (let obj-loop ((objlist objects))
      (let* ((unparsed (getter (car objlist)))
             (parsed #f))
        (if (string? unparsed)
            (begin
              (set! parsed (parser unparsed (car formats)))
              (for-each
               (lambda (fmt)
                 (let ((this-parsed (parser unparsed fmt)))
                   (if (not (equiv-thunk parsed this-parsed))
                       (begin
                         (set! all-ok #f)
                         (errormsg errortype
                          (with-output-to-string
                            (lambda ()
                              (for-each
                               (lambda (elt)
                                 (display elt))
                               (list
                                "Parse ambiguity : between formats "
                                formats "\nValue " unparsed " could be "
                                (printer parsed) " or "
                                (printer this-parsed)
                                "\nand no evidence exists to distinguish."
                                "\nUsing " (printer parsed) ". "
                                "\nSee help for more info.")))))))))
               (cdr formats))))
        (if (and all-ok (not (null? (cdr objlist))))
            (obj-loop (cdr objlist)))))
    all-ok))
