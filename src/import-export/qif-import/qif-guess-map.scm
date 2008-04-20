;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-guess-map.scm
;;;  guess (or load from prefs) mappings from QIF cats/accts to gnc
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (srfi srfi-13))

(define GNC-BANK-TYPE 0)
(define GNC-CASH-TYPE 1)
(define GNC-ASSET-TYPE 2)
(define GNC-LIABILITY-TYPE 4)
(define GNC-CCARD-TYPE 3)
(define GNC-STOCK-TYPE 5)
(define GNC-MUTUAL-TYPE 6)
(define GNC-INCOME-TYPE 8)
(define GNC-EXPENSE-TYPE 9)
(define GNC-EQUITY-TYPE 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:load-map-prefs
;;
;;  Load the saved mappings file, and make a table of all the
;;  accounts with their full names and pointers for later
;;  guessing of a mapping.
;;
;;  We'll be returning a list with the following members:
;;   - a list of all the known gnucash accounts in
;;     (shortname fullname Account*) format
;;   - a hash of QIF account name to gnucash account info
;;   - a hash of QIF category to gnucash account info
;;   - a hash of QIF memo/payee to gnucash account info
;;     (older saved prefs may not have this one)
;;   - a hash of QIF stock name to gnc-commodity*
;;     (older saved prefs may not have this one)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:load-map-prefs)

  ;; This procedure builds a list of all descendants of an existing
  ;; GnuCash account. Each member of the list is itself a list with
  ;; the form: (shortname fullname Account*)
  (define (extract-all-account-info an-account root-name)
    (if (null? an-account)
        '()
        (let ((children-list (gnc-account-get-children-sorted an-account))
              (names '()))

          ;; Recursively walk the account tree.
          (for-each
           (lambda (child-acct)
             (let* ((name (xaccAccountGetName child-acct))
                    (fullname
                     (if (string? root-name)
                         (string-append root-name
                                        (gnc-get-account-separator-string)
                                        name)
                         name)))
               (set! names
                     (append (cons (list name fullname child-acct)
                                   (extract-all-account-info child-acct fullname))
                             names))))
           children-list)
          names)))

  (define (safe-read)
    (false-if-exception
     (read)))

  (let* ((pref-filename (gnc-build-dotgnucash-path "qif-accounts-map"))
         (results '()))

    ;; Get the user's saved mappings.
    (if (access? pref-filename R_OK)
        ;; We have access to the mapping file (qif-accounts-map).
        (with-input-from-file pref-filename
          (lambda ()
            (let ((qif-account-list #f)
                  (qif-cat-list #f)
                  (qif-memo-list #f)
                  (qif-stock-list #f)
                  (qif-account-hash #f)
                  (qif-cat-hash #f)
                  (qif-memo-hash #f)
                  (qif-stock-hash #f)
                  (saved-sep #f))

              ;; Read the mapping file.
              (set! qif-account-list (safe-read))
              (set! qif-cat-list (safe-read))
              (set! qif-memo-list (safe-read))
              (set! qif-stock-list (safe-read))
              (set! saved-sep (safe-read))

              ;; Process the QIF account mapping.
              (if (not (list? qif-account-list))
                  (set! qif-account-hash (make-hash-table 20))
                  (set! qif-account-hash
                        (qif-import:read-map qif-account-list
                                             saved-sep)))

              ;; Process the QIF category mapping.
              (if (not (list? qif-cat-list))
                  (set! qif-cat-hash (make-hash-table 20))
                  (set! qif-cat-hash (qif-import:read-map qif-cat-list
                                                          saved-sep)))

              ;; Process the QIF payee/memo mapping.
              (if (not (list? qif-memo-list))
                  (set! qif-memo-hash (make-hash-table 20))
                  (set! qif-memo-hash (qif-import:read-map qif-memo-list
                                                           saved-sep)))

              ;; Process the QIF security mapping.
              (if (not (list? qif-stock-list))
                  (set! qif-stock-hash (make-hash-table 20))
                  (set! qif-stock-hash (qif-import:read-commodities
                                          qif-stock-list)))

              ;; Put all the mappings together in a list.
              (set! results (list qif-account-hash
                                  qif-cat-hash
                                  qif-memo-hash
                                  qif-stock-hash)))))

        ;; Otherwise, we can't get any saved mappings. Use empty tables.
        (set! results (list (make-hash-table 20)
                            (make-hash-table 20)
                            (make-hash-table 20)
                            (make-hash-table 20))))

    ;; Build the list of all known account names.
    (let* ((all-accounts (gnc-get-current-root-account))
           (all-account-info (extract-all-account-info all-accounts #f)))
      (set! results (cons all-account-info results)))

    results))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:write-map
;;
;;  Writes out a mapping hash table, setting the number of
;;  transactions to 0 along the way to prevent the creation
;;  of bogus accounts if you have funny stuff in your map.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:write-map hashtab)
  (let ((table '()))
    (hash-fold
     (lambda (key value p)
       (set! table (cons (cons key (simple-obj-to-list value)) table))
       #f) #f hashtab)
    (write table)))

(define (qif-import:read-map tablist tab-sep)
  (let* ((table (make-hash-table 20))
         (sep (string-ref (gnc-get-account-separator-string) 0))
         (changed-sep? (and (char? tab-sep) (not (char=? tab-sep sep)))))

    (for-each
     (lambda (entry)
       (let ((key (car entry))
             (value (simple-obj-from-list (cdr entry) <qif-map-entry>)))

         ;; If the account separator has changed, fix the account name.
         (if changed-sep?
           (let ((acct-name (qif-map-entry:gnc-name value)))
             (if (string? acct-name)
                 (qif-map-entry:set-gnc-name! value
                   (string-map (lambda (c) (if (char=? c tab-sep) sep c))
                               acct-name)))))

         (qif-map-entry:set-display?! value #f)
         (hash-set! table key value)))
     tablist)
    table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:read-commodities
;;
;;  This procedure examines a list of previously seen commodities
;;  and returns a hash table of them, if they still exist.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:read-commodities commlist)
  (let ((table (make-hash-table 20)))
    (for-each
     (lambda (entry)
       (if (and (list? entry)
                (= 3 (length entry)))
           ;; The saved information about each commodity is a
           ;; list of three items: name, namespace, and mnemonic.
           ;; Example: ("McDonald's" "NYSE" "MCD")
           (let ((commodity (gnc-commodity-table-lookup
                              (gnc-commodity-table-get-table
                                (gnc-get-current-book))
                              (cadr entry)
                              (caddr entry))))
             (if (and commodity (not (null? commodity)))
                 ;; The commodity is defined in GnuCash.
                 (hash-set! table (car entry) commodity)))))
     commlist)
    table))

(define (qif-import:write-commodities hashtab)
  (let ((table '()))
    (hash-fold
     (lambda (key value p)
       ;;FIXME: we used to type-check the values, like:
       ;; (gw:wcp-is-of-type? <gnc:commodity*> value)
       (if (and value #t)
           (set! table
                 (cons (list key
                             (gnc-commodity-get-namespace value)
                             (gnc-commodity-get-mnemonic value))
                       table))
           (display "write-commodities: something funny in hash table.\n"))
       #f) #f hashtab)
    (write table)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:save-map-prefs
;;
;;  This procedure saves all the user's mapping preferences to a
;;  file.  This only gets called when the user clicks the Apply
;;  button in the druid, so any new mappings will be lost if the
;;  user cancels the import instead.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:save-map-prefs acct-map cat-map memo-map stock-map)
  (let* ((pref-filename (gnc-build-dotgnucash-path "qif-accounts-map")))
    ;; does the file exist? if not, create it; in either case,
    ;; make sure it's a directory and we have write and execute
    ;; permission.
        (with-output-to-file pref-filename
          (lambda ()
            (display ";;; qif-accounts-map\n")
            (display ";;; automatically generated by GNUcash.  DO NOT EDIT\n")
            (display ";;; (unless you really, really want to).\n")

            (display ";;; map from QIF accounts to GNC accounts") (newline)
            (qif-import:write-map acct-map)
            (newline)

            (display ";;; map from QIF categories to GNC accounts") (newline)
            (qif-import:write-map cat-map)
            (newline)

            (display ";;; map from QIF payee/memo to GNC accounts") (newline)
            (qif-import:write-map memo-map)
            (newline)

            (display ";;; map from QIF stock name to GNC commodity") (newline)
            (qif-import:write-commodities stock-map)
            (newline)

            (display ";;; GnuCash separator used in these mappings") (newline)
            (write (string-ref (gnc-get-account-separator-string) 0))
            (newline)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  here's where we do all the guessing.  We really want to find the
;;  match in the hash table, but failing that we guess intelligently
;;  and then (failing that) not so intelligently. called in the
;;  dialog routines to rebuild the category and account map pages.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  guess-acct
;;
;;  find an existing gnc acct of the right type and name, or
;;  specify a type and name for a new one.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:guess-acct acct-name allowed-types gnc-acct-info)
  ;; see if there's a saved mapping in the hash table or an
  ;; existing gnucash account with a name that could reasonably
  ;; be said to be the same name (i.e. ABC Bank == abc bank)
  (let* ((mapped-gnc-acct
          (qif-import:find-similar-acct acct-name allowed-types
                                        gnc-acct-info))
         (retval (make-qif-map-entry)))

    ;; set fields needed for any return value
    (qif-map-entry:set-qif-name! retval acct-name)
    (qif-map-entry:set-allowed-types! retval allowed-types)

    (if mapped-gnc-acct
        ;; ok, we've found an existing account that
        ;; seems to work OK name-wise.
        (begin
          (qif-map-entry:set-gnc-name! retval (car mapped-gnc-acct))
          (qif-map-entry:set-allowed-types! retval
                                            (cadr mapped-gnc-acct))
          (qif-map-entry:set-new-acct?! retval #f))
        ;; we haven't found a match, so by default just create a new
        ;; one.  Try to put the new account in a similar place in
        ;; the hierarchy if there is one.
        (let ((new-acct-info
               (qif-import:find-new-acct acct-name allowed-types
                                         gnc-acct-info)))
          (qif-map-entry:set-gnc-name! retval (car new-acct-info))
          (qif-map-entry:set-allowed-types! retval (cadr new-acct-info))
          (qif-map-entry:set-new-acct?! retval #t)))
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:find-similar-acct
;;
;;  guess a translation from QIF info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:find-similar-acct qif-acct-name allowed-types
                                      gnc-acct-info)
  (let* ((same-type-accts '())
         (matching-name-accts '())
         (retval #f))
    (for-each
     (lambda (gnc-acct)
       ;; check against allowed-types
       (let ((acct-matches? #f))
         (for-each
          (lambda (type)
            (if (= type (xaccAccountGetType (caddr gnc-acct)))
                (set! acct-matches? #t)))
          allowed-types)
         (if acct-matches?
             (set! same-type-accts (cons gnc-acct same-type-accts)))))
     gnc-acct-info)

    ;; now find one in the same-type-list with a similar name.
    (for-each
     (lambda (gnc-acct)
       (if (qif-import:possibly-matching-name?
            qif-acct-name gnc-acct)
           (set! matching-name-accts
                 (cons gnc-acct matching-name-accts))))
     same-type-accts)

    ;; now we have either nothing, something, or too much :)
    ;; return the full-name of the first name-matching account
    (if (not (null? matching-name-accts))
        (set! retval (list
                      (cadr (car matching-name-accts))
                      (list (xaccAccountGetType
                             (caddr (car matching-name-accts))))))
        #f)
    retval))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:possibly-matching-name?
;;
;;  try various normalizations and permutations of the names
;;  to see if they could be the same.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:possibly-matching-name? qif-acct-name gnc-acct)
  (or
   ;; the QIF acct is the same name as the short name of the
   ;; gnc acct [ignoring case] (likely)
   (string=? (string-downcase  qif-acct-name)
             (string-downcase (car gnc-acct)))

   ;; the QIF acct is the same name as the long name of the
   ;; gnc acct [ignoring case] (not so likely)
   (string=? (string-downcase qif-acct-name)
             (string-downcase (cadr gnc-acct)))

   ;; the QIF name is a substring of the gnc full name.
   ;; this happens if you have the same tree but a different
   ;; top-level structure. (i.e. expenses:tax vs. QIF tax)
   (and (> (string-length qif-acct-name) 0)
        (string-contains (string-downcase (cadr gnc-acct))
                         (string-downcase qif-acct-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:find-new-acct
;;
;;  Come up with a logical name for a new account based on
;;  the Quicken name and type of the account
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:find-new-acct qif-acct allowed-types gnc-acct-info)
  (cond ((and (string? qif-acct)
              (string=? qif-acct (default-equity-account)))
         (let ((existing-equity
                (qif-import:find-similar-acct (default-equity-account)
                                              (list GNC-EQUITY-TYPE)
                                              gnc-acct-info)))
           (if existing-equity
               existing-equity
               (list (default-equity-account) (list GNC-EQUITY-TYPE)))))
        ((and (string? qif-acct)
              (not (string=? qif-acct "")))
         (list qif-acct allowed-types))
        (#t
         (list (default-unspec-acct) allowed-types))))

