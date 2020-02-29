;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-guess-map.scm
;;;  guess (or load from prefs) mappings from QIF cats/accts to gnc
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-modules (srfi srfi-13))
(use-modules (ice-9 match))

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
(define GNC-RECEIVABLE-TYPE 11)
(define GNC-PAYABLE-TYPE 12)

(define (record-fields->list record)
  (let ((type (record-type-descriptor record)))
    (map
     (lambda (field) ((record-accessor type field) record))
     (record-type-fields type))))

(define (list->record-fields lst type)
  (apply (record-constructor type) lst))

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
;;   - a hash of QIF security name to gnc-commodity*
;;   - a list of all previously saved security mappings
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

  (let* ((pref-filename (gnc-build-userdata-path "qif-accounts-map"))
         (results '()))

    ;; Get the user's saved mappings.
    (if (access? pref-filename R_OK)
        ;; We have access to the mapping file (qif-accounts-map).
        (with-input-from-file pref-filename
          (lambda ()
            (let ((qif-account-list #f)
                  (qif-cat-list #f)
                  (qif-memo-list #f)
                  (qif-security-list #f)
                  (qif-account-hash #f)
                  (qif-cat-hash #f)
                  (qif-memo-hash #f)
                  (qif-security-hash #f)
                  (saved-sep #f))

              ;; Read the mapping file.
              (set! qif-account-list (safe-read))
              (set! qif-cat-list (safe-read))
              (set! qif-memo-list (safe-read))
              (set! qif-security-list (safe-read))
              (set! saved-sep (safe-read))

              ;; Convert the separator to a string if necessary.
              ;; It was a character prior to 2.2.6.
              (if (char? saved-sep)
                  (set! saved-sep (string saved-sep)))

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
              (if (not (list? qif-security-list))
                  (set! qif-security-hash (make-hash-table 20))
                  (set! qif-security-hash (qif-import:read-securities
                                           qif-security-list)))

              ;; Put all the mappings together in a list.
              (set! results (list qif-account-hash
                                  qif-cat-hash
                                  qif-memo-hash
                                  qif-security-hash
                                  qif-security-list)))))

        ;; Otherwise, we can't get any saved mappings. Use empty tables.
        (set! results (list (make-hash-table 20)
                            (make-hash-table 20)
                            (make-hash-table 20)
                            (make-hash-table 20)
                            '())))

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

(define (qif-import:write-map hashtab port)
  (let ((table '()))
    (hash-for-each
     (lambda (key value)
       (set! table
         (cons (cons key (record-fields->list value)) table)))
     hashtab)
    (write table port)))

(define (qif-import:read-map tablist tab-sep)
  (let* ((table (make-hash-table 20))
         (sep (gnc-get-account-separator-string))
         (changed-sep? (and (string? tab-sep) (not (string=? tab-sep sep)))))

    (for-each
     (lambda (entry)
       (let ((key (car entry))
             (value (list->record-fields (cdr entry) <qif-map-entry>)))

         ;; If the account separator has changed, fix the account name.
         (if changed-sep?
           (let ((acct-name (qif-map-entry:gnc-name value)))
             (if (string? acct-name)
                 (qif-map-entry:set-gnc-name! value
                                              (gnc:substring-replace acct-name
                                                                     tab-sep  
                                                                     sep)))))

         (qif-map-entry:set-display?! value #f)
         (hash-set! table key value)))
     tablist)
    table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:read-securities
;;
;;  This procedure examines a list of previously seen security
;;  mappings and returns a hash table pairing QIF security names
;;  with existing GnuCash commodities.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:read-securities security-list)
  (let ((table (make-hash-table))
        (comm-table (gnc-commodity-table-get-table (gnc-get-current-book))))
    (for-each
     (match-lambda
       ((name ns mnemonic)
        ;; The saved information about each security mapping is a
        ;; list of three items: the QIF name, and the GnuCash
        ;; namespace and mnemonic (symbol) to which it maps.
        ;; Example: ("McDonald's" "NYSE" "MCD")
        (let ((commodity (gnc-commodity-table-lookup comm-table ns mnemonic)))
          (if (and commodity (not (null? commodity)))
              ;; There is an existing GnuCash commodity for this
              ;; combination of namespace and symbol.
              (hash-set! table name commodity))))
       (_ #f))
     security-list)
    table))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:write-securities
;;
;;  This procedure writes a mapping QIF security names to
;;  GnuCash commodity namespaces and mnemonics (symbols).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:write-securities security-hash security-prefs port)
  (let ((table '()))
    ;; For each security that has been paired with an existing
    ;; GnuCash commodity, create a list containing the QIF name
    ;; and the commodity's namespace and mnemonic (symbol).
    (hash-for-each
     (lambda (key value)
       (set! table
         (cons (list key
                     (gnc-commodity-get-namespace value)
                     (gnc-commodity-get-mnemonic value))
               table)))
     security-hash)

    ;; Add on the rest of the saved security mapping preferences.
    (for-each
     (lambda (m)
       (if (not (hash-ref security-hash (car m)))
           (set! table (cons m table))))
     security-prefs)

    ;; Write out the mappings.
    (write table port)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:save-map-prefs
;;
;;  This procedure saves all the user's mapping preferences to a
;;  file.  This only gets called when the user clicks the Apply
;;  button in the druid, so any new mappings will be lost if the
;;  user cancels the import instead.
;;
;;  Returns #t upon success or #f on failure.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:save-map-prefs acct-map cat-map memo-map
                                   security-map security-prefs)

  ;; This procedure does all the work. We'll define it, then call it safely.
  (define (private-save)
    (call-with-output-file (gnc-build-userdata-path "qif-accounts-map")
      (lambda (port)
        (display ";;; qif-accounts-map" port)
        (newline port)
        (display ";;; Automatically generated by GnuCash. DO NOT EDIT." port)
        (newline port)
        (display ";;; (Unless you really, really want to.)" port)
        (newline port)
        (display ";;; Map QIF accounts to GnuCash accounts" port)
        (newline port)
        (qif-import:write-map acct-map port)
        (newline port)

        (display ";;; Map QIF categories to GnuCash accounts" port)
        (newline port)
        (qif-import:write-map cat-map port)
        (newline port)

        (display ";;; Map QIF payee/memo to GnuCash accounts" port)
        (newline port)
        (qif-import:write-map memo-map port)
        (newline port)

        (display ";;; Map QIF security names to GnuCash commodities" port)
        (newline port)
        (qif-import:write-securities security-map security-prefs port)
        (newline port)

        (display ";;; GnuCash separator used in these mappings" port)
        (newline port)
        (write (gnc-get-account-separator-string) port)
        (newline port)))
    #t)

  ;; Safely save the file.
  (gnc:backtrace-if-exception private-save))


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
  (cond
   ((equal? qif-acct (default-equity-account))
    (or (qif-import:find-similar-acct
         (default-equity-account) (list GNC-EQUITY-TYPE) gnc-acct-info)
        (list (default-equity-account) (list GNC-EQUITY-TYPE))))
   ((equal? qif-acct "") (list (default-unspec-acct) allowed-types))
   (else (list qif-acct allowed-types))))

