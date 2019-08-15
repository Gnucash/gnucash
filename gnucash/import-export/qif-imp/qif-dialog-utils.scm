;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-dialog-utils.scm
;;;  build qif->gnc account maps and put them in a displayable
;;;  form.
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

(use-modules (gnucash string))

(define (default-stock-acct brokerage security)
  (string-append brokerage (gnc-get-account-separator-string) security))

(define (default-dividend-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Dividends") (gnc-get-account-separator-string)
                 brokerage (gnc-get-account-separator-string)
                 security))

(define (default-interest-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Interest") (gnc-get-account-separator-string)
                 brokerage
                 (if (string=? security "")
                  ""
                  (string-append (gnc-get-account-separator-string)
                                  security))))

(define (default-capital-return-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Cap Return") (gnc-get-account-separator-string)
                 brokerage (gnc-get-account-separator-string)
                 security))

(define (default-cglong-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Cap. gain (long)") (gnc-get-account-separator-string)
                 brokerage (gnc-get-account-separator-string)
                 security))

(define (default-cgmid-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Cap. gain (mid)") (gnc-get-account-separator-string)
                 brokerage (gnc-get-account-separator-string)
                 security))

(define (default-cgshort-acct brokerage security)
  (string-append (_ "Income") (gnc-get-account-separator-string)
                 (_ "Cap. gain (short)") (gnc-get-account-separator-string)
                 brokerage (gnc-get-account-separator-string)
                 security))

(define (default-equity-holding security)
  (string-append (_ "Equity") (gnc-get-account-separator-string)
                 (_ "Retained Earnings")))

(define (default-equity-account)
  (string-append (_ "Equity") (gnc-get-account-separator-string)
                 (_ "Retained Earnings")))

(define (default-commission-acct brokerage)
  (string-append (_ "Expenses") (gnc-get-account-separator-string)
                 (_ "Commissions") (gnc-get-account-separator-string)
                 brokerage))

(define (default-margin-interest-acct brokerage)
  (string-append (_ "Expenses") (gnc-get-account-separator-string)
                 (_ "Margin Interest") (gnc-get-account-separator-string)
                 brokerage))

(define (default-unspec-acct)
  (_ "Unspecified"))

;; The following investment actions implicitly specify
;; the two accounts involved in the transaction.
(define qif-import:actions-implicit
  (list 'buy 'cglong 'cgmid 'cgshort 'div 'intinc 'margint 'reinvdiv
        'reinvint 'reinvlg 'reinvmd 'reinvsg 'reinvsh 'reminder
        'rtrncap 'sell 'shrsin 'shrsout 'stksplit))


(define (qif-import:gnc-account-exists map-entry acct-list)
  (let ((retval #f))
    (for-each
     (lambda (acct)
       (if (string=? (qif-map-entry:gnc-name map-entry)
                     (cadr acct))
           (set! retval #t)))
     acct-list)
    retval))

;; the account-display is a 3-columned list of accounts in the QIF
;; import dialog (the "Account" page of the notebook).  Column 1 is
;; the account name in the QIF file, column 2 is the number of QIF
;; xtns with that account name, and column 3 is the guess for the
;; translation.  Sorted on # transactions, then alpha.

(define (qif-dialog:make-account-display qif-files acct-hash gnc-acct-info)
  ;; first, clear the "display" flags in the acct-hash and set up the
  ;; new-file? flags.  If there's nothing to show any more, don't.
  (hash-fold
   (lambda (k v p)
     (qif-map-entry:set-display?! v #f)
     #f)
   #f acct-hash)

  (let ((retval '()))
    ;; we want to make two passes here.  The first pass picks the
    ;; explicit Account descriptions out of each file.  These are the
    ;; best sources of info because we will have types and so on for
    ;; them.  The second pass picks out account-style L fields and
    ;; investment security names from the transactions.  Hopefully
    ;; we'll have most of the accounts already located by that point.
    ;; Otherwise, we have to guess them.

    ;; acct-hash hashes the qif name to a <qif-map-entry> object.
    ;; guess-acct returns one.
    (for-each
     (lambda (file)
       ;; first, get the explicit account references.
       (for-each
        (lambda (acct)
          (let ((entry (hash-ref acct-hash (qif-acct:name acct))))
            (if (not entry)
                (set! entry
                      (qif-import:guess-acct (qif-acct:name acct)
                                             (qif-acct:type acct)
                                             gnc-acct-info)))
            (qif-map-entry:set-description! entry (qif-acct:description acct))
            (hash-set! acct-hash (qif-acct:name acct) entry)))
        (qif-file:accounts file)))
     qif-files)

    ;; now make the second pass through the files, looking at the
    ;; transactions.  Hopefully the accounts are all there already.
    ;; stock accounts can have both a category/account and another
    ;; account ref from the security name.
    (for-each
     (lambda (file)
       (for-each
        (lambda (xtn)
          (let ((stock-acct (qif-xtn:security-name xtn))
                (action (qif-xtn:action xtn))
                (from-acct (qif-xtn:from-acct xtn))
                (qif-account #f)
                (qif-account-types #f)
                (entry #f))

            (if (and stock-acct action)
                ;; stock transactions are weird.  there can be several
                ;; accounts associated with stock xtns: the security,
                ;; the brokerage, a dividend account, a long-term CG
                ;; account, a short-term CG account, an interest
                ;; account.  Make sure all of the right ones get stuck
                ;; in the map.
                (begin
                  ;; first: figure out what the near-end account is.
                  ;; it's generally the security account, but could be
                  ;; an interest, dividend, or CG account.
                  (case action
                    ((buy buyx sell sellx reinvint reinvdiv reinvsh reinvsg
                          reinvlg reinvmd shrsin shrsout stksplit)
                     (set! qif-account
                           (default-stock-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-STOCK-TYPE
                                                   GNC-MUTUAL-TYPE)))
                    ((div cgshort cgmid cglong intinc miscinc miscexp
                          margint rtrncap xin xout)
                     (set! qif-account from-acct)
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE)))

                    ((divx cgshortx cgmidx cglongx intincx margintx rtrncapx)
                     (set! qif-account
                           (and (qif-split:category-is-account?
                                  (car (qif-xtn:splits xtn)))
                                (qif-split:category
                                  (car (qif-xtn:splits xtn)))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE)))
                    ((miscincx miscexpx)
                     (set! qif-account
                           (qif-split:miscx-category
                            (car (qif-xtn:splits xtn))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE))))

                  ;; now reference the near-end account
                  (if qif-account
                      (begin
                        (set! entry (hash-ref acct-hash qif-account))
                        (if (not entry)
                            (set! entry
                                  (qif-import:guess-acct qif-account
                                                         qif-account-types
                                                         gnc-acct-info)))
                        (qif-map-entry:set-display?! entry #t)
                        (hash-set! acct-hash qif-account entry)))

                  ;; now figure out the other end of the transaction.
                  ;; the far end will be the brokerage for buy, sell,
                  ;; etc, or the "L"-referenced account for buyx,
                  ;; sellx, etc, or an equity account for ShrsIn/ShrsOut

                  ;; miscintx and miscexpx are very, very "special"
                  ;; cases ... I'm not sure this is right.
                  ;; the L line looks like :
                  ;; LCategory/class [Account]/class
                  ;; so I assume near-acct is Account and far acct
                  ;; is Category.  This matches the intincx/divx
                  ;; behavior.

                  (set! qif-account #f)
                  (case action
                    ((buy sell)
                     (set! qif-account from-acct)
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE)))
                    ((buyx sellx xin xout)
                     (set! qif-account
                           (and (qif-split:category-is-account?
                                  (car (qif-xtn:splits xtn)))
                                (qif-split:category
                                  (car (qif-xtn:splits xtn)))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE)))

                    ((stksplit)
                     (set! qif-account
                           (default-stock-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-STOCK-TYPE
                                                   GNC-MUTUAL-TYPE
                                                   GNC-ASSET-TYPE
                                                   GNC-LIABILITY-TYPE
                                                   GNC-RECEIVABLE-TYPE
                                                   GNC-PAYABLE-TYPE)))
                    ((cgshort cgshortx reinvsg reinvsh)
                     (set! qif-account
                           (default-cgshort-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((cgmid cgmidx reinvmd)
                     (set! qif-account
                           (default-cgmid-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((cglong cglongx reinvlg)
                     (set! qif-account
                           (default-cglong-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((intinc intincx reinvint)
                     (set! qif-account
                           (default-interest-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((div divx reinvdiv)
                     (set! qif-account
                           (default-dividend-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((rtrncap rtrncapx)
                     (set! qif-account
                           (default-capital-return-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-INCOME-TYPE)))

                    ((shrsin shrsout)
                     (set! qif-account
                           (default-equity-holding stock-acct))
                     (set! qif-account-types (list GNC-EQUITY-TYPE)))

                    ((margint margintx)
                     (set! qif-account
                           (default-margin-interest-acct from-acct))
                     (set! qif-account-types (list GNC-EXPENSE-TYPE)))

                    ((miscinc miscexp miscincx miscexpx)
                     ;; these reference a category on the other end
                     (set! qif-account #f)))

                  ;; now reference the far-end account
                  (if qif-account
                      (begin
                        (set! entry (hash-ref acct-hash qif-account))
                        (if (not entry)
                            (set! entry (qif-import:guess-acct
                                         qif-account qif-account-types
                                         gnc-acct-info)))
                        (qif-map-entry:set-display?! entry #t)
                        (hash-set! acct-hash qif-account entry)))

                  ;; if there's a commission, reference the
                  ;; commission account
                  (if (qif-xtn:commission xtn)
                      (begin
                        (set! qif-account
                              (default-commission-acct from-acct))
                        (set! entry
                              (hash-ref acct-hash qif-account))
                        (if (not entry)
                            (set! entry
                                  (qif-import:guess-acct
                                   qif-account
                                   (list GNC-EXPENSE-TYPE)
                                   gnc-acct-info)))
                        (qif-map-entry:set-display?! entry #t)
                          (hash-set! acct-hash qif-account entry))))

                ;; non-stock transactions.  these are a bit easier.
                ;; the near-end account (from) is always in the
                ;; transaction, and the far end(s) are in the splits.
                (begin
                  (set! entry (hash-ref acct-hash from-acct))
                  (if (not entry)
                      (set! entry (qif-import:guess-acct
                                   from-acct
                                   (list GNC-BANK-TYPE
                                         GNC-CCARD-TYPE
                                         GNC-CASH-TYPE
                                         GNC-ASSET-TYPE
                                         GNC-LIABILITY-TYPE
                                         GNC-RECEIVABLE-TYPE
                                         GNC-PAYABLE-TYPE)
                                   gnc-acct-info)))
                  (qif-map-entry:set-display?! entry #t)
                  (hash-set! acct-hash from-acct entry)

                  ;; iterate over the splits doing the same thing.
                  (for-each
                   (lambda (split)
                     (let ((xtn-is-acct (qif-split:category-is-account? split))
                           (xtn-acct #f)
                           (entry #f))
                       (if xtn-is-acct
                           (begin
                             (set! xtn-acct (qif-split:category split))
                             (set! entry (hash-ref acct-hash xtn-acct))
                             (if (not entry)
                                 (set! entry
                                       (qif-import:guess-acct
                                        xtn-acct
                                        (list GNC-BANK-TYPE
                                              GNC-CCARD-TYPE
                                              GNC-CASH-TYPE
                                              GNC-ASSET-TYPE
                                              GNC-LIABILITY-TYPE
                                              GNC-RECEIVABLE-TYPE
                                              GNC-PAYABLE-TYPE)
                                        gnc-acct-info)))
                             (qif-map-entry:set-display?! entry #t)
                             (hash-set! acct-hash xtn-acct entry)))))
                   (qif-xtn:splits xtn))))))
        (qif-file:xtns file)))
     qif-files)

    ;; now that the hash table is filled, make the display list
    (hash-fold
     (lambda (k v p)
       (if (qif-map-entry:display? v)
           (begin
             (qif-map-entry:set-new-acct?!
              v (not (qif-import:gnc-account-exists v gnc-acct-info)))
             (set! retval (cons v retval))))
       #f)
     #f acct-hash)

    ;; sort by number of transactions with that account so the
    ;; most important are at the top
    (set! retval
          (sort retval
                (lambda (a b)
                  (string<? (qif-map-entry:qif-name a)
                            (qif-map-entry:qif-name b)))))
    retval))


;; the category display is similar to the Account display.
;; QIF category name, xtn count, then GNUcash account.

(define (qif-dialog:make-category-display qif-files cat-hash gnc-acct-info)
  ;; first, clear the "display" flags in the cat-hash.  If there's
  ;; nothing to show any more, don't.
  (hash-fold
   (lambda (k v p)
     (qif-map-entry:set-display?! v #f)
     #f)
   #f cat-hash)

  (let ((retval '())
        (entry #f))
    ;; get the Cat entries from each file
    (for-each
     (lambda (file)
       (for-each
        (lambda (cat)
          (set! entry (hash-ref cat-hash (qif-cat:name cat)))
          (if (not entry)
              (set! entry
                    (qif-import:guess-acct (qif-cat:name cat)
                                           (if (qif-cat:expense-cat cat)
                                               (list GNC-EXPENSE-TYPE
                                                     GNC-INCOME-TYPE)
                                               (list GNC-INCOME-TYPE
                                                     GNC-EXPENSE-TYPE))
                                           gnc-acct-info)))
          (qif-map-entry:set-description!
           entry (qif-cat:description cat))
          (hash-set! cat-hash (qif-cat:name cat) entry))
        (qif-file:cats file)))
     qif-files)

    ;; now look at every transaction and increment the count
    ;; in the account slot if the string matches, or make a
    ;; new hash reference if not.
    (for-each
     (lambda (qif-file)
       (for-each
        (lambda (xtn)
          (let ((action (qif-xtn:action xtn)))
            ;; Many types of investment transactions implicitly use the
            ;; brokerage account or a known offshoot. There is no need
            ;; to consider a category mapping for these.
            (if (not (and action
                          (memv action qif-import:actions-implicit)))
                ;; iterate over the splits
                (for-each
                 (lambda (split)
                   (let ((xtn-is-acct (qif-split:category-is-account? split))
                         (xtn-cat #f)
                         (entry #f))
                     (if (not xtn-is-acct)
                         (begin
                           (set! xtn-cat (qif-split:category split))
                           (set! entry (hash-ref cat-hash xtn-cat))
                           ;; NOTE: It would be more robust and efficient if the
                           ;; three "make display" routines below were combined:
                           ;;   make-account-display
                           ;;   make-category-display
                           ;;   make-memo-display
                           ;;
                           ;; This would also require adjusting several callback
                           ;; functions that reference these procedures from C.
                           ;;
                           ;; Until then, the maintainer of this code must make
                           ;; sure that the logic used in the "if" below matches
                           ;; the criteria for making memo/payee mappings (seen
                           ;; in make-memo-display).

                           ;; Add an entry if there isn't one already and either
                           ;;  (a) the category is non-blank, or
                           ;;  (b) no memo/payee mapping can be applied
                           (if (and (not entry)
                                    (or (not (and (string? xtn-cat)
                                                  (string=? xtn-cat "")))
                                        (and (or (not (qif-split:memo split))
                                                 (equal? (qif-split:memo split) ""))
                                             (or (> (length (qif-xtn:splits xtn)) 1)
                                                 (not (qif-xtn:payee xtn))
                                                 (equal? (qif-xtn:payee xtn) "")))))
                               (set! entry
                                     (qif-import:guess-acct
                                      xtn-cat
                                      (if (gnc-numeric-positive-p
                                           (qif-split:amount split))
                                          (list GNC-INCOME-TYPE
                                                GNC-EXPENSE-TYPE)
                                          (list GNC-EXPENSE-TYPE
                                                GNC-INCOME-TYPE))
                                      gnc-acct-info)))
                           (if entry
                               (begin
                                 (qif-map-entry:set-display?! entry #t)
                                 (hash-set! cat-hash xtn-cat entry)))))))
                 (qif-xtn:splits xtn)))))
        (qif-file:xtns qif-file)))
     qif-files)

    ;; now that the hash table is filled, make the display list
    (hash-fold
     (lambda (k v p)
       (if (qif-map-entry:display? v)
           (begin
             (qif-map-entry:set-new-acct?!
              v (not (qif-import:gnc-account-exists v gnc-acct-info)))
             (set! retval (cons v retval))))
       #f)
     #f cat-hash)

    ;; sort by qif account name
    (set! retval (sort retval
                       (lambda (a b)
                         (string<? (qif-map-entry:qif-name a)
                                   (qif-map-entry:qif-name b)))))
    retval))

;; this one's like the other display builders, it just looks at the
;; payee and memo too.

(define (qif-dialog:make-memo-display qif-files memo-hash gnc-acct-info)
  (let ((retval '()))
    ;; clear the display flags for existing items
    (hash-fold
     (lambda (k v p)
       (qif-map-entry:set-display?! v #f)
       #f)
     #f memo-hash)

    ;; iterate over every imported transaction.  If there's no
    ;; category in the transaction, look at the payee to get a clue.
    ;; of there's no payee, look at the split memo.
    (for-each
     (lambda (file)
       (for-each
        (lambda (xtn)
          (let ((payee (qif-xtn:payee xtn))
                (action (qif-xtn:action xtn))
                (splits (qif-xtn:splits xtn)))
            ;; Many types of investment transactions implicitly use the
            ;; brokerage account or a known offshoot. There is no need
            ;; to consider a memo/payee mapping for these.
            (if (not (and action
                          (memv action qif-import:actions-implicit)))
                (for-each
                 (lambda (split)
                   (let ((cat (qif-split:category split))
                         (memo (qif-split:memo split))
                         (key-string #f))
                     ;; for each split: if there's a category, do nothing.
                     ;; if there's a payee, use that as the
                     ;; key otherwise, use the split memo.
                     (cond ((and cat
                                 (or (not (string? cat))
                                     (not (string=? cat ""))))
                            (set! key-string #f))
                           ((and payee (= (length splits) 1))
                            (set! key-string payee))
                           (memo
                            (set! key-string memo)))

                     (if key-string
                         (let ((entry (hash-ref memo-hash key-string)))
                           (if (not entry)
                               (begin
                                 (set! entry (make-qif-map-entry))
                                 (qif-map-entry:set-qif-name! entry key-string)
                                 (if (string=? key-string "")
                                     (set! key-string (default-unspec-acct)))
                                 (qif-map-entry:set-gnc-name!
                                  entry (default-unspec-acct))
                                 (qif-map-entry:set-allowed-types!
                                  entry
                                  (if (gnc-numeric-positive-p
                                       (qif-split:amount split))
                                      (list GNC-INCOME-TYPE GNC-EXPENSE-TYPE
                                            GNC-BANK-TYPE GNC-CCARD-TYPE
                                            GNC-LIABILITY-TYPE GNC-ASSET-TYPE
                                            GNC-RECEIVABLE-TYPE GNC-PAYABLE-TYPE
                                            GNC-STOCK-TYPE GNC-MUTUAL-TYPE)
                                      (list GNC-EXPENSE-TYPE GNC-INCOME-TYPE
                                            GNC-BANK-TYPE GNC-CCARD-TYPE
                                            GNC-LIABILITY-TYPE GNC-ASSET-TYPE
                                            GNC-RECEIVABLE-TYPE GNC-PAYABLE-TYPE
                                            GNC-STOCK-TYPE GNC-MUTUAL-TYPE)))))
                           (qif-map-entry:set-display?! entry #t)
                           (hash-set! memo-hash key-string entry)))))
                 splits))))
        (qif-file:xtns file)))
     qif-files)

    ;; build display list
    (hash-fold
     (lambda (k v p)
       (if (qif-map-entry:display? v)
           (begin
             (qif-map-entry:set-new-acct?!
              v (not (qif-import:gnc-account-exists v gnc-acct-info)))
             (set! retval (cons v retval))))
       #f)
     #f memo-hash)

    ;; sort by qif memo/payee name
    (set! retval (sort retval
                       (lambda (a b)
                         (string<? (qif-map-entry:qif-name a)
                                   (qif-map-entry:qif-name b)))))
    retval))


(define (qif-dialog:qif-file-loaded? filename list-of-files)
  (let ((status (map
                 (lambda (file)
                   (string=? filename (qif-file:path file)))
                 list-of-files)))
    (if (memq #t status)
        #t
        #f)))

(define (qif-dialog:unload-qif-file oldfile list-of-files)
  (delq oldfile list-of-files))

(define (qif-import:any-new-accts? hash-table)
  (let ((retval #f))
    (for-each
     (lambda (bin)
       (for-each
        (lambda (elt)
          (if (and (qif-map-entry:new-acct? (cdr elt))
                   (qif-map-entry:display? (cdr elt)))
              (set! retval #t)))
        bin))
     (vector->list hash-table))
    retval))

(define (qif-import:fix-from-acct qif-file new-acct-name)
  (for-each
   (lambda (xtn)
     (if (not (qif-xtn:from-acct xtn))
         (qif-xtn:set-from-acct! xtn new-acct-name)))
   (qif-file:xtns qif-file)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:get-account-name
;;
;;  Given an account name, return the rightmost subaccount.
;;  For example, given the account name "foo:bar", "bar" is
;;  returned (assuming the account separator is ":").
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:get-account-name fullname)
  (let* ((sep (gnc-get-account-separator-string))
         (last-sep (gnc:string-rcontains fullname sep)))
    (if last-sep
        (substring fullname (+ last-sep (string-length sep)))
        fullname)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-dialog:default-namespace
;;
;;  Given a security's QIF symbol and type, along with all
;;  previously seen security mapping preferences, return a
;;  default namespace.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (qif-dialog:default-namespace qif-symbol qif-type prefs)

  ;; Guess a namespace based on the symbol alone.
  (define (guess-by-symbol s)
    (if (string? s)
      (let ((l (string-length s))
            (d (string-index s #\.))
            (pref-match
              (if (list? prefs)
                  (find (lambda (elt)
                          ;; Does the symbol match, and is the namespace
                          ;; compatible with the QIF type?
                          (and (string=? s (caddr elt))
                               (not (and (string? qif-type)
                                         (string=? GNC_COMMODITY_NS_MUTUAL
                                                   (cadr elt))
                                         (or (string-ci=? qif-type "stock")
                                             (string-ci=? qif-type "etf"))))))
                        prefs)
                   #f)))
        (cond
          ;; If a preferences match was found, use its namespace.
          (pref-match
           (cadr pref-match))

          ;; Guess NYSE for symbols of 1-3 characters.
          ((< l 4)
           GNC_COMMODITY_NS_NYSE)

          ;; Guess NYSE for symbols of 1-3 characters
          ;; followed by a dot and 1-2 characters.
          ((and d
                (< l 7)
                (< 0 d 4)
                (<= 2 (- l d) 3))
           GNC_COMMODITY_NS_NYSE)

          ;; Guess NASDAQ for symbols of 4 characters.
          ((= l 4)
           GNC_COMMODITY_NS_NASDAQ)

          ;; Otherwise it's probably a fund.
          (else
           GNC_COMMODITY_NS_MUTUAL)))
      ;; There's no symbol. Default to a fund.
      GNC_COMMODITY_NS_MUTUAL))

  ;; Was a QIF type given?
  (if (string? qif-type)
     ;; Yes. We might be able to definitely determine the namespace.
     (cond
       ;; Mutual fund
       ((string-ci=? qif-type "mutual fund")
        GNC_COMMODITY_NS_MUTUAL)

       ;; Index
       ((string-ci=? qif-type "index")
        ;; This QIF type must be wrong; indexes aren't tradable!
        GNC_COMMODITY_NS_MUTUAL)

       (else
        (guess-by-symbol qif-symbol)))

     ;; No QIF type was given, so guess a
     ;; default namespace by symbol alone.
     (guess-by-symbol qif-symbol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:update-security-hash
;;
;;  For each QIF security in acct-hash, find a matching
;;  GnuCash security or create a new one, then add it to the
;;  security-hash table. Return a list of security-hash keys
;;  for all newly created GnuCash securities, or #f if none.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:update-security-hash security-hash ticker-map
                                         acct-hash sec-prefs)
  (let ((names '()))
    (hash-fold
     (lambda (qif-name map-entry p)
       (let ((security-name (qif-import:get-account-name qif-name)))
         ;; Is this account going to be imported, is it security-denominated,
         ;; and is the security not already in the security hash table?
         (if (and
              security-name
              (qif-map-entry:display? map-entry)
              (or (memv GNC-STOCK-TYPE
                        (qif-map-entry:allowed-types map-entry))
                  (memv GNC-MUTUAL-TYPE
                        (qif-map-entry:allowed-types map-entry)))
              (not (hash-ref security-hash security-name)))
             (let ((existing-gnc-acct
                     (gnc-account-lookup-by-full-name
                      (gnc-get-current-root-account)
                      (qif-map-entry:gnc-name map-entry)))
                   (book (gnc-account-get-book (gnc-get-current-root-account))))
               ;; Are we importing to an existing, security-denominated account?
               (if (and (not (null? existing-gnc-acct))
                        (memv (xaccAccountGetType existing-gnc-acct)
                              (list GNC-STOCK-TYPE GNC-MUTUAL-TYPE)))
                   ;; Yes, so that security is the one to use. Add it
                   ;; to the security hash table.
                   (let ((commodity
                          (xaccAccountGetCommodity existing-gnc-acct)))
                     (hash-set! security-hash security-name commodity))

                   ;; Otherwise, since we can't definitively match this QIF
                   ;; security to a GnuCash security, create a new one with
                   ;; some (hopefully) intelligent defaults.
                   (let* ((qif-symbol
                            (qif-ticker-map:lookup-symbol ticker-map
                                                          security-name))
                          (qif-type
                            (qif-ticker-map:lookup-type ticker-map
                                                        security-name))
                          (namespace (qif-dialog:default-namespace qif-symbol
                                                                   qif-type
                                                                   sec-prefs)))

                     ;; If no symbol has been provided, default to the name.
                     (if (not qif-symbol)
                         (set! qif-symbol security-name))

                     ;; Create the new security and add it to the hash table.
                     (hash-set! security-hash
                                security-name
                                (gnc-commodity-new book
                                                   security-name
                                                   namespace
                                                   qif-symbol
                                                   ""
                                                   100000))

                     ;; Add the hash key to the list to be returned.
                     (set! names (cons security-name names))))))
         #f))
     #f acct-hash)

    (if (not (null? names))
        (sort names string<?)
        #f)))

;; this is used within the dialog to get a list of all the new
;; accounts the importer thinks it's going to make.  Passed to the
;; account picker.
;;
;; returned is a tree-structured list of all the old and new accounts
;; like so : (name new? children).  trees are sorted alphabetically.
;; This should probably change but it's beeter than no sort at all.

(define (qif-import:get-all-accts extra-maps)
  (define (cvt-to-tree path new?)
    (if (null? path)
        '()
        (list (car path) new?
              (if (null? (cdr path)) '()
                  (list (cvt-to-tree (cdr path) new?))))))

  (define (merge-into-tree tree path new?)
    (if (null? path)
        tree
        (if (null? tree)
            (list (cvt-to-tree path new?))
            (let ((newtree '()))
              (let loop ((tree-elt (car tree))
                         (tree-left (cdr tree)))
                (if (string=? (car path) (car tree-elt))
                    (let ((old-children (caddr tree-elt)))
                      (set! newtree
                            (cons (list (car path)
                                        (and new? (cadr tree-elt))
                                        (merge-into-tree
                                         old-children (cdr path) new?))
                                  (append newtree tree-left))))
                    (begin
                      (set! newtree (cons tree-elt newtree))
                      (if (not (null? tree-left))
                          (loop (car tree-left) (cdr tree-left))
                          (set! newtree (cons (cvt-to-tree path new?)
                                              newtree))))))
              (sort newtree (lambda (a b) (string<? (car a) (car b))))))))


  (let ((accts '())
        (acct-tree '())
        (sep (gnc-get-account-separator-string)))

    ;; get the new accounts from the account map
    (for-each
     (lambda (acctmap)
       (if acctmap
           (hash-fold
            (lambda (k v p)
              (if (qif-map-entry:display? v)
                  (set! accts
                        (cons
                         (cons (gnc:substring-split (qif-map-entry:gnc-name v)
                                                    sep)
                               (qif-map-entry:new-acct? v))
                         accts)))
              #f)
            #f acctmap)))
     extra-maps)

    ;; get the old accounts from the current account group
    (for-each
     (lambda (acct)
       (set! accts
             (cons
              (cons (gnc:substring-split (gnc-account-get-full-name acct) sep)
                    #f)
              accts)))
     (gnc-account-get-descendants-sorted (gnc-get-current-root-account)))

    ;; now build a tree structure
    (for-each
     (lambda (acct)
       (set! acct-tree
             (merge-into-tree acct-tree (car acct) (cdr acct))))
     accts)

    ;; we're done
    acct-tree))

(define (qif-import:refresh-match-selection matches item)
  (if (> item -1)
      (let ((i 0))
        (for-each
         (lambda (match)
           (if (= i item)
               (if (cdr match)
                   (set-cdr! match #f)
                   (set-cdr! match #t))
               (set-cdr! match #f))
           (set! i (+ 1 i)))
         matches))))
