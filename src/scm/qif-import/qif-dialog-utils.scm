;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-dialog-utils.scm
;;;  build qif->gnc account maps and put them in a displayable 
;;;  form. 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-dialog-utils.scm")

(use-modules (ice-9 regex))

(define (default-stock-acct brokerage security)
  (string-append brokerage (gnc:account-separator-char) security))

(define (default-dividend-acct brokerage security)
  (string-append "Dividends" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char) 
                 security))

(define (default-interest-acct brokerage security) 
  (string-append "Interest" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char)  
                 security))

(define (default-capital-return-acct brokerage security) 
  (string-append "Cap Return" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char)  
                 security))

(define (default-cglong-acct brokerage security)
  (string-append "Cap. gain (long)" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char) 
                 security))

(define (default-cgmid-acct brokerage security)
  (string-append "Cap. gain (mid)" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char) 
                 security))

(define (default-cgshort-acct brokerage security)
  (string-append "Cap. gain (short)" (gnc:account-separator-char) 
                 brokerage (gnc:account-separator-char) 
                 security))

(define (default-equity-holding security) "Retained Earnings")

(define (default-equity-account) "Retained Earnings")  

(define (default-commission-acct brokerage) 
  (string-append "Commissions" (gnc:account-separator-char) 
                 brokerage))

(define (default-margin-interest-acct brokerage) 
  (string-append "Margin Interest" (gnc:account-separator-char) 
                 brokerage))

;; the account-display is a 3-columned list of accounts in the QIF
;; import dialog (the "Account" page of the notebook).  Column 1 is
;; the account name in the QIF file, column 2 is the number of QIF
;; xtns with that account name, and column 3 is the guess for the
;; translation.  Sorted on # transactions, then alpha.

(define (qif-dialog:make-account-display qif-files acct-hash gnc-acct-info) 
  ;; first, clear the "display" flags in the acct-hash.  If there's 
  ;; nothing to show any more, don't. 
  (for-each 
   (lambda (bin)
     (for-each 
      (lambda (elt)
        (qif-map-entry:set-display?! (cdr elt) #f))
      bin))
   (vector->list acct-hash))

  (let ((retval '()))    
    ;; we want to make two passes here.  The first pass picks the
    ;; explicit Account descriptions out of each file.  These are the
    ;; best sources of info because we will have types and so on for
    ;; them.  The second pass picks out account-style L fields and
    ;; investment security names from the transactions.  Hopefully
    ;; we'll have most of the accounts already located by that point.
    ;; Otherwise, we have to guess them.

    ;; guess-acct returns a list that's
    ;; (qif-name gnc-name gnc-type new-acct?)
    ;; acct-hash hashes QIF account name to a list that's composed of
    ;; (qif-acct-name gnc-acct-name gnc-acct-type gnc-acct-new?
    ;;  num-qif-xtns qif-object) so we can find the properties later. 
    
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
                                             (list (qif-acct:type acct))
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
                                                   GNC-MUTUAL-TYPE
                                                   GNC-ASSET-TYPE)))
                    ((div cgshort cgmid cglong intinc miscinc miscexp 
                          margint rtrncap xin xout)
                     (set! qif-account from-acct)
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE)))
                    
                    ((divx cgshortx cgmidx cglongx intincx margintx rtrncapx)
                     (set! qif-account 
                           (qif-split:category 
                            (car (qif-xtn:splits xtn))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE)))
                    ((miscincx miscexpx)
                     (set! qif-account 
                           (qif-split:miscx-category 
                            (car (qif-xtn:splits xtn))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE))))
                  
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
                                                   GNC-ASSET-TYPE)))
                    ((buyx sellx xin xout)
                     (set! qif-account 
                           (qif-split:category 
                            (car (qif-xtn:splits xtn))))
                     (set! qif-account-types (list GNC-BANK-TYPE
                                                   GNC-CCARD-TYPE
                                                   GNC-CASH-TYPE
                                                   GNC-ASSET-TYPE)))
                    
                    ((stksplit)
                     (set! qif-account 
                           (default-stock-acct from-acct stock-acct))
                     (set! qif-account-types (list GNC-STOCK-TYPE 
                                                   GNC-MUTUAL-TYPE
                                                   GNC-ASSET-TYPE)))
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
                           (default-capital-return-acct from-acct))
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
                                   (list 
                                    GNC-BANK-TYPE
                                    GNC-CCARD-TYPE
                                    GNC-CASH-TYPE
                                    GNC-ASSET-TYPE)
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
                                        (list 
                                         GNC-BANK-TYPE
                                         GNC-CCARD-TYPE
                                         GNC-CASH-TYPE
                                         GNC-ASSET-TYPE)
                                        gnc-acct-info)))
                             (qif-map-entry:set-display?! entry #t)
                             (hash-set! acct-hash xtn-acct entry)))))
                   (qif-xtn:splits xtn))))))
        (qif-file:xtns file)))
     qif-files)
    
    ;; now that the hash table is filled, make the display list 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (qif-map-entry:display? (cdr elt))
              (set! retval 
                    (cons (cdr elt) retval))))
        bin))
     (vector->list acct-hash))
    
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
  (for-each 
   (lambda (bin)
     (for-each 
      (lambda (elt)
        (qif-map-entry:set-display?! (cdr elt) #f))
      bin))
   (vector->list cat-hash))

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
                                               (list GNC-EXPENSE-TYPE)
                                               (list GNC-INCOME-TYPE))
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
                     (if (not entry)
                         (set! entry 
                               (qif-import:guess-acct
                                xtn-cat
                                (if (> (qif-split:amount split) 0)
                                    (list GNC-INCOME-TYPE)
                                    (list GNC-EXPENSE-TYPE))
                                gnc-acct-info)))
                     (qif-map-entry:set-display?! entry #t)
                     (hash-set! cat-hash xtn-cat entry)))))
           (qif-xtn:splits xtn)))
        (qif-file:xtns qif-file)))
     qif-files)
    
    ;; now that the hash table is filled, make the display list 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (qif-map-entry:display? (cdr elt))
              (set! retval (cons (cdr elt) retval))))
        bin))
     (vector->list cat-hash))
    
    ;; sort by qif account name
    (set! retval (sort retval 
                       (lambda (a b)
                         (string<? (qif-map-entry:qif-name a)
                                   (qif-map-entry:qif-name b)))))
    retval))

;; UNFINISHED (and currently not connected to anything)
;; this one's like the other display builders, it just looks at the
;; payee and memo too.  

(define (qif-dialog:make-memo-display qif-files memo-hash gnc-acct-info)
  (let ((retval '()))

    ;; clear the display flags for existing items 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (qif-map-entry:set-display?! (cdr elt) #f))
        bin))
     (vector->list memo-hash))

    ;; iterate over every imported transaction.  If there's no
    ;; category in the transaction, look at the payee to get a clue.
    ;; of there's no payee, look at the split memo.
    (for-each 
     (lambda (file) 
       (for-each 
        (lambda (xtn)
          (let ((payee (qif-xtn:payee xtn))
                (splits (qif-xtn:splits xtn)))
            (for-each 
             (lambda (split)
               (let ((cat (qif-split:category split))
                     (memo (qif-split:memo split)))
                 ;; for each split: if there's a category, do nothing.
                 ;; if there's a payee, use that as the
                 ;; key. otherwise, use the split memo.
                 (cond ((and cat 
                             (or (not (string? cat))
                                 (string=? cat "")))
                        (set! key-string #f))
                       (payee 
                        (set! key-string payee))
                       (memo
                        (set! ley-string memo)))
                 
                 (if key-string 
                     (let ((entry (hash-ref memo-hash key-string)))
                       (if (not entry)
                           (set! entry
                                 (qif-import:guess-acct 
                                  payee 
                                  (if (> (qif-split:amount split) 0)
                                      (list GNC-INCOME-TYPE)
                                      (list GNC-EXPENSE-TYPE)))))
                       (qif-map-entry:set-display?! entry #t)
                       (hash-set! memo-hash key-string entry)))))
             splits)))
        (qif-file:xtns file)))
     qif-files)

    ;; build display list 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (qif-map-entry:display? (cdr elt))
              (set! retval (cons (cdr elt) retval))))
        bin))
     (vector->list memo-hash))
    
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

(define (qif-import:any-new-stock-accts? hash-table) 
  (let ((retval #f))
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (and 
               (qif-map-entry:new-acct? (cdr elt))
               (qif-map-entry:display? (cdr elt))
               (or 
                (memv GNC-STOCK-TYPE 
                      (qif-map-entry:allowed-types (cdr elt)))
                (memv GNC-MUTUAL-TYPE 
                      (qif-map-entry:allowed-types (cdr elt)))))
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

(define qif-import:account-name-regexp #f)

(define (qif-import:get-account-name fullname)
  (if (not qif-import:account-name-regexp)
      (let* ((rstr ":([^:]+)$|^([^:]+)$")
             (newstr (regexp-substitute/global 
                      #f ":" rstr 'pre (gnc:account-separator-char) 'post)))
        
        (set! qif-import:account-name-regexp (make-regexp newstr))))
  
  (let ((match (regexp-exec qif-import:account-name-regexp fullname)))
    (if match
        (begin 
          (let ((substr (match:substring match 1)))
            (if substr
                substr
                (match:substring match 2))))
        fullname)))

(define (qif-import:setup-stock-hash hash-table)
  (let ((newhash (make-hash-table 20))
        (names '()))
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (and 
               (qif-map-entry:new-acct? (cdr elt))
               (qif-map-entry:display? (cdr elt)) 
               (or 
                (memv GNC-STOCK-TYPE 
                      (qif-map-entry:allowed-types (cdr elt)))
                (memv GNC-MUTUAL-TYPE 
                      (qif-map-entry:allowed-types (cdr elt)))))
              (let* ((name (qif-map-entry:qif-name (cdr elt)))
                     (stock-name (qif-import:get-account-name name)))
                (if (not stock-name)
                    (begin
                      (display "stock-name #f.. name ==")
                      (display name)(newline)))
                      
                (if (not (hash-ref newhash stock-name))
                    (begin 
                      (set! names (cons stock-name names))
                      (hash-set! newhash stock-name 
                                 (gnc:commodity-create 
                                  stock-name
                                  GNC_COMMODITY_NS_NYSE
                                  stock-name
                                  ""
                                  100000)))))))
        bin))
     (vector->list hash-table))
    (list newhash (sort names string<?))))


(define (qif-import:refresh-match-selection matches item)
  (if (> item -1)
      (let ((i 0))
        (for-each-in-order 
         (lambda (match)
           (if (= i item)
               (if (cdr match) 
                   (set-cdr! match #f)
                   (set-cdr! match #t))
               (set-cdr! match #f))
           (set! i (+ 1 i)))
         matches))))


