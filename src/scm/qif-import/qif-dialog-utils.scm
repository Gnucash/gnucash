;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-dialog-utils.scm
;;;  build qif->gnc account maps and put them in a displayable 
;;;  form. 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-dialog-utils.scm")

(define (qif-dialog:munge-account-mapping old-map new-info)
  (let ((new-name (car new-info))
        (new-type (cadr new-info))
        (new-descript (caddr new-info)))
    (list-set! old-map 1 new-name)
    (list-set! old-map 2 new-type)
    (cond ((qif-cat? (list-ref old-map 5))
           (qif-cat:set-description! (list-ref old-map 5) new-descript))
          ((qif-acct? (list-ref old-map 5))
           (qif-acct:set-description! (list-ref old-map 5) new-descript))
          (#t 
           (list-set! old-map 5 new-descript)))))
    

;; the account-display is a 3-columned list of accounts in the QIF
;; import dialog (the "Account" page of the notebook).  Column 1 is
;; the account name in the QIF file, column 2 is the number of QIF
;; xtns with that account name, and column 3 is the guess for the
;; translation.  Sorted on # transactions, then alpha.

(define (qif-dialog:make-account-display qif-files gnc-acct-info) 
  (let ((acct-hash (make-hash-table 20))
        (retval '()))

    ;; we want to make two passes here.  The first pass picks the
    ;; explicit Account descriptions and implicit "this" description
    ;; out of each file.  These are the best sources of info because
    ;; we will have types and so on for them.  The second pass picks
    ;; out account-style L fields and investment security names from
    ;; the transactions.  Hopefully we'll have most of the accounts
    ;; already located by that point.  Otherwise, we have to guess
    ;; them.

    ;; guess-acct returns a list that's
    ;; (qif-name gnc-name gnc-type new-acct?)
    ;; acct-hash hashes QIF account name to a list that's composed of
    ;; (qif-acct-name gnc-acct-name gnc-acct-type gnc-acct-new?
    ;;  num-qif-xtns qif-object) so we can find the properties later. 
    (for-each 
     (lambda (file)
       ;; first, get the explicit account references.  
       (for-each 
        (lambda (acct)
          (if (not (hash-ref acct-hash (qif-acct:name acct)))
              (hash-set! 
               acct-hash (qif-acct:name acct)
               (append 
                (qif-import:guess-acct (qif-acct:name acct) 
                                       (list (qif-acct:type acct))
                                       gnc-acct-info)
                (list 0 acct)))))
        (qif-file:accounts file))

       ;; then make an implicit account entry for the file
       (if (and (qif-file:account file)
                (qif-file:account-type file))
;                (not (eq? (qif-file:account-type file) GNC-STOCK-TYPE)))
           (let ((entry (hash-ref acct-hash (qif-file:account file))))
             (if entry
                 ;; increment the xtn count in place 
                 (list-set! entry 4
                            (+ (list-ref entry 4) 
                               (length (qif-file:xtns file))))
                 ;; make a new hash table entry for the account
                 ;; make it a Bank account by default. 
                 (hash-set! 
                  acct-hash (qif-file:account file)
                  (append (qif-import:guess-acct 
                           (qif-file:account file)
                           (list GNC-BANK-TYPE
                                 GNC-CCARD-TYPE)
                           gnc-acct-info)
                          (list 
                           (length (qif-file:xtns file)) 
                           #f)))))))
     qif-files)

    ;; now make the second pass through the files, looking at the 
    ;; transactions.  Hopefully the accounts are all there already.
    ;; stock accounts can have both a category/account and another
    ;; account ref from the security name.
    (for-each 
     (lambda (file)
       (for-each 
        (lambda (xtn)
          (let ((bank-xtn? (qif-xtn:bank-xtn? xtn))
                (stock-acct (qif-xtn:security-name xtn))
                (entry #f))
            (if (not bank-xtn?)
                (begin 
                  (set! entry (hash-ref acct-hash stock-acct))
                  (if entry
                      (list-set! entry 4
                                 (+ 1 (list-ref entry 4)))
                      (hash-set! acct-hash stock-acct 
                                 (append (qif-import:guess-acct
                                          stock-acct
                                          (list GNC-STOCK-TYPE
                                                GNC-MUTUAL-TYPE)
                                          gnc-acct-info)
                                         (list 1 xtn)))))))
          
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
                     (if entry
                         (list-set! entry 4
                                    (+ 1 (list-ref entry 4)))
                         (hash-set! acct-hash xtn-acct 
                                    (append (qif-import:guess-acct
                                             xtn-acct 
                                             (list 
                                              GNC-BANK-TYPE
                                              GNC-CCARD-TYPE
                                              GNC-STOCK-TYPE)
                                             gnc-acct-info)
                                            (list 1 #f))))))))
           (qif-xtn:splits xtn)))
        (qif-file:xtns file)))
     qif-files)
    
    ;; now that the hash table is filled, make the display list 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (> (list-ref (cdr elt) 4) 0)
              (set! retval 
                    (cons (cdr elt) retval))))
        bin))
     (vector->list acct-hash))

    (list-set! gnc-acct-info 1 acct-hash)

    ;; sort by number of transactions with that account so the 
    ;; most important are at the top
;    (set! retval (sort-list retval 
;                            (lambda (a b)
;                              (or 
;                               (> (list-ref a 4) (list-ref b 4))
;                               (and 
;                                (eq? (list-ref a 4) (list-ref b 4))
;                                (string<? (car a) (car b)))))))
    retval))


;; the category display is similar to the Account display.  
;; QIF category name, xtn count, then GNUcash account. 

(define (qif-dialog:make-category-display qif-files gnc-acct-info) 
  (let ((cat-hash (make-hash-table 20))
        (retval '()))

    ;; get the Cat entries from each file 
    (for-each 
     (lambda (file)
       (for-each 
        (lambda (cat)
          (if (not (hash-ref cat-hash (qif-cat:name cat)))
              (begin 
                (hash-set! cat-hash
                           (qif-cat:name cat)
                           (append 
                            (qif-import:guess-acct
                             (qif-cat:name cat)
                             (if (qif-cat:expense-cat cat)
                                 (list GNC-EXPENSE-TYPE)
                                 (list GNC-INCOME-TYPE))
                             gnc-acct-info)
                            (list 0 cat))))))
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
                     (if entry
                         (list-set! entry 4
                                    (+ 1 (list-ref entry 4)))
                         (hash-set! cat-hash xtn-cat 
                                    (append (qif-import:guess-acct
                                             xtn-cat
                                             (if (> (qif-split:amount split) 0)
                                                 (list GNC-INCOME-TYPE)
                                                 (list GNC-EXPENSE-TYPE))
                                             gnc-acct-info)
                                            (list 1 #f))))))))
           (qif-xtn:splits xtn)))
        (qif-file:xtns qif-file)))
     qif-files)
    
    ;; now that the hash table is filled, make the display list 
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (elt)
          (if (> (list-ref (cdr elt) 4) 0)
              (set! retval (cons (cdr elt) retval))))
        bin))
     (vector->list cat-hash))
    
    (list-set! gnc-acct-info 2 cat-hash)

    ;; sort by number of transactions with that account so the 
    ;; most important are at the top
;    (set! retval (sort-list retval 
;                            (lambda (a b)
;                              (or 
;                               (> (list-ref a 4) (list-ref b 4))
;                               (and 
;                                (eq? (list-ref a 4) (list-ref b 4))
;                                (string<? (car a) (car b)))))))
    retval))


(define (qif-dialog:qif-file-loaded? filename list-of-files) 
  (let ((status (map 
                 (lambda (file)
                   (string=? filename (qif-file:path file)))
                 list-of-files)))
    (if (memq #t status)
        #t
        #f)))
    
(define (qif-dialog:unload-qif-file filename list-of-files)
  (delq #f
        (map 
         (lambda (file)
           (if (string=? filename (qif-file:path file))
               #f
               file))
           list-of-files)))
