;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-to-gnc.scm
;;;  this is where QIF transactions are transformed into a 
;;;  Gnucash account tree.
;;;
;;;  Copyright 2000-2001 Bill Gribble <grib@billgribble.com> 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-to-gnc.scm")

(define (gnc:qif-fuzzy= num-1 num-2)
  (< (abs (- num-1 num-2)) .00000001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  find-or-make-acct:
;;  given a colon-separated account path, return an Account* to
;;  an existing or new account.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:find-or-make-acct acct-info currency security 
                                      gnc-acct-hash old-group new-group)
  (let* ((separator (string-ref (gnc:account-separator-char) 0))
         (gnc-name (qif-map-entry:gnc-name acct-info))
         (existing-account (hash-ref gnc-acct-hash gnc-name))
         (same-gnc-account 
          (gnc:get-account-from-full-name old-group gnc-name separator))
         (allowed-types 
          (qif-map-entry:allowed-types acct-info))
         (make-new-acct #f)
         (incompatible-acct #f))

    (define (make-unique-name-variant long-name short-name)
      (if (gnc:get-account-from-full-name old-group long-name separator)
          (let loop ((count 2))
            (let ((test-name 
                   (string-append long-name (sprintf #f " %a" count))))
              (if (gnc:get-account-from-full-name 
                   old-group test-name separator)
                  (loop (+ 1 count))
                  test-name)))
          short-name))

    ;; just because we found an account doesn't mean we can use it.
    ;; if the name is in use but the currency, security, or type are
    ;; incompatible, we need to create a new account with a modified
    ;; name.
    (if same-gnc-account 
        (if (and (gnc:commodity-equiv? 
                  currency (gnc:account-get-currency same-gnc-account))
                 (or (eq? (gnc:account-get-security same-gnc-account) #f)
                     (gnc:commodity-equiv? 
                      security (gnc:account-get-security same-gnc-account)))
                 (list? allowed-types)
                 (memq (gnc:account-get-type same-gnc-account)
                       allowed-types))
            (begin 
              ;; everything is ok, so we can just use the same
              ;; account.  Make sure we make the same type. 
              (set! make-new-acct #f)
              (set! incompatible-acct #f)
              (set! allowed-types 
                    (list (gnc:account-get-type same-gnc-account))))
            (begin 
              ;; there's an existing account with that name, so we
              ;; have to make a new acct with different properties and
              ;; something to indicate that it's different
              (set! make-new-acct #t)
              (set! incompatible-acct #t)))
        (begin 
          ;; otherwise, there is no existing account with the same 
          ;; name.
          (set! make-new-acct #t)
          (set! incompatible-acct #f)))
    
    (if existing-account 
        existing-account 
        (let ((new-acct (gnc:malloc-account))
              (parent-acct #f)
              (parent-name #f)
              (acct-name #f)
              (last-colon #f))
          (set! last-colon (string-rindex gnc-name separator))
          
          (gnc:account-begin-edit new-acct)

          ;; if this is a copy of an existing gnc account, copy the
          ;; account properties.  For incompatible existing accts,
          ;; we'll do something different later.
          (if same-gnc-account
              (begin 
                (gnc:account-set-name 
                 new-acct (gnc:account-get-name same-gnc-account))
                (gnc:account-set-description
                 new-acct (gnc:account-get-description same-gnc-account))
                (gnc:account-set-type
                 new-acct (gnc:account-get-type same-gnc-account))
                (gnc:account-set-currency
                 new-acct (gnc:account-get-currency same-gnc-account))
                (gnc:account-set-notes 
                 new-acct (gnc:account-get-notes same-gnc-account))
                (gnc:account-set-code 
                 new-acct (gnc:account-get-code same-gnc-account))
                (gnc:account-set-security
                 new-acct (gnc:account-get-security same-gnc-account))))
          
          ;; make sure that if this is a nested account foo:bar:baz,
          ;; foo:bar and foo exist also.
          (if last-colon
              (let ((pinfo (make-qif-map-entry)))
                (set! parent-name (substring gnc-name 0 last-colon))
                (set! acct-name (substring gnc-name (+ 1 last-colon) 
                                           (string-length gnc-name)))
                (qif-map-entry:set-qif-name! pinfo parent-name)
                (qif-map-entry:set-gnc-name! pinfo parent-name)
                (qif-map-entry:set-allowed-types! 
                 pinfo (qif-map-entry:allowed-types acct-info))
                
                (set! parent-acct (qif-import:find-or-make-acct 
                                   pinfo currency security 
                                   gnc-acct-hash old-group new-group)))
              (begin 
                (set! acct-name gnc-name)))
          
          ;; if this is a new account, use the 
          ;; parameters passed in
          (if make-new-acct
              (begin 
                ;; set the name, description, etc.
                (gnc:account-set-name new-acct acct-name)
                (if (qif-map-entry:description acct-info)
                    (gnc:account-set-description 
                     new-acct (qif-map-entry:description acct-info)))
                (gnc:account-set-currency new-acct currency)
                (gnc:account-set-security new-acct security)
                
                ;; if it's an incompatible account, set the
                ;; name to be unique, and a description that 
                ;; hints what's happening 
                (if incompatible-acct
                    (let ((new-name (make-unique-name-variant 
                                     gnc-name acct-name)))
                      (gnc:account-set-name new-acct new-name)
                      (gnc:account-set-description 
                       new-acct 
                       (_ "QIF import: Name conflict with another account."))))
                
                ;; set the account type.  this could be smarter. 
                (if (qif-map-entry:allowed-types acct-info)
                    (gnc:account-set-type 
                     new-acct (car (qif-map-entry:allowed-types acct-info))))))
          
          (gnc:account-commit-edit new-acct)
          (if parent-acct
              (gnc:account-insert-subaccount parent-acct new-acct)
              (gnc:group-insert-account new-group new-acct))
          
          (hash-set! gnc-acct-hash gnc-name new-acct)
          new-acct))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:qif-to-gnc 
;; this is the top-level of the back end conversion from 
;; QIF to GNC.  all the account mappings and so on should be 
;; done before this is called. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:qif-to-gnc qif-files-list 
                               qif-acct-map qif-cat-map 
                               qif-memo-map stock-map 
                               default-currency-name)
  (gnc:backtrace-if-exception 
   (lambda ()
     (let* ((old-group (gnc:get-current-group))
            (new-group (gnc:malloc-account-group))
            (gnc-acct-hash (make-hash-table 20))
            (separator (string-ref (gnc:account-separator-char) 0))
            (default-currency 
              (gnc:commodity-table-find-full 
               (gnc:engine-commodities) 
               GNC_COMMODITY_NS_ISO default-currency-name))
            (sorted-accounts-list '())
            (markable-xtns '())
            (sorted-qif-files-list 
             (sort qif-files-list 
                   (lambda (a b)
                     (> (length (qif-file:xtns a)) 
                        (length (qif-file:xtns b))))))
            (progress-dialog #f)
            (work-to-do 0)
            (work-done 0))
       
       ;; first, build a local account tree that mirrors the gnucash
       ;; accounts in the mapping data.  we need to iterate over the
       ;; cat-map and the acct-map to build the list
       (hash-fold 
        (lambda (k v p)
          (if (qif-map-entry:display? v)
              (set! sorted-accounts-list
                    (cons v sorted-accounts-list)))
          #t)
        #t qif-acct-map)

       (hash-fold 
        (lambda (k v p)
          (if (qif-map-entry:display? v)
              (set! sorted-accounts-list
                    (cons v sorted-accounts-list)))
          #t)
        #t qif-cat-map)

       (hash-fold 
        (lambda (k v p)
          (if (qif-map-entry:display? v)
              (set! sorted-accounts-list
                    (cons v sorted-accounts-list)))
          #t)
        #t qif-memo-map)
       
       ;; sort the account info on the depth of the account path.  if a
       ;; short part is explicitly mentioned, make sure it gets created
       ;; before the deeper path, which will create the parent accounts
       ;; without the information about their type.
       (set! sorted-accounts-list 
             (sort sorted-accounts-list 
                   (lambda (a b)
                     (let ((a-depth 
                            (length 
                             (string-split-on (qif-map-entry:gnc-name a) 
                                              separator)))
                           (b-depth 
                            (length 
                             (string-split-on (qif-map-entry:gnc-name b) 
                                              separator))))
                       (< a-depth b-depth)))))
       
       ;; make all the accounts 
       (for-each 
        (lambda (acctinfo)
          (let* ((security 
                  (and stock-map 
                       (hash-ref stock-map 
                                 (qif-import:get-account-name 
                                  (qif-map-entry:qif-name acctinfo)))))
                 (ok-types (qif-map-entry:allowed-types acctinfo))
                 (equity? (memq GNC-EQUITY-TYPE ok-types)))
            
            (cond ((and equity? security)  ;; a "retained holdings" acct
                   (qif-import:find-or-make-acct acctinfo 
                                                 security security
                                                 gnc-acct-hash 
                                                 old-group new-group))
                  (security 
                   (qif-import:find-or-make-acct 
                    acctinfo default-currency security
                    gnc-acct-hash old-group new-group))
                  (#t 
                   (qif-import:find-or-make-acct 
                    acctinfo default-currency default-currency
                    gnc-acct-hash old-group new-group)))))
        sorted-accounts-list)
       
       ;; before trying to mark transactions, prune down the list of 
       ;; ones to match. 
       (for-each 
        (lambda (qif-file)
          (for-each 
           (lambda (xtn)
             (set! work-to-do (+ 1 work-to-do))
             (let splitloop ((splits (qif-xtn:splits xtn)))             
               (if (qif-split:category-is-account? (car splits))
                   (begin 
                     (set! markable-xtns (cons xtn markable-xtns))
                     (set! work-to-do (+ 1 work-to-do)))
                   (if (not (null? (cdr splits)))
                       (splitloop (cdr splits))))))
           (qif-file:xtns qif-file)))
        qif-files-list)
       
       (if (> work-to-do 100)
           (begin 
             (set! progress-dialog (gnc:progress-dialog-new #f #f))
             (gnc:progress-dialog-set-title progress-dialog "Progress")
             (gnc:progress-dialog-set-heading progress-dialog
                                              "Importing transactions...")
             (gnc:progress-dialog-set-limits progress-dialog 0.0 100.0)))
       

       ;; now run through the markable transactions marking any
       ;; duplicates.  marked transactions/splits won't get imported.
       (if (> (length markable-xtns) 1)
           (let xloop ((xtn (car markable-xtns))
                       (rest (cdr markable-xtns)))
             (set! work-done (+ 1 work-done))
             (if progress-dialog 
                 (begin 
                   (gnc:progress-dialog-set-value 
                    progress-dialog (* 100 (/ work-done work-to-do)))
                   (gnc:progress-dialog-update progress-dialog))) 
             (if (not (qif-xtn:mark xtn))
                 (qif-import:mark-matching-xtns xtn rest))
             (if (not (null? (cdr rest)))
                 (xloop (car rest) (cdr rest)))))
       
       ;; iterate over files. Going in the sort order by number of 
       ;; transactions should give us a small speed advantage.
       (for-each 
        (lambda (qif-file)
          (for-each 
           (lambda (xtn)
             (set! work-done (+ 1 work-done))
             (if progress-dialog 
                 (begin 
                   (gnc:progress-dialog-set-value 
                    progress-dialog (* 100 (/ work-done work-to-do)))
                   (gnc:progress-dialog-update progress-dialog))) 
             (if (not (qif-xtn:mark xtn))
                 (begin 
                   ;; create and fill in the GNC transaction
                   (let ((gnc-xtn (gnc:transaction-create)))
                     (gnc:transaction-begin-edit gnc-xtn)
                     
                     ;; build the transaction
                     (qif-import:qif-xtn-to-gnc-xtn 
                      xtn qif-file gnc-xtn gnc-acct-hash 
                      qif-acct-map qif-cat-map qif-memo-map)
                     
                     ;; rebalance and commit everything
                     (gnc:transaction-commit-edit gnc-xtn)))))
           (qif-file:xtns qif-file)))
        sorted-qif-files-list)
       
       ;; get rid of the progress dialog 
       (if progress-dialog
           (gnc:progress-dialog-destroy progress-dialog))
       
       new-group))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:qif-xtn-to-gnc-xtn
;; translate a single transaction to a set of gnucash splits and 
;; a gnucash transaction structure. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:qif-xtn-to-gnc-xtn qif-xtn qif-file gnc-xtn 
                                       gnc-acct-hash 
                                       qif-acct-map qif-cat-map qif-memo-map)
  (let ((splits (qif-xtn:splits qif-xtn))
        (gnc-near-split (gnc:split-create))
        (near-split-total (gnc:numeric-zero))
        (near-acct-info #f)
        (near-acct-name #f)
        (near-acct #f)
        (qif-payee (qif-xtn:payee qif-xtn))
        (qif-number (qif-xtn:number qif-xtn))
        (qif-action (qif-xtn:action qif-xtn))
        (qif-security (qif-xtn:security-name qif-xtn))
        (qif-memo (qif-split:memo (car (qif-xtn:splits qif-xtn))))
        (qif-from-acct (qif-xtn:from-acct qif-xtn))
        (qif-cleared (qif-xtn:cleared qif-xtn))
        (amt-cvt (lambda (n)
                   (if n
                       (gnc:double-to-gnc-numeric 
                        n GNC-DENOM-AUTO
                        (logior (GNC-DENOM-SIGFIGS 6) GNC-RND-ROUND))
                       (gnc:numeric-zero))))
        (n- (lambda (n) (gnc:numeric-neg n)))
        (nsub (lambda (a b) (gnc:numeric-sub a b 0 GNC-DENOM-LCD)))
        (n+ (lambda (a b) (gnc:numeric-add a b 0 GNC-DENOM-LCD)))
        (n* (lambda (a b) (gnc:numeric-mul a b 0 GNC-DENOM-REDUCE)))
        (n/ (lambda (a b) (gnc:numeric-div a b 0 GNC-DENOM-REDUCE))))
    
    ;; set properties of the whole transaction     
    (apply gnc:transaction-set-date gnc-xtn (qif-xtn:date qif-xtn))
    
    (if qif-payee
        (gnc:transaction-set-description gnc-xtn qif-payee))
    (if qif-number
        (gnc:transaction-set-xnum gnc-xtn qif-number))
    (if qif-memo
        (gnc:split-set-memo gnc-near-split qif-memo))
    
    (if (eq? qif-cleared 'cleared)        
        (gnc:split-set-reconcile gnc-near-split #\c))
    (if (eq? qif-cleared 'reconciled)
        (gnc:split-set-reconcile gnc-near-split #\y))
    
    (if (not qif-security)
        (begin 
          ;; NON-STOCK TRANSACTIONS: the near account is the current
          ;; bank-account or the default associated with the file.
          ;; the far account is the one associated with the split
          ;; category.
          (set! near-acct-info (hash-ref qif-acct-map qif-from-acct))
          (set! near-acct-name (qif-map-entry:gnc-name near-acct-info))
          (set! near-acct (hash-ref gnc-acct-hash near-acct-name))
          
          ;; iterate over QIF splits.  Each split defines one "far
          ;; end" for the transaction.
          (for-each 
           (lambda (qif-split)
             (if (not (qif-split:mark qif-split))
                 (let ((gnc-far-split (gnc:split-create))
                       (far-acct-info #f)
                       (far-acct-name #f)
                       (far-acct-type #f)
                       (far-acct #f)
                       (split-amt (amt-cvt (qif-split:amount qif-split)))
                       (memo (qif-split:memo qif-split))
                       (cat (qif-split:category qif-split)))
                   
                   (if (not split-amt) (set! split-amt (gnc:numeric-zero)))
                   ;; fill the splits in (near first).  This handles
                   ;; files in multiple currencies by pulling the
                   ;; currency value from the file import.
                   (set! near-split-total (n+ near-split-total split-amt))
                   (gnc:split-set-value gnc-far-split (n- split-amt))
                   (gnc:split-set-share-amount gnc-far-split 
                                               (n- split-amt))

                   (if memo (gnc:split-set-memo gnc-far-split memo))
                   
                   ;; figure out what the far acct is
                   (cond 
                    ;; if the category is valid, use that.  look it up
                    ;; in the acct-hash if it's an account.
                    ((and (not (string=? cat ""))
                          (qif-split:category-is-account? qif-split))
                     (set! far-acct-info
                           (hash-ref qif-acct-map 
                                     (qif-split:category qif-split))))
                    
                    ;; .. look it up in the cat-hash if it's a category 
                    ((not (string=? cat ""))
                     (set! far-acct-info
                           (hash-ref qif-cat-map 
                                     (qif-split:category qif-split))))
                    ;; otherwise, try to find the payee in the
                    ;; memo-map if it's likely to have been used (only
                    ;; 1 split).  then try the memo.  if neither
                    ;; works, go back to the Unspecified account.
                    (#t
                     (set! far-acct-info 
                           (or 
                            (and (string? qif-payee)
                                 (not (string=? qif-payee ""))
                                 (= (length splits) 1)
                                 (hash-ref qif-memo-map qif-payee))
                            (and (string? memo)
                                 (not (string=? qif-memo ""))
                                 (hash-ref qif-memo-map memo))))
                     (if (not far-acct-info)
                         (set! far-acct-info
                               (hash-ref 
                                qif-cat-map 
                                (qif-split:category qif-split))))))
                   (set! far-acct-name (qif-map-entry:gnc-name far-acct-info))
                   (set! far-acct (hash-ref gnc-acct-hash far-acct-name))
                   
                   ;; set the reconcile status. 
                   (let ((cleared (qif-split:matching-cleared qif-split)))
                     (if (eq? 'cleared cleared)
                         (gnc:split-set-reconcile gnc-far-split #\c))
                     (if (eq? 'reconciled cleared)
                         (gnc:split-set-reconcile gnc-far-split #\y)))
                   
                   ;; finally, plug the split into the account 
                   (gnc:account-insert-split far-acct gnc-far-split)
                   (gnc:transaction-append-split gnc-xtn gnc-far-split))))
           splits)
          
          ;; the value of the near split is the total of the far splits.
          (gnc:split-set-value gnc-near-split near-split-total)
          (gnc:split-set-share-amount gnc-near-split near-split-total)
          (gnc:transaction-append-split gnc-xtn gnc-near-split)
          (gnc:account-insert-split near-acct gnc-near-split))
        
        ;; STOCK TRANSACTIONS: the near/far accounts depend on the
        ;; "action" encoded in the Number field.  It's generally the
        ;; security account (for buys, sells, and reinvests) but can
        ;; also be an interest, dividend, or SG/LG account.
        (let* ((share-price (amt-cvt (qif-xtn:share-price qif-xtn)))
               (num-shares (amt-cvt (qif-xtn:num-shares qif-xtn)))
               (split-amt (n* share-price num-shares)) 
               (xtn-amt (amt-cvt 
                         (qif-split:amount (car (qif-xtn:splits qif-xtn)))))
               (qif-accts #f)
               (qif-near-acct #f)
               (qif-far-acct #f)
               (qif-commission-acct #f)
               (far-acct-info #f)
               (far-acct-name #f)
               (far-acct #f)
               (commission-acct #f)
               (commission-amt (amt-cvt (qif-xtn:commission qif-xtn)))
               (commission-split #f)
               (defer-share-price #f)
               (gnc-far-split (gnc:split-create)))
          
          (if (not num-shares) (set! num-shares (gnc:numeric-zero)))
          (if (not share-price) (set! share-price (gnc:numeric-zero)))
          (if (not split-amt) (set! split-amt (n* num-shares share-price)))
          
          ;; I don't think this should ever happen, but I want 
          ;; to keep this check just in case. 
          (if (> (length splits) 1)
              (begin 
                (display "qif-import:qif-xtn-to-gnc-xtn : ")
                (display "splits in stock transaction!") (newline)))

          (set! qif-accts 
                (qif-split:accounts-affected (car (qif-xtn:splits qif-xtn))
                                             qif-xtn))
          
          (set! qif-near-acct (car qif-accts))
          (set! qif-far-acct (cadr qif-accts))
          (set! qif-commission-acct (caddr qif-accts))

          ;; translate the QIF account names into Gnucash accounts
          (if (and qif-near-acct qif-far-acct)
              (begin 
                (set! near-acct-info 
                      (or (hash-ref qif-acct-map qif-near-acct)
                          (hash-ref qif-cat-map qif-near-acct)))
                (set! near-acct-name (qif-map-entry:gnc-name near-acct-info))
                (set! near-acct (hash-ref gnc-acct-hash near-acct-name))
                
                (set! far-acct-info
                      (or (hash-ref qif-acct-map qif-far-acct)
                          (hash-ref qif-cat-map qif-far-acct)))
                (set! far-acct-name (qif-map-entry:gnc-name far-acct-info))
                (set! far-acct (hash-ref gnc-acct-hash far-acct-name))))
          
          ;; the amounts and signs: are shares going in or out? 
          ;; are amounts currency or shares? 
          (case qif-action
            ((buy buyx reinvint reinvdiv reinvsg reinvsh reinvmd reinvlg)
             (if (not share-price) (set! share-price (gnc:numeric-zero)))
             (gnc:split-set-share-amount gnc-near-split num-shares)
             (gnc:split-set-value gnc-near-split split-amt)
             (gnc:split-set-value gnc-far-split (n- xtn-amt))
             (gnc:split-set-share-amount gnc-far-split (n- xtn-amt)))
            
            ((sell sellx) 
             (if (not share-price) (set! share-price (gnc:numeric-zero)))
             (gnc:split-set-share-amount gnc-near-split (n- num-shares))
             (gnc:split-set-value gnc-near-split (n- split-amt))
             (gnc:split-set-value gnc-far-split xtn-amt)
             (gnc:split-set-share-amount gnc-far-split xtn-amt))
            
            ((cgshort cgshortx cgmid cgmidx cglong cglongx intinc intincx 
                      div divx miscinc miscincx xin rtrncap rtrncapx)
             (gnc:split-set-value gnc-near-split xtn-amt)
             (gnc:split-set-share-amount gnc-near-split xtn-amt)
             (gnc:split-set-value gnc-far-split (n- xtn-amt))
             (gnc:split-set-share-amount gnc-far-split (n- xtn-amt)))
            
            ((xout miscexp miscexpx margint margintx)
             (gnc:split-set-value gnc-near-split (n- xtn-amt))
             (gnc:split-set-share-amount gnc-near-split (n- xtn-amt))
             (gnc:split-set-value gnc-far-split  xtn-amt)
             (gnc:split-set-share-amount gnc-far-split  xtn-amt))
            
            ((shrsin)
             ;; getting rid of the old equity-acct-per-stock trick.
             ;; you must now have a cash/basis value for the stock.
             (gnc:split-set-share-amount gnc-near-split num-shares)
             (gnc:split-set-value gnc-near-split split-amt)
             (gnc:split-set-value gnc-far-split (n- xtn-amt))
             (gnc:split-set-share-amount gnc-far-split (n- xtn-amt)))
            
            ((shrsout)
             ;; shrsout is like shrsin             
             (gnc:split-set-share-amount gnc-near-split (n- num-shares))
             (gnc:split-set-value gnc-near-split (n- split-amt))
             (gnc:split-set-value gnc-far-split xtn-amt)
             (gnc:split-set-share-amount gnc-far-split xtn-amt))
            
            ;; stock splits: QIF just specifies the split ratio, not
            ;; the number of shares in and out, so we have to fetch
            ;; the number of shares from the security account 
            
            ;; FIXME : this could be wrong.  Make sure the
            ;; share-amount is at the correct time.
            ((stksplit)
             (let* ((splitratio (n/ num-shares (gnc:numeric-create 10 1)))
                    (in-shares 
                     (gnc:account-get-share-balance near-acct))
                    (out-shares (n* in-shares splitratio)))
               (gnc:split-set-share-amount gnc-near-split out-shares)
               (gnc:split-set-share-amount gnc-far-split (n- in-shares))
               (gnc:split-set-value gnc-near-split (n- split-amt))
               (gnc:split-set-value gnc-far-split split-amt))))
          
          (let ((cleared (qif-split:matching-cleared 
                          (car (qif-xtn:splits qif-xtn)))))
            (if (eq? 'cleared cleared)
                (gnc:split-set-reconcile gnc-far-split #\c))
            (if (eq? 'reconciled cleared)
                (gnc:split-set-reconcile gnc-far-split #\y)))
          
          (if qif-commission-acct
              (let* ((commission-acct-info 
                      (or (hash-ref qif-acct-map qif-commission-acct)
                          (hash-ref qif-cat-map qif-commission-acct)))
                     (commission-acct-name 
                      (qif-map-entry:gnc-name commission-acct-info)))
                (set! commission-acct 
                      (hash-ref gnc-acct-hash commission-acct-name))))
          
          (if (and commission-amt commission-acct)
              (begin 
                (set! commission-split (gnc:split-create))
                (gnc:split-set-value commission-split commission-amt)
                (gnc:split-set-share-amount commission-split commission-amt)))
          
          (if (and qif-near-acct qif-far-acct)
              (begin 
                (gnc:transaction-append-split gnc-xtn gnc-near-split)
                (gnc:account-insert-split near-acct gnc-near-split)
                
                (gnc:transaction-append-split gnc-xtn gnc-far-split)
                (gnc:account-insert-split far-acct gnc-far-split)
                
                (if commission-split
                    (begin 
                      (gnc:transaction-append-split gnc-xtn commission-split)
                      (gnc:account-insert-split commission-acct 
                                                commission-split)))))))
    ;; return the modified transaction (though it's ignored).
    gnc-xtn))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:mark-matching-xtns 
;;  find transactions that are the "opposite half" of xtn and 
;;  mark them so they won't be imported. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:mark-matching-xtns xtn candidate-xtns)
  (let splitloop ((splits-left (qif-xtn:splits xtn)))
    
    ;; splits-left starts out as all the splits of this transaction.
    ;; if multiple splits match up with a single split on the other 
    ;; end, we may remove more than one split from splits-left with
    ;; each call to mark-some-splits.  
    (if (not (null? splits-left))
        (if (and (not (qif-split:mark (car splits-left)))
                 (qif-split:category-is-account? (car splits-left)))
            (set! splits-left 
                  (qif-import:mark-some-splits 
                   splits-left xtn candidate-xtns))
            (set! splits-left (cdr splits-left))))
    
    (if (not (null? splits-left))
        (splitloop splits-left))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:mark-some-splits
;; find split(s) matching elements of splits and mark them so they
;; don't get imported.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:mark-some-splits splits xtn candidate-xtns)
  (let* ((split (car splits))
         (near-acct-name #f)
         (far-acct-name #f)
         (date (qif-xtn:date xtn))
         (amount (- (qif-split:amount split)))
         (group-amount #f)
         (memo (qif-split:memo split))        
         (security-name (qif-xtn:security-name xtn))
         (action (qif-xtn:action xtn))
         (bank-xtn? (not security-name))
         (cleared? #f)
         (different-acct-splits '())
         (same-acct-splits '())
         (how #f)
         (done #f))
    
    (if bank-xtn?
        (begin 
          (set! near-acct-name (qif-xtn:from-acct xtn))
          (set! far-acct-name (qif-split:category split))
          (set! group-amount 0.0)
          
          ;; group-amount is the sum of all the splits in this xtn
          ;; going to the same account as 'split'.  We might be able
          ;; to match this whole group to a single matching opposite
          ;; split.
          (for-each 
           (lambda (s)
             (if (and (qif-split:category-is-account? s)
                      (string=? far-acct-name (qif-split:category s)))
                 (begin
                   (set! same-acct-splits 
                         (cons s same-acct-splits))
                   (set! group-amount (- group-amount (qif-split:amount s))))
                 (set! different-acct-splits 
                       (cons s different-acct-splits))))
           splits)
          
          (set! same-acct-splits (reverse same-acct-splits))
          (set! different-acct-splits (reverse different-acct-splits)))
          
        ;; stock transactions.  they can't have splits as far as I can
        ;; tell, so the 'different-acct-splits' is always '()
        (let ((qif-accts 
               (qif-split:accounts-affected split xtn)))
          (set! near-acct-name (car qif-accts))
          (set! far-acct-name (cadr qif-accts))
          (set! same-acct-splits (list split))
          (if action
              ;; we need to do some special massaging to get
              ;; transactions to match up.  Quicken thinks the near
              ;; and far accounts are different than we do.
              (case action
                ((intincx divx cglongx cgmidx cgshortx rtrncapx margintx 
                          sellx)
                 (set! amount (- amount))
                 (set! near-acct-name (qif-xtn:from-acct xtn))
                 (set! far-acct-name (qif-split:category split)))
                ((miscincx miscexpx)
                 (set! amount (- amount))
                 (set! near-acct-name (qif-xtn:from-acct xtn))
                 (set! far-acct-name (qif-split:miscx-category split)))
                ((buyx)
                 (set! near-acct-name (qif-xtn:from-acct xtn))
                 (set! far-acct-name (qif-split:category split)))
                ((xout)
                 (set! amount (- amount)))))))
    
    ;; this is the grind loop.  Go over every unmarked transaction in
    ;; the candidate-xtns list.
    (let xtn-loop ((xtns candidate-xtns))
      (if (not (qif-xtn:mark (car xtns)))
          (begin 
            (set! how
                  (qif-import:xtn-has-matches? (car xtns) near-acct-name
                                               date amount group-amount))
            (if how
                (begin
                  (qif-import:merge-and-mark-xtns xtn same-acct-splits 
                                                  (car xtns) how)
                  (set! done #t)))))
      ;; iterate with the next transaction
      (if (and (not done)
               (not (null? (cdr xtns))))
          (xtn-loop (cdr xtns))))
    
    ;; return the rest of the splits to iterate on
    (if (not how)
        (cdr splits)
        (case (car how)
          ((one-to-one many-to-one)
           (cdr splits))
          ((one-to-many)
           different-acct-splits)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:xtn-has-matches?
;;  check for one-to-one, many-to-one, one-to-many split matches.
;;  returns either #f (no match) or a cons cell with the car being one
;;  of 'one-to-one 'one-to-many 'many-to-one, the cdr being a list of
;;  splits that were part of the matching group.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:xtn-has-matches? xtn acct-name date amount group-amt)
  (let ((matching-splits '())
        (same-acct-splits '())
        (this-group-amt 0.0)
        (how #f)
        (date-matches 
         (let ((self-date (qif-xtn:date xtn)))
           (and (pair? self-date)
                (pair? date)
                (eq? (length self-date) 3)
                (eq? (length date) 3)
                (= (car self-date) (car date))
                (= (cadr self-date) (cadr date))
                (= (caddr self-date) (caddr date))))))
    (if date-matches 
        (begin 
          ;; calculate a group total for splits going to acct-name    
          (let split-loop ((splits-left (qif-xtn:splits xtn)))
            (let ((split (car splits-left)))
              ;; does the account match up?
              (if (and (qif-split:category-is-account? split)
                       (string? acct-name)
                       (string=? (qif-split:category split) acct-name))
                  ;; if so, get the amount 
                  (let ((this-amt (qif-split:amount split))
                        (stock-xtn (qif-xtn:security-name xtn))
                        (action (qif-xtn:action xtn)))
                    ;; need to change the sign of the amount for some
                    ;; stock transactions (buy/sell both positive in
                    ;; QIF)
                    (if (and stock-xtn action)
                        (case action 
                          ((xout sellx intincx divx cglongx cgshortx 
                                 miscincx miscexpx)
                           (set! this-amt (- this-amt)))))
                    
                    ;; we might be done if this-amt is either equal 
                    ;; to the split amount or the group amount.
                    (cond 
                     ((gnc:qif-fuzzy= this-amt amount)
                      (set! how 
                            (cons 'one-to-one (list split))))
                     ((and group-amt (gnc:qif-fuzzy= this-amt group-amt))
                      (set! how
                            (cons 'one-to-many (list split))))
                     (#t
                      (set! same-acct-splits (cons split same-acct-splits))
                      (set! this-group-amt 
                            (+ this-group-amt this-amt))))))
              
              ;; if 'how' is non-#f, we are ready to return.
              (if (and (not how) 
                       (not (null? (cdr splits-left))))
                  (split-loop (cdr splits-left)))))
          
          ;; now we're out of the loop.  if 'how' isn't set, 
          ;; we can still have a many-to-one match.
          (if (and (not how)
                   (gnc:qif-fuzzy= this-group-amt amount))
              (begin 
                (set! how 
                      (cons 'many-to-one same-acct-splits))))))
    
    ;; we're all done.  'how' either is #f or a 
    ;; cons of the way-it-matched and a list of the matching 
    ;; splits. 
    how))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (qif-split:accounts-affected split xtn)
;;  Get the near and far ends of a split, returned as a list 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-split:accounts-affected split xtn) 
  (let ((near-acct-name #f)
        (far-acct-name #f)
        (commission-acct-name #f)
        (security (qif-xtn:security-name xtn))
        (action (qif-xtn:action xtn))
        (from-acct (qif-xtn:from-acct xtn)))
    
    ;; for non-security transactions, the near account is the 
    ;; acct in which the xtn is, and the far is the account 
    ;; linked by the category line. 
    
    (if (not security)
        ;; non-security transactions 
        (begin 
          (set! near-acct-name from-acct)
          (set! far-acct-name (qif-split:category split)))
        
        ;; security transactions : the near end is either the 
        ;; brokerage, the stock, or the category 
        (begin
          (case action
            ((buy buyx sell sellx reinvint reinvdiv reinvsg reinvsh 
                  reinvlg reinvmd shrsin shrsout stksplit)
             (set! near-acct-name (default-stock-acct from-acct security)))
            ((div cgshort cglong cgmid intinc miscinc miscexp 
                  rtrncap margint xin xout)
             (set! near-acct-name from-acct))
            ((divx cgshortx cglongx cgmidx intincx rtrncapx margintx)
             (set! near-acct-name 
                   (qif-split:category (car (qif-xtn:splits xtn)))))
            ((miscincx miscexpx)
             (set! near-acct-name 
                   (qif-split:miscx-category (car (qif-xtn:splits xtn))))))

          ;; the far split: where is the money coming from?  
          ;; Either the brokerage account, the category,
          ;; or an external account 
          (case action
            ((buy sell)
             (set! far-acct-name from-acct))
            ((buyx sellx miscinc miscincx miscexp miscexpx xin xout)
             (set! far-acct-name 
                   (qif-split:category (car (qif-xtn:splits xtn)))))
            ((stksplit)
             (set! far-acct-name (default-stock-acct from-acct security)))
            ((cgshort cgshortx reinvsg reinvsh)
             (set! far-acct-name
                   (default-cgshort-acct from-acct security)))
            ((cglong cglongx reinvlg)
             (set! far-acct-name
                   (default-cglong-acct from-acct security)))
            ((cgmid cgmidx reinvmd)
             (set! far-acct-name
                   (default-cgmid-acct from-acct security)))
            ((intinc intincx reinvint)
             (set! far-acct-name
                   (default-interest-acct from-acct security)))
            ((margint margintx)
             (set! far-acct-name
                   (default-margin-interest-acct from-acct)))
            ((rtrncap rtrncapx)
             (set! far-acct-name
                   (default-return-capital-acct from-acct)))
            ((div divx reinvdiv)
             (set! far-acct-name
                   (default-dividend-acct from-acct security)))            
            ((shrsin shrsout)
             (set! far-acct-name
                   (default-equity-holding security))))
          
          ;; the commission account, if it exists 
          (if (qif-xtn:commission xtn)
              (set! commission-acct-name 
                    (default-commission-acct from-acct)))))
    
    (list near-acct-name far-acct-name commission-acct-name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:merge-and-mark-xtns 
;; we know that the splits match.  Pick one to mark and 
;; merge the information into the other one.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:merge-and-mark-xtns xtn splits other-xtn how)
  ;; merge transaction fields 
  (let ((action (qif-xtn:action xtn))
        (o-action (qif-xtn:action other-xtn))
        (security (qif-xtn:security-name xtn))
        (o-security (qif-xtn:security-name other-xtn))
        (split (car splits))
        (match-type (car how))
        (match-splits (cdr how)))
    (case match-type 
      ;; many-to-one: the other-xtn has several splits that total
      ;; in amount to 'split'.  We want to preserve the multi-split
      ;; transaction.  
      ((many-to-one)
       (qif-xtn:mark-split xtn split)
       (qif-import:merge-xtn-info xtn other-xtn)
       (for-each 
        (lambda (s)
          (qif-split:set-matching-cleared! s (qif-xtn:cleared xtn)))
        match-splits))
      
      ;; one-to-many: 'split' is just one of a set of splits in xtn
      ;; that total up to the split in match-splits.
      ((one-to-many)
       (qif-xtn:mark-split other-xtn (car match-splits))
       (qif-import:merge-xtn-info other-xtn xtn)
       (for-each 
        (lambda (s)
          (qif-split:set-matching-cleared! 
           s (qif-xtn:cleared other-xtn)))
        splits))

      ;; otherwise: one-to-one, a normal single split match.
      (else 
       (cond 
        ;; this is a transfer involving a security xtn.  Let the 
        ;; security xtn dominate the way it's handled. 
        ((and (not action) o-action o-security)
         (qif-xtn:mark-split xtn split)
         (qif-import:merge-xtn-info xtn other-xtn)
         (qif-split:set-matching-cleared! 
          (car match-splits) (qif-xtn:cleared xtn)))
        
        ((and action (not o-action) security)
         (qif-xtn:mark-split other-xtn (car match-splits))
         (qif-import:merge-xtn-info other-xtn xtn)
         (qif-split:set-matching-cleared! 
          split (qif-xtn:cleared other-xtn)))
        
        ;; this is a security transaction from one brokerage to another
        ;; or within a brokerage.  The "foox" xtn has the most
        ;; information about what went on, so use it.
        ((and action o-action o-security)
         (case o-action
           ((buyx sellx cgshortx cgmidx cglongx intincx divx 
                  margintx rtrncapx miscincx miscexpx)
            (qif-xtn:mark-split xtn split)
            (qif-import:merge-xtn-info xtn other-xtn)
            (qif-split:set-matching-cleared!
             (car match-splits) (qif-xtn:cleared xtn)))
           
           (else 
            (qif-xtn:mark-split other-xtn (car match-splits))
            (qif-import:merge-xtn-info other-xtn xtn)
            (qif-split:set-matching-cleared! 
             split (qif-xtn:cleared other-xtn)))))        
        
        ;; otherwise, this is a normal no-frills split match.  if one
        ;; transaction has more splits than the other one,
        ;; (heuristically) mark the one with less splits.
        (#t 
         (if (< (length (qif-xtn:splits xtn))
                (length (qif-xtn:splits other-xtn)))
             (begin 
               (qif-xtn:mark-split xtn split)
               (qif-import:merge-xtn-info xtn other-xtn)
               (qif-split:set-matching-cleared!
                (car match-splits) (qif-xtn:cleared xtn)))
             
             (begin
               (qif-xtn:mark-split other-xtn (car match-splits))
               (qif-import:merge-xtn-info other-xtn xtn)
               (qif-split:set-matching-cleared!
                split (qif-xtn:cleared other-xtn))))))))))


(define (qif-import:merge-xtn-info from-xtn to-xtn)
  (if (and (qif-xtn:payee from-xtn)
           (not (qif-xtn:payee to-xtn)))
      (qif-xtn:set-payee! to-xtn (qif-xtn:payee from-xtn)))
  (if (and (qif-xtn:address from-xtn)
           (not (qif-xtn:address to-xtn)))
      (qif-xtn:set-address! to-xtn (qif-xtn:address from-xtn)))
  (if (and (qif-xtn:number from-xtn)
           (not (qif-xtn:number to-xtn)))
      (qif-xtn:set-number! to-xtn (qif-xtn:number from-xtn))))


(define (qif-xtn:mark-split xtn split)
  (qif-split:set-mark! split #t)
  (let ((all-marked #t))
    (let loop ((splits (qif-xtn:splits xtn)))
      (if (not (qif-split:mark (car splits)))
          (set! all-marked #f)
          (if (not (null? (cdr splits)))
              (loop (cdr splits)))))
    (if all-marked
        (qif-xtn:set-mark! xtn #t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:set-share-price split 
;; find the split that precedes 'split' in the account and set split's
;; share price to that.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:set-share-price split)
  (let* ((account (gnc:split-get-account split))
         (numsplits (gnc:account-get-split-count account)))
    (let loop ((i 0)
               (last-split #f))
      (let ((ith-split (gnc:account-get-split account i)))        
        (if (gw:wcp=? ith-split split)
            (if last-split
                (d-gnc:split-set-share-price 
                 split (d-gnc:split-get-share-price last-split)))
            (if (< i numsplits) (loop (+ 1 i) ith-split)))))))

