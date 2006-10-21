;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-invst-xtn-import.scm
;;;  routines for converting a QIF investment transaction to a gnc
;;;  transaction
;;;
;;;  Copyright (c) 2001 Linux Developers Group, Inc.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; account name generators.  these are changeable by the user during
;; the mapping phase but you have to start somewhere.

(define (default-stock-acct brokerage security)
  (string-append brokerage ":" security))

(define (default-dividend-acct brokerage security)
  (string-append (_ "Dividends") ":" 
                 brokerage ":" 
                 security))

(define (default-interest-acct brokerage security) 
  (string-append (_ "Interest") ":" 
                 brokerage ":"  
                 security))

(define (default-capital-return-acct brokerage security) 
  (string-append (_ "Cap Return") ":" 
                 brokerage ":"  
                 security))

(define (default-cglong-acct brokerage security)
  (string-append (_ "Cap. gain (long)") ":" 
                 brokerage ":" 
                 security))

(define (default-cgmid-acct brokerage security)
  (string-append (_ "Cap. gain (mid)") ":" 
                 brokerage ":" 
                 security))

(define (default-cgshort-acct brokerage security)
  (string-append (_ "Cap. gain (short)") ":" 
                 brokerage ":" 
                 security))

(define (default-equity-holding security) (_ "Retained Earnings"))

(define (default-equity-account) (_ "Retained Earnings"))  

(define (default-commission-acct brokerage) 
  (string-append (_ "Commissions") ":" 
                 brokerage))

(define (default-margin-interest-acct brokerage) 
  (string-append (_ "Margin Interest") ":" 
                 brokerage))

(define (default-unspec-acct)
  (_ "Unspecified"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (qif-io:invst-xtn-accounts-affected xtn)
;;  What accounts are affected by the transaction?  it depends on 
;;  the 'action' field.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:invst-xtn-accounts-affected xtn) 
  (let* ((near-acct #f)
         (far-acct #f)
         (security (qif-io:invst-xtn-security xtn))
         (action (qif-io:parse-action-field (qif-io:invst-xtn-action xtn)))
         (from-acct (qif-io:invst-xtn-source-acct xtn))
         (category (qif-io:invst-xtn-category xtn))
         (parsed-cat 
          (if category (qif-io:parse-category category) 
              (list "" #f #f #f #f #f))))
                   
    ;; the "near split", i.e. the split that would normally go to the
    ;; source account.
    (case action
      ((buy buyx sell sellx reinvint reinvdiv reinvsg reinvsh 
            reinvlg reinvmd shrsin shrsout stksplit)
       (set! near-acct 
             (cons (default-stock-acct from-acct security) 'security)))
      ((div cgshort cglong cgmid intinc miscinc miscexp 
            rtrncap margint xin xout)
       (set! near-acct (cons from-acct 'account)))
      ((divx cgshortx cglongx cgmidx intincx rtrncapx margintx)
       (set! near-acct 
             (cons (car parsed-cat) 
                   (if (list-ref parsed-cat 1) 'account 'category))))
      ((miscincx miscexpx)
       (set! near-acct 
             (cons (list-ref parsed-cat 3)
                   (if (list-ref parsed-cat 4) 'account 'category))))
      (else 
       (throw 'qif-io:unhandled-action action)))
    
    ;; the far split: where is the money coming from?  Either the
    ;; brokerage account, the category, or an external account
    (case action
      ((buy sell)
       (set! far-acct 
             (cons from-acct 'account)))
      ((buyx sellx miscinc miscincx miscexp miscexpx xin xout)
       (set! far-acct 
             (cons (list-ref parsed-cat 0)
                   (if (list-ref parsed-cat 1) 'account 'category))))
      ((stksplit)
       (set! far-acct 
             (cons (default-stock-acct from-acct security) 'security)))
      ((cgshort cgshortx reinvsg reinvsh)
       (set! far-acct
             (cons (default-cgshort-acct from-acct security) 'brokerage)))
      ((cglong cglongx reinvlg)
       (set! far-acct
             (cons (default-cglong-acct from-acct security) 'brokerage)))
      ((cgmid cgmidx reinvmd)
       (set! far-acct
             (cons (default-cgmid-acct from-acct security) 'brokerage)))
      ((intinc intincx reinvint)
       (set! far-acct
             (cons (default-interest-acct from-acct security) 'brokerage)))
      ((margint margintx)
       (set! far-acct
             (cons (default-margin-interest-acct from-acct) 'brokerage)))
      ((rtrncap rtrncapx)
       (set! far-acct
             (cons (default-capital-return-acct from-acct) 'brokerage)))
      ((div divx reinvdiv)
       (set! far-acct
             (cons (default-dividend-acct from-acct security) 'brokerage)))
      ((shrsin shrsout)
       (set! far-acct
             (cons (default-equity-holding security) 'account)))
      (else 
       (throw 'qif-io:unhandled-action action)))
    
    (list near-acct far-acct (default-commission-acct from-acct))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-io:invst-xtn-import 
;;  translate a single invst transaction into a GNC transaction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-io:invst-xtn-import qif-xtn qif-file gnc-acct-info commodity)
  (let ((gnc-xtn (xaccMallocTransaction (gnc-get-current-book)))
        (format-info (qif-io:file-invst-xtn-format qif-file)))
    ;; utility to make a new split and add it both to an 
    ;; account and to the transaction
    (define (add-split acct-info amount value memo reconcile)
      (let* ((acct-name (car acct-info))
             (acct-type (cdr acct-info))
             (acct (qif-io:acct-table-lookup 
                    gnc-acct-info acct-name acct-type))
             (split (xaccMallocSplit (gnc-get-current-book))))
        ;; make the account if necessary 
        (if (not acct)
            (begin 
              (set! acct (xaccMallocAccount (gnc-get-current-book)))
              (xaccAccountSetName acct acct-name)
              (qif-io:acct-table-insert! gnc-acct-info 
                                         acct-name acct-type acct)))
        ;; fill in the split 
        (xaccSplitSetAmount split amount)
        (xaccSplitSetValue split value)
        (xaccSplitSetMemo split memo)
        (xaccSplitSetReconcile split reconcile)
        
        ;; add it to the account and the transaction
        (xaccAccountBeginEdit acct)
        (xaccSplitSetAccount split acct)
        (xaccAccountCommitEdit acct)
        (xaccTransAppendSplit gnc-xtn split)
        split))

    (define (lookup-balance acct-info)
      (let ((acct (qif-io:acct-table-lookup gnc-acct-info 
                                            (car acct-info) (cdr acct-info))))
        (xaccAccountGetBalance acct)))
    
    (if (not (qif-io:invst-xtn-source-acct qif-xtn))
        (qif-io:invst-xtn-set-source-acct! 
         qif-xtn (qif-io:file-default-src-acct qif-file)))
    
    (xaccTransBeginEdit gnc-xtn)
    (xaccTransSetCurrency gnc-xtn commodity)
    
    ;; set the transaction date, number and description 
    (let ((date (qif-io:parse-date/format 
                 (qif-io:invst-xtn-date qif-xtn) 
                 (qif-io:invst-xtn-date format-info))))
      (apply xaccTransSetDate gnc-xtn date))
    
    (xaccTransSetNum gnc-xtn (qif-io:invst-xtn-action qif-xtn))
    (xaccTransSetDescription gnc-xtn (qif-io:invst-xtn-payee qif-xtn))
    
    ;; get the relevant info, including 'near-acct' and 'far-acct', 
    ;; the accounts affected by the transaction
    (let* ((action 
            (qif-io:parse-action-field (qif-io:invst-xtn-action qif-xtn)))
           (num-shares
            (let ((val (qif-io:invst-xtn-share-amount qif-xtn)))
              (if val 
                  (qif-io:parse-number/format 
                   val (qif-io:invst-xtn-share-amount format-info))
                  #f)))
           (share-price 
            (let ((val (qif-io:invst-xtn-share-price qif-xtn)))
              (if val 
                  (qif-io:parse-number/format 
                   val (qif-io:invst-xtn-share-price format-info))
                  #f)))
           (commission-val
            (let ((val (qif-io:invst-xtn-commission qif-xtn)))
              (if val 
                  (qif-io:parse-number/format 
                   val (qif-io:invst-xtn-commission format-info))
                  #f)))
           (total-val
            (let ((uamt (qif-io:invst-xtn-u-amount qif-xtn))
                  (tamt (qif-io:invst-xtn-t-amount qif-xtn))
                  ($amt (qif-io:invst-xtn-$-amount qif-xtn)))
              (cond 
               (uamt 
                (qif-io:parse-number/format 
                 uamt (qif-io:invst-xtn-u-amount format-info)))
               (tamt 
                (qif-io:parse-number/format 
                 tamt (qif-io:invst-xtn-t-amount format-info)))
               ($amt 
                (qif-io:parse-number/format 
                 $amt (qif-io:invst-xtn-$-amount format-info)))
               (#t (gnc-numeric-zero)))))
           (action-val 
            (if (and num-shares share-price)
                (gnc-numeric-mul num-shares share-price
                                 (gnc-numeric-denom total-val)
                                 GNC-RND-ROUND)
                (gnc-numeric-zero)))
           (cleared 
            (qif-io:parse-cleared-field (qif-io:invst-xtn-cleared qif-xtn)))
           (payee (qif-io:invst-xtn-payee qif-xtn))
           (memo (qif-io:invst-xtn-memo qif-xtn))
           (accounts-affected 
            (qif-io:invst-xtn-accounts-affected qif-xtn))
           (near-acct (car accounts-affected))
           (far-acct (cadr accounts-affected))
           (commission-acct 
            (cons (default-commission-acct 
                    (qif-io:invst-xtn-source-acct qif-xtn)) 'brokerage))
           (n- (lambda (n) (gnc-numeric-neg n))))
      
      ;; now build the splits.  We have to switch on the action 
      ;; again to get the signs of the amounts, and whether we use the 
      ;; monetary value or share count. 
      (case action 
        ((buy buyx reinvint reinvdiv reinvsg reinvsh reinvmd reinvlg)
         (add-split near-acct num-shares action-val memo cleared)
         (add-split far-acct (n- total-val) (n- total-val) memo cleared)
         (if commission-val 
             (add-split commission-acct commission-val commission-val
                        memo cleared)))

        ((sell sellx)
         (add-split near-acct (n- num-shares) (n- action-val) memo cleared)
         (add-split far-acct total-val total-val memo cleared)
         (if commission-val 
             (add-split commission-acct commission-val commission-val
                        memo cleared)))
        
        ;; fixme: can these have commissions? 
        ((cgshort cgshortx cgmid cgmidx cglong cglongx intinc intincx 
                  div divx miscinc miscincx xin rtrncap rtrncapx)
         (add-split near-acct total-val total-val memo cleared)
         (add-split far-acct (n- total-val) (n- total-val) memo #\n))

        ;; fixme: can these have commissions? 
        ((xout miscexp miscexpx margint margintx) 
         (add-split near-acct (n- total-val) (n- total-val) memo cleared)
         (add-split far-acct total-val total-val memo #\n))
        ((shrsin)
         (add-split near-acct num-shares action-val memo cleared)
         (add-split far-acct (n- total-val) (n- total-val) memo cleared)
         (if commission-val 
             (add-split commission-acct commission-val commission-val
                        memo cleared)))
        ((shrsout)
         (add-split near-acct (n- num-shares) (n- action-val) memo cleared)
         (add-split far-acct total-val total-val memo cleared)
         (if commission-val 
             (add-split commission-acct commission-val commission-val
                        memo cleared)))
        ((stksplit)
         (let* ((splitratio (gnc-numeric-div
                             num-shares (gnc-numeric-create 10 1)
                             GNC-DENOM-AUTO GNC-DENOM-REDUCE))
                (in-shares (lookup-balance near-acct))
                (out-shares (n* in-shares splitratio)))
           (add-split near-acct out-shares (n- action-amt) memo cleared)
           (add-split far-acct in-shares action-amt memo cleared)))
        (else 
         (throw 'qif-io:unhandled-action action))))
    
    (xaccTransCommitEdit gnc-xtn)
    gnc-xtn))


  
           
      
  
  
