;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-to-gnc.scm
;;;  this is where QIF transactions are transformed into a 
;;;  Gnucash account tree.
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-to-gnc.scm")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  find-or-make-acct:
;;  given a colon-separated account path, return an Account* to
;;  an existing or new account.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:find-or-make-acct gnc-name gnc-acct-hash 
                                      gnc-type qif-info acct-group)
  (let ((existing-account (hash-ref gnc-acct-hash gnc-name))
        (same-gnc-account (gnc:get-account-from-full-name acct-group
                                                          gnc-name 
                                                          #\:))
        (check-full-name #f)
        (make-new-acct #f))
    
    (if (or (pointer-token-null? same-gnc-account) 
            (and (not (pointer-token-null? same-gnc-account))
                 (not (string=? 
                       (gnc:account-get-full-name same-gnc-account)
                       gnc-name))))
        (set! make-new-acct #t))
    
    (if (and make-new-acct
             (not (pointer-token-null? same-gnc-account)))
        (begin (display " BUG IN get-account-from-full-name !!")(newline)))
    
    (if existing-account 
        existing-account 
        (let ((new-acct (gnc:malloc-account))
              (parent-acct #f)
              (parent-name #f)
              (acct-name #f)
              (last-colon #f))
          (set! last-colon (string-rindex gnc-name #\:))

          (gnc:init-account new-acct)
          (gnc:account-begin-edit new-acct 1)

          ;; if this is a copy of an existing gnc account, 
          ;; copy the account properties 
          (if (not make-new-acct)
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
              (begin                 
                (set! parent-name (substring gnc-name 0 last-colon))
                (set! acct-name (substring gnc-name (+ 1 last-colon) 
                                           (string-length gnc-name)))
                (set! parent-acct (qif-import:find-or-make-acct 
                                   parent-name gnc-acct-hash 
                                   gnc-type qif-info
                                   acct-group))
                
                ;; if this is a new account, use the 
                ;; parameters passed in
                (if make-new-acct
                    (begin 
                      (gnc:account-set-name new-acct acct-name)
                      (if gnc-type (gnc:account-set-type new-acct gnc-type))
                      (cond ((and (qif-acct? qif-info)
                                  (qif-acct:description qif-info))
                             (gnc:account-set-description 
                              new-acct (qif-acct:description qif-info)))
                            ((and (qif-cat? qif-info)
                                  (qif-cat:description qif-info))
                             (gnc:account-set-description 
                              new-acct (qif-cat:description qif-info)))
                            ((and (qif-xtn? qif-info)
                                  (not (qif-xtn:bank-xtn? qif-info)))
                             (gnc:account-set-security 
                              (qif-xtn:security-name qif-info)))
                            ((string? qif-info)
                             (gnc:account-set-description 
                              new-acct qif-info)))))
                
                (gnc:account-commit-edit new-acct)
                (gnc:insert-subaccount parent-acct new-acct))
              (begin 
                (if make-new-acct
                    (begin 
                      (gnc:account-set-name new-acct gnc-name)
                      (cond ((and (qif-acct? qif-info)
                                  (qif-acct:description qif-info))
                             (gnc:account-set-description 
                              new-acct (qif-acct:description qif-info)))
                            ((and (qif-cat? qif-info)
                                  (qif-cat:description qif-info))
                             (gnc:account-set-description 
                              new-acct (qif-cat:description qif-info)))
                            ((string? qif-info)
                             (gnc:account-set-description 
                              new-acct qif-info)))
                      (if gnc-type (gnc:account-set-type new-acct gnc-type))))
                
                (gnc:account-commit-edit new-acct)
                (gnc:group-insert-account acct-group new-acct)))
          (hash-set! gnc-acct-hash gnc-name new-acct)
          new-acct))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:qif-to-gnc 
;; this is the top-level of the back end conversion from 
;; QIF to GNC.  all the account mappings and so on should be 
;; done before this is called. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:qif-to-gnc qif-files-list mapping-data)
  (let* ((existing-gnc-accts (car mapping-data))
         (qif-acct-map (cadr mapping-data))
         (qif-cat-map (caddr mapping-data))
         (account-group (gnc:get-current-group))
         (gnc-acct-hash (make-hash-table 20))
         (existing-gnc-accounts #f)
         (sorted-qif-files-list 
          (sort qif-files-list 
                (lambda (a b)
                  (> (length (qif-file:xtns a)) 
                     (length (qif-file:xtns b)))))))
    
    ;; first, build a local account tree that mirrors the gnucash
    ;; accounts in the mapping data.  we need to iterate over the
    ;; cat-map and the acct-map, building the gnc-acct-hash as we go.
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (hashpair)
          (let* ((acctinfo (cdr hashpair))
                 (qif-name (list-ref acctinfo 0))
                 (gnc-name (list-ref acctinfo 1))
                 (gnc-type (list-ref acctinfo 2))
                 (gnc-new  (list-ref acctinfo 3))
                 (gnc-xtns (list-ref acctinfo 4))
                 (qif-info (list-ref acctinfo 5)))
            (if (> gnc-xtns 0)
                (qif-import:find-or-make-acct gnc-name gnc-acct-hash
                                              gnc-type qif-info 
                                              account-group))))
        bin))
     (vector->list qif-acct-map))
    
    (for-each 
     (lambda (bin)
       (for-each 
        (lambda (hashpair)
          (let* ((acctinfo (cdr hashpair))
                 (qif-name (list-ref acctinfo 0))
                 (gnc-name (list-ref acctinfo 1))
                 (gnc-type (list-ref acctinfo 2))
                 (gnc-new  (list-ref acctinfo 3))
                 (gnc-xtns (list-ref acctinfo 4))
                 (qif-info  (list-ref acctinfo 5)))
            (if (> gnc-xtns 0)
                (qif-import:find-or-make-acct gnc-name gnc-acct-hash
                                              gnc-type qif-info 
                                              account-group))))
        bin))
     (vector->list qif-cat-map))
    
    ;; iterate over files. Going in the sort order by number of 
    ;; transactions should give us a small speed advantage.
    (for-each 
     (lambda (qif-file)
       ;; within the file, iterate over transactions.  key things to
       ;; remember: if the L line in the transaction is a category,
       ;; it's a single-entry xtn and no need to look for the other
       ;; end.  if it's an account, search for a QIF file with that
       ;; account name and find the xtn to mark.
       (for-each 
        (lambda (xtn)
          (if (not (qif-xtn:mark xtn))
              (begin 
                ;; mark the transaction and find any other QIF 
                ;; xtns that refer to the same xtn 
                (qif-xtn:set-mark! xtn #t)
                (qif-import:mark-matching-xtns xtn qif-file qif-files-list)

                ;; create and fill in the GNC transaction
                (let ((gnc-xtn (gnc:transaction-create)))
                  (gnc:transaction-init gnc-xtn)
                  (gnc:transaction-begin-edit gnc-xtn 1) 

                  ;; destroy any automagic splits in the transaction
                  (let ((numsplits (gnc:transaction-get-split-count gnc-xtn)))
                    (if (not (eqv? 0 numsplits))
                        (let splitloop ((ind (- numsplits 1)))
                          (gnc:split-destroy 
                           (gnc:transaction-get-split gnc-xtn ind))
                          (if (> ind 0)
                              (loop (- ind 1))))))

                  ;; build the transaction
                  (qif-import:qif-xtn-to-gnc-xtn 
                   xtn qif-file gnc-xtn gnc-acct-hash mapping-data)
                  
                  ;; rebalance and commit everything
                  (gnc:transaction-commit-edit gnc-xtn)))))

        (qif-file:xtns qif-file)))
     sorted-qif-files-list)
    
    ;; now take the new account tree and merge it in with the 
    ;; existing gnucash account tree. 
    (gnc:merge-accounts account-group)
    (gnc:refresh-main-window)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; qif-import:qif-xtn-to-gnc-xtn
;; translate a single transaction to a set of gnucash splits and 
;; a gnucash transaction structure. 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:qif-xtn-to-gnc-xtn qif-xtn qif-file gnc-xtn 
                                       gnc-acct-hash mapping-data)
  (let ((splits (qif-xtn:splits qif-xtn))
        (qif-cat-map (caddr mapping-data))
        (qif-acct-map (cadr mapping-data))
        (near-acct-info #f)
        (near-acct-name #f)
        (near-acct #f))
    
    ;; set properties of the whole transaction     
    (apply gnc:transaction-set-date gnc-xtn (qif-xtn:date qif-xtn))
    
    (if (qif-xtn:payee qif-xtn)
        (gnc:transaction-set-description gnc-xtn (qif-xtn:payee qif-xtn)))
    (if (qif-xtn:number qif-xtn)
        (gnc:transaction-set-xnum gnc-xtn (qif-xtn:number qif-xtn)))

    ;; find the GNC account for the near end of the transaction 
    ;; (all splits have the same near end)
    (if (qif-xtn:bank-xtn? qif-xtn)
        (begin 
          (set! near-acct-info
                (hash-ref qif-acct-map 
                          (qif-file:account qif-file)))
          (set! near-acct-name 
                (list-ref near-acct-info 1))
          (set! near-acct (hash-ref gnc-acct-hash near-acct-name)))
        (begin 
          (set! near-acct-info 
                (hash-ref qif-acct-map 
                          (qif-xtn:security-name qif-xtn)))
          (set! near-acct-name 
                (list-ref near-acct-info 1))
          (set! near-acct (hash-ref gnc-acct-hash near-acct-name))))
    
    ;; iterate over QIF splits 
    (for-each 
     (lambda (qif-split)
       (let ((gnc-near-split (gnc:split-create))
             (gnc-far-split (gnc:split-create))
             (far-acct-info #f)
             (far-acct-name #f)
             (far-acct-type #f)
             (far-acct #f))
         
         ;; fill the splits in (near first).  This handles files in
         ;; multiple currencies by pulling the currency value from the
         ;; file import.
         (gnc:split-set-base-value gnc-near-split 
                                   (qif-split:amount qif-split)
                                   (qif-file:currency qif-file))
         (gnc:split-set-base-value gnc-far-split 
                                   (- (qif-split:amount qif-split))
                                   (qif-file:currency qif-file))

         (if (qif-split:memo qif-split)
             (begin 
               (gnc:split-set-memo gnc-near-split (qif-split:memo qif-split))
               (gnc:split-set-memo gnc-far-split (qif-split:memo qif-split))))
         
         ;; my guess is that you can't have Quicken splits 
         ;; on stock transactions.  This will break if you can. 
         (if (qif-xtn:share-price qif-xtn)
             (begin 
               (if (> (length splits) 1) 
                   (begin 
                     (display "qif-import:qif-xtn-to-gnc-xtn : ")
                     (display "splits in stock transaction!") (newline)))
               (gnc:split-set-share-price gnc-near-split 
                                          (qif-xtn:share-price qif-xtn))
               (gnc:split-set-share-price gnc-far-split 
                                          (qif-xtn:share-price qif-xtn)))
             (begin 
               (gnc:split-set-share-price gnc-near-split 1.0)
               (gnc:split-set-share-price gnc-far-split 1.0)))
         
         (if (qif-xtn:num-shares qif-xtn)
             (begin 
               (if (> (length splits) 1) 
                   (begin 
                     (display "qif-import:qif-xtn-to-gnc-xtn : ")
                     (display "splits in stock transaction!") (newline)))
                                 
               (gnc:split-set-share-amount gnc-near-split 
                                           (qif-xtn:num-shares qif-xtn))
               (gnc:split-set-share-amount gnc-far-split 
                                           (- (qif-xtn:num-shares qif-xtn)))))
         
         ;; find the GNC account on the far end of the split 
         (cond 
          ;; this is a stock xtn with no specified category, which 
          ;; generally means this account is a brokerage account 
          ;; description.
          ((and (not (qif-xtn:bank-xtn? qif-xtn))
                (string=? (qif-split:category qif-split) ""))
           (set! far-acct-info
                 (hash-ref qif-acct-map 
                           (qif-file:account qif-file)))
           (set! far-acct-name 
                 (list-ref far-acct-info 1))
           (set! far-acct (hash-ref gnc-acct-hash far-acct-name)))

          ;; this is a normal stock or bank transfer to another 
          ;; account 
          ((qif-split:category-is-account? qif-split)
           (set! far-acct-info
                 (hash-ref qif-acct-map 
                           (qif-split:category qif-split)))
           (set! far-acct-name 
                 (list-ref far-acct-info 1))
           (set! far-acct (hash-ref gnc-acct-hash far-acct-name)))
         
          ;; otherwise the category is a category and won't have a 
          ;; matching split in the QIF world.
          (#t
           (set! far-acct-info
                 (hash-ref qif-cat-map 
                           (qif-split:category qif-split)))
           (set! far-acct-name 
                 (list-ref far-acct-info 1))
           (set! far-acct (hash-ref gnc-acct-hash far-acct-name))))
         
         ;; finally, plug the splits into the accounts 
         (gnc:transaction-append-split gnc-xtn gnc-near-split)
         (gnc:transaction-append-split gnc-xtn gnc-far-split)
         (gnc:account-insert-split near-acct gnc-near-split)
         (gnc:account-insert-split far-acct gnc-far-split)))
     
     splits)
    
    ;; return the modified transaction (though it's ignored).
    gnc-xtn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-import:mark-matching-xtns 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-import:mark-matching-xtns xtn qif-file qif-files)
  (for-each 
   (lambda (split)
     (if (not (qif-split:mark split))
         (if (qif-split:category-is-account? split)
             (begin                
               (qif-split:set-mark! split #t)
               (qif-import:mark-matching-split split xtn qif-file qif-files))
             (qif-split:set-mark! split #t))))   
   (qif-xtn:splits xtn))
  (qif-xtn:set-mark! xtn #t))

(define (qif-import:mark-matching-split split xtn qif-file qif-files)
  (let ((near-acct-name #f)
        (far-acct-name (qif-split:category split))
        (date (qif-xtn:date xtn))
        (amount (- (qif-split:amount split)))
        (memo (qif-split:memo split))        
        (bank-xtn? (qif-xtn:bank-xtn? xtn))
        (done #f))

    (if bank-xtn?
        (set! near-acct-name (qif-file:account qif-file))
        (set! near-acct-name (qif-xtn:security-name xtn)))
    

;;     (display "mark-matching-split : near-acct = ")
;;     (write near-acct-name)
;;     (display " far-acct = ") 
;;     (write far-acct-name)
;;     (display " date = ")
;;     (write date)
;;     (newline)

    ;; this is the grind loop.  Go over every unmarked split of every
    ;; unmarked transaction of every file that's not this one.
    (let file-loop ((files qif-files))
      (if (and (not (eq? qif-file (car files)))
               (or (not bank-xtn?)
                   (string=? far-acct-name 
                             (qif-file:account (car files)))))
          (let xtn-loop ((xtns (qif-file:xtns (car files))))
            (if (not (qif-xtn:mark (car xtns)))
                (let split-loop ((splits (qif-xtn:splits (car xtns))))
                  (if (qif-split:split-matches?
                       (car splits) (car xtns) 
                       near-acct-name date amount memo)
                      (begin
;;                        (display "found ")(write (car splits))(newline)
                        (qif-split:set-mark! (car splits) #t)
                        (set! done #t)
                        (let ((all-marked #t))
                          (for-each 
                           (lambda (s) (if (not (qif-split:mark s))
                                           (set! all-marked #f)))
                           (qif-xtn:splits (car xtns)))
                          (if all-marked (qif-xtn:set-mark! 
                                          (car xtns) #t)))))
                  (if (and (not done)
                           (not (null? (cdr splits))))
                      (split-loop (cdr splits)))))
            (if (and (not done)
                     (not (null? (cdr xtns))))
                (xtn-loop (cdr xtns)))))
      (if (and (not done)
               (not (null? (cdr files))))
          (file-loop (cdr files))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  qif-split:split-matches? 
;;  check if a split matches date, amount, and other criteria
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (qif-split:split-matches? split xtn acct-name date amount memo)
  (and 
   ;; account name matches 
   (string=? acct-name (qif-split:category split))
   
   ;; is the amount right? 
   (eqv? amount (qif-split:amount split))
   
   ;; is the date the same?
   (let ((self-date (qif-xtn:date xtn)))
     (and (pair? self-date)
          (pair? date)
          (eq? (length self-date) 3)
          (eq? (length date) 3)
          (eqv? (car self-date) (car date))
          (eqv? (cadr self-date) (cadr date))
          (eqv? (caddr self-date) (caddr date))))
  
   ;; is the memo the same? (is this true?)
   ;; ignore it for now
   ))



