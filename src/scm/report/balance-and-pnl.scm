;; -*-scheme-*-

(use-modules (ice-9 slib))
(require 'stdio)

(gnc:depend "text-export.scm")

(let ()
  ;; Just a private scope.

  (define (render-level-2-account level-2-account level-2-balance)
    (let ((account-name (gnc:account-get-name level-2-account))
          (type-name (gnc:account-get-type-string
                      (gnc:account-get-type level-2-account))))
      (string-append
       "<tr><td>" account-name "<td>" type-name
       (sprintf #f "<td align=right nowrap>&nbsp;$%10.2f\n"
                level-2-balance))))

  (define (render-level-1-account account level-1-balance level-2-balance)
    (let ((name (gnc:account-get-name account))
          (type (gnc:account-get-type-string (gnc:account-get-type account))))
      (string-append
       "<tr><td>" name "<td>" type
       (sprintf #f "<td align=right nowrap>&nbsp;$%10.2f" level-2-balance)
       (sprintf #f "<td align=right nowrap>&nbsp;&nbsp;<u>$%10.2f</u> \n"
                level-1-balance)
       "<tr><td>&nbsp;<td>&nbsp;<td>&nbsp;\n")))   ;; blank line

  (define (render-total level-0-balance)
    (string-append
     "<tr><td>&nbsp;<td>&nbsp;<td>&nbsp;\n"   ;; blank line
     "<tr><td><b>Net</b><td>&nbsp;"
     "<td>&nbsp;"
     (sprintf #f "<td align=right nowrap>&nbsp;&nbsp;<u>$%10.2f</u> \n"
              level-0-balance)))

  (define (generate-balance-sheet-or-pnl report-name
                                         report-description
                                         balance-sheet?)

    ;; currency symbol that is printed is a dollar sign, for now
    ;; currency amounts get printed with two decimal places
    ;; balance sheet doesn't print income or expense
    ;; top-level accounts get printed in right-most column

    ;; This code could definitely be more "schemy", but for now I mostly
    ;; just translated it directly from the old ePerl with a few
    ;; schemifications.

    (let ((level-0-balance 0)
          (level-1-balance 0)
          (level-2-balance 0)
          (current-group (gnc:get-current-group))
          (output '()))

      (define (handle-level-2-account account)
        (let ((type (gnc:account-type->symbol (gnc:account-get-type account)))
              (balance (gnc:account-get-balance account)))
          
          (if (not balance-sheet?) (set! balance (- balance)))
          
          (if (not (or (and balance-sheet?
                            (not (eq? type 'INCOME))
                            (not (eq? type 'EXPENSE)))
                       (and (not balance-sheet?)
                            (or (eq? type 'INCOME)
                                (eq? type 'EXPENSE)))))
              ;; Ignore
              '()

              ;; add in balances for any sub-sub groups
              (let ((grandchildren (gnc:account-get-children account)))
                
                (if (not (pointer-token-null? grandchildren))
                    (set! balance
                          ((if balance-sheet? + -)
                           balance (gnc:group-get-balance grandchildren))))

                (set! level-2-balance (+ level-2-balance balance))
                (set! level-1-balance (+ level-1-balance level-2-balance))
                (let ((result (render-level-2-account account level-2-balance)))
                  (set! level-2-balance 0)
                  result)))))

      (define (handle-level-1-account account)
        (let ((type (gnc:account-type->symbol (gnc:account-get-type account))))

          (if (not (or (and balance-sheet?
                            (not (eq? type 'INCOME))
                            (not (eq? type 'EXPENSE)))
                       (and (not balance-sheet?)
                            (or (eq? type 'INCOME)
                                (eq? type 'EXPENSE)))))

              ;; Ignore
              '()

              (let ((childrens-output (gnc:group-map-accounts
                                       handle-level-2-account
                                       (gnc:account-get-children account)))
                    (account-balance (gnc:account-get-balance account)))

                (if (not balance-sheet?)
                    (set! account-balance (- account-balance)))

                (set! level-2-balance (+ level-2-balance account-balance))
                (set! level-1-balance (+ level-1-balance account-balance))
                (set! level-0-balance (+ level-0-balance level-1-balance))

                (let ((level-1-output (render-level-1-account account
                                                              level-1-balance
                                                              level-2-balance)))
                  (set! level-1-balance 0)
                  (set! level-2-balance 0)
                  (list childrens-output level-1-output))))))

      (if (not (pointer-token-null? current-group))
          (set! output
                (list
                 (gnc:group-map-accounts handle-level-1-account current-group)
                 (render-total level-0-balance))))

      (list
       "<html>"
       "<head>"
       "<title>" report-name "</title>"
       "</head>"

       "<body bgcolor=#ccccff>"
       report-description
       "<p>"

       "<table cellpadding=1>"
       "<caption><b>" report-name "</b></caption>"
       "<tr><th>Account Name<th align=center>Type<th> <th align=center>Balance"

       output

       "</table>"
       "</body>"
       "</html>")))


  (gnc:define-report
   ;; version
   1
   ;; Menu name
   "Balance sheet"
   ;; Options (none currently)
   #f
   ;; Code to generate the report   
   (lambda (options)
     (generate-balance-sheet-or-pnl "Balance Sheet"
                                    "This page shows your net worth."
                                    #t)))

  (gnc:define-report
   ;; version
   1
   ;; Menu name
   "Profit and Loss"
   ;; Options (none currently)
   #f
   ;; Code to generate the report   
   (lambda (options)
     (generate-balance-sheet-or-pnl "Profit and Loss"
                                    "This page shows your profits and losses."
                                    #f))))
