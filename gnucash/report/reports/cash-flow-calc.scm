;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cash-flow-calc.scm: Cash Flow in-out calculation.
;;
;; copyright 2015 Peter Broadberry
;; copyright 2019 Christopher Lam
;;
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
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-module (gnucash reports cash-flow-calc))
(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))

;; function to add inflow and outflow of money
(define-public (cash-flow-calc-money-in-out settings)
  (let* ((accounts (cdr (assq 'accounts settings)))
         (to-date-t64 (cdr (assq 'to-date-t64 settings)))
         (from-date-t64 (cdr (assq 'from-date-t64 settings)))
         (report-currency (cdr (assq 'report-currency settings)))
         (include-trading-accounts
          (cdr (assq 'include-trading-accounts settings)))
         (to-report-currency (cdr (assq 'to-report-currency settings)))
         (money-in '())
         (money-in-collector (gnc:make-commodity-collector))
         (money-out '())
         (money-out-collector (gnc:make-commodity-collector))
         (all-splits (gnc:account-get-trans-type-splits-interval
                      accounts '() from-date-t64 to-date-t64))
         (splits-to-do (length all-splits))
         (splits-seen-list '()))

    (let loop ((splits all-splits)
               (work-done 0))
      (unless (null? splits)
        (if (zero? (modulo work-done 100))
            (gnc:report-percent-done (* 85 (/ work-done splits-to-do))))
        (let* ((split (car splits))
               (parent (xaccSplitGetParent split)))
          (for-each
           (lambda (s)
             (let* ((s-account (xaccSplitGetAccount s))
                    (s-value (xaccSplitGetValue s))
                    (s-report-value (to-report-currency (xaccTransGetCurrency parent)
                                                        (abs s-value)
                                                        (xaccTransGetDate parent))))
               (cond
                ((null? s-account)
                 (format #t "WARNING: s-account is NULL for split: ~a\n"
                         (gncSplitGetGUID s)))
                ((or (and include-trading-accounts
                          (eqv? (xaccAccountGetType s-account)
                                ACCT-TYPE-TRADING))
                     (member s-account accounts)
                     (member s splits-seen-list))
                 #f)
                ((negative? s-value)
                 (let ((s-account-in-collector
                        (or (assoc-ref money-in s-account)
                            (let ((coll (gnc:make-commodity-collector)))
                              (set! money-in
                                (assoc-set! money-in s-account coll))
                              coll))))
                   (set! splits-seen-list (cons s splits-seen-list))
                   (money-in-collector 'add report-currency s-report-value)
                   (s-account-in-collector
                    'add report-currency s-report-value)))
                ((positive? s-value)
                 (let ((s-account-out-collector
                        (or (assoc-ref money-out s-account)
                            (let ((coll (gnc:make-commodity-collector)))
                              (set! money-out
                                (assoc-set! money-out s-account coll))
                              coll))))
                   (set! splits-seen-list (cons s splits-seen-list))
                   (money-out-collector 'add report-currency s-report-value)
                   (s-account-out-collector
                    'add report-currency s-report-value))))))
           (xaccTransGetSplitList parent)))
        (loop (cdr splits) (1+ work-done))))

    ;; Return an association list of results
    (list
     (cons 'money-in-accounts (map car money-in))
     (cons 'money-in-alist (map (lambda (p) (list (car p) (cdr p))) money-in))
     (cons 'money-in-collector money-in-collector)
     (cons 'money-out-accounts (map car money-out))
     (cons 'money-out-alist (map (lambda (p) (list (car p) (cdr p))) money-out))
     (cons 'money-out-collector money-out-collector))))

