;; this file is meant to be run via the gnucash-cli interface: --script simple-book-add-txn.scm
;;
;; gnucash-cli book.gnucash --script simple-book-add-txn.scm
;;

(use-modules (gnucash core-utils))
(use-modules (gnucash engine))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (ice-9 match))

(define iso-date (qof-date-format-get-string QOF-DATE-FORMAT-ISO))
(define book (gnc-get-current-book))
(define root (gnc-get-current-root-account))
(define query (qof-query-create-for-splits))
(qof-query-set-book query (gnc-get-current-book))
(xaccQueryAddAccountMatch query (gnc-account-get-descendants root) QOF-GUID-MATCH-ANY QOF-QUERY-AND)
(qof-query-set-sort-order
 query
 (list SPLIT-TRANS TRANS-DATE-POSTED)
 '()
 (list QUERY-DEFAULT-SORT))

(define (dump-transaction trans)
  (format #t "~a ~a\n"
          (gnc-print-time64 (xaccTransGetDate trans) iso-date)
          (xaccTransGetDescription trans))
  (define (split->account s)
    (gnc-account-get-full-name (xaccSplitGetAccount s)))
  (define (split->amount s)
    (format #f "~a ~a"
            (exact->inexact (xaccSplitGetAmount s))
            (gnc-commodity-get-mnemonic (xaccAccountGetCommodity (xaccSplitGetAccount s)))))
  (define max-width
    (let lp ((splits (xaccTransGetSplitList trans)) (maximum 0))
      (match splits
        (() (+ maximum 2))
        ((s . rest)
         (lp rest (max maximum (+ (string-length (split->account s))
                                  (string-length (split->amount s)))))))))
  (for-each
   (lambda (s)
     (define txn (xaccSplitGetParent s))
     (define acc-name (split->account s))
     (define amt-str (split->amount s))
     (format #t "  ~a~a~a\n"
             acc-name
             (make-string (- max-width (string-length acc-name) (string-length amt-str)) #\space)
             amt-str))
   (xaccTransGetSplitList trans)))

(define split-has-no-account? (compose null? xaccSplitGetAccount))

(let lp ((splits (xaccQueryGetSplitsUniqueTrans query)))
  (newline)
  (match splits
    (() #f)
    (((? split-has-no-account?) . rest) (lp rest))
    ((split . rest) (dump-transaction (xaccSplitGetParent split)) (lp rest))))

(qof-query-destroy query)
(gnc-clear-current-session)
