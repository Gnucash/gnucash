;; -*-scheme-*-
;; transaction-report.scm
;; Report on all transactions in an account
;; Robert Merkel (rgmerk@mira.net)

(use-modules (ice-9 slib))
(require 'printf)

(gnc:depend "text-export.scm")

(define gnc:*transaction-report-options* '())

;;returns a list contains elements of the-list for which predictate is
;; true
(define (gnc:filter-list the-list predicate)
  (cond ((not (list? the-list))
         (gnc:error("Attempted to filter a non-list object")))
        ((null? the-list) '())
        ((predicate (car the-list))
         (cons (car the-list)
               (gnc:filter-list (cdr the-list) predicate)))
        (else (gnc:filter-list (cdr the-list) predicate))))

(define (gnc:register-trep-option new-option)
  (set! gnc:*transaction-report-options*
	(gnc:register-option gnc:*transaction-report-options* new-option))
  new-option)

;; from date
;; hack alert - could somebody set this to an appropriate date?
(define begindate
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "From"
    "a" "Report Items from this date" 
    (lambda ()
      (let ((bdtime (localtime (current-time))))
        (set-tm:sec bdtime 0)
        (set-tm:min bdtime 0)
        (set-tm:hour bdtime 0)
        (set-tm:mday bdtime 1)
        (set-tm:mon bdtime 0)
        (let ((time (car (mktime bdtime))))
          (cons time 0))))
    #f)))

;; to-date
(define enddate
  (gnc:register-trep-option
   (gnc:make-date-option
    "Report Options" "To"
    "b" "Report items up to and including this date"
    (lambda () (cons (current-time) 0))
    #f)))

;; account to do report on
;; hack alert - default setting doesn't work!

(define tr-report-account-op
  (gnc:register-trep-option
   (gnc:make-account-list-option
    "Report Options" "Account"
    "c" "Do transaction report on this account"
    (lambda () (list (gnc:group-get-account (gnc:get-current-group) 0)))
    #f #f)))

;; extract fields out of the scheme split representation

(define (gnc:tr-report-get-memo split-scm)
  (vector-ref split-scm 0))

(define (gnc:tr-report-get-action split-scm)
  (vector-ref split-scm 1))

(define (gnc:tr-report-get-description split-scm)
  (vector-ref split-scm 2))

(define (gnc:tr-report-get-date split-scm)
  (vector-ref split-scm 3))

(define (gnc:tr-report-get-reconcile-state split-scm)
  (vector-ref split-scm 4))

(define (gnc:tr-report-get-reconcile-date split-scm)
  (vector-ref split-scm 5))

(define (gnc:tr-report-get-share-amount split-scm)
  (vector-ref split-scm 6))

(define (gnc:tr-report-get-share-price split-scm)
  (vector-ref split-scm 7))

(define (gnc:tr-report-get-value split-scm)
  (vector-ref split-scm 8))

(define (gnc:tr-report-get-docref split-scm)
  (vector-ref split-scm 9))

(define (gnc:tr-report-get-other-splits split-scm)
  (vector-ref split-scm 10))

;;; something like 
;;; for(i = first; i < last; i+= step) { thunk(i);}

(define (gnc:for-loop thunk first last step)
  (cond ((< first last) (thunk first) 
	 (gnc:for-loop thunk (+ first step) last step))
	(else #f)))

;;; applies thunk to each split in account account
(define (gnc:for-each-split-in-account account thunk)
  (gnc:for-loop (lambda (x) (thunk (gnc:account-get-split account x)))
		0 (gnc:account-get-split-count account) 1))

(define (gnc:split-get-transaction-date split)
  (gnc:transaction-get-date-posted (gnc:split-get-parent split)))

(define (gnc:split-get-description-from-parent split)
  (gnc:transaction-get-description (gnc:split-get-parent split)))


(define (gnc:split-get-account-name split)  
  (gnc:account-get-name (gnc:split-get-account split)))

;; builds a list of the account name and values for the other
;; splits in a transaction
;; hack alert - lots of debugging cruft in here

(define (gnc:split-get-corresponding-account-name-and-values split) 
  (let* ((my-sign (positive? (gnc:split-get-value split)))
         (diff-list '())
         (parent-transaction (gnc:split-get-parent split))
         (num-splits (gnc:transaction-get-split-count parent-transaction)))
    (gnc:for-loop 
     (lambda (n) 
       (let ((split-in-trans 
              (gnc:transaction-get-split parent-transaction n)))
         (if (not (eq? my-sign 
                       (positive? (gnc:split-get-value split-in-trans))))
             (set! diff-list
                   (cons
                    (list
                     (gnc:split-get-account-name split-in-trans)
                     (gnc:split-get-value split-in-trans))
                    diff-list)))))
     0 num-splits 1)
    (reverse diff-list)))

;; takes a C split, extracts relevant data and converts to a scheme 
;; representation

(define (gnc:make-split-scheme-data split)
  (vector (gnc:split-get-memo split) 
	  (gnc:split-get-action split)
	  (gnc:split-get-description-from-parent split)
	  (gnc:split-get-transaction-date split)
	  (gnc:split-get-reconcile-state split)
	  (gnc:split-get-reconciled-date split)
	  (gnc:split-get-share-amount split)
	  (gnc:split-get-share-price split)
	  (gnc:split-get-value split)
	  (gnc:split-get-docref split)
	  (gnc:split-get-corresponding-account-name-and-values split)))

(define (gnc:timepair-to-datestring tp)
  (let ((bdtime (localtime (car tp))))
    (strftime "%x" bdtime)))

(define (gnc:timepair-earlier-or-eq t1 t2)
  (let ((time1 (car t1)) 
        (time2 (car t2)))
    (<= time1 time2)))

(define (gnc:timepair-later-or-eq t1 t2)
  (gnc:timepair-earlier-or-eq t2 t1))

(define (gnc:tr-report-make-filter-predicate early-date late-date)
  (lambda (split-scm)
    (let ((split-date (gnc:tr-report-get-date split-scm)))
      (and (gnc:timepair-later-or-eq split-date early-date)
           (gnc:timepair-earlier-or-eq split-date late-date)))))

;; converts a scheme split representation to a line of HTML,
;; updates the values of total-inflow and total-outflow based
;; on the split value

(define (gnc:tr-report-split-to-html split-scm total-inflow total-outflow
                                     starting-balance)
  (let ((other-splits (gnc:tr-report-get-other-splits split-scm)))
    (string-append 
     "<TR><TD>" 
     (gnc:timepair-to-datestring
      (gnc:tr-report-get-date split-scm))
     "</TD><TD>"
     (gnc:tr-report-get-docref split-scm)
     "</TD><TD>"
     (gnc:tr-report-get-description split-scm)
     "</TD><TD>"
     (gnc:tr-report-get-memo split-scm)
     "</TD><TD>"
     (cond ((null? other-splits) "")
           ((= (length other-splits) 1) (caar other-splits))
           (else "Multi-split (not implemented yet)"))
     "</TD><TD>"
     (cond ((> (gnc:tr-report-get-value split-scm) 0)
            (set! total-inflow (+ total-inflow 
                                  (gnc:tr-report-get-value split-scm)))
            (sprintf #f "%.2f" (gnc:tr-report-get-value split-scm)))
           (else 
            (set! total-outflow (+ total-outflow
                                   (- (gnc:tr-report-get-value split-scm))))
            (string-append 
             (sprintf #f "%.2f" 
                      (- (gnc:tr-report-get-value split-scm))))))
     "</TD></TR>")))

;; hack alert - stub for testing

(define (gnc:tr-report-get-starting-balance scm-split-list beginning-date)
  0)

(gnc:define-report
 ;; version
 1
 ;; Name
 "Account Transactions"
 ;; Options
 gnc:*transaction-report-options*
 ;; renderer
 (lambda (options)
   (let* ((prefix  (list "<HTML>" "<BODY bgcolor=#99ccff>" "<TABLE>"))
	  (suffix  (list "</TABLE>" "</BODY>" "</HTML>"))
	  (report-lines '())
	  (date-filter-pred (gnc:tr-report-make-filter-predicate
			     (op-value begindate) 
			     (op-value enddate)))
	  (total-inflow 0)
	  (total-outflow 0)
	  (starting-balance 0)
          (accounts (op-value tr-report-account-op))
	  (html-mapper (lambda (split-scm) (gnc:tr-report-split-to-html 
					    split-scm
					    total-inflow
					    total-outflow
					    starting-balance))))

     (if (null? accounts)
         (set! report-lines
               (list "<TR><TD>You have not selected an account.</TD></TR>"))
         (begin
           (gnc:for-each-split-in-account
            (car accounts)
            (lambda (split)		
              (set! report-lines 
                    (append! report-lines 
                             (list (gnc:make-split-scheme-data split))))))
           (set! starting-balance
                 (gnc:tr-report-get-starting-balance
                  report-lines (op-value begindate)))
           (set! report-lines (gnc:filter-list report-lines date-filter-pred))
           (set! report-lines (map html-mapper report-lines))))
     (append prefix report-lines suffix))))
