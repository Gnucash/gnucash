;;; $Id$
(gnc:support "qifs/qifstate.scm")
;;;;; - Transactions should not be marked off as being finally reconciled on
;;;;;   the GnuCash side, as the reconciliation hasn't been done there.  
;;;;;
;;;;;   Bad Things would happen if we double-load a batch of QIF transactions, 
;;;;;   and treat it as if it were fully reconciled.

;;;;;  This returns the "thunk" that should be used to translate statuses
(define (status-handling qif-txn-list)
  (define cleared? #f)
  (define (look-for-cleared txn)
    (if
     (string=? "X" (cdr (assoc 'status txn)))
     (set! cleared #t)))
  (for-each look-for-cleared qif-txn-list)
  (if cleared?
      (begin
	(display "Warning:  This transaction list includes transactions marked as cleared.")
	(display "Are you *completely* confident of the correctness of that")
	(display "reconciliation, and that it is *truly* safe to mark them as reconciled")
	(display "in GnuCash?")
	(display "It is suggested that you indicate ``No,'' which will result in those")
	(display "transactions being statused as ``marked,'' which should make the")
	(display "reconciliation in GnuCash take place reasonably quickly.")
        ;;;; Now ask if the user is certain...
	;;;; Need some code here...
	(let ((certain? (lambda () #f)))
	  (set! cleared (certain?)))))
  (let* 
      ((cleared-to-what (if cleared? 'cleared 'marked))
	   (ttable
;;;  QIF Status translation table
;;;  The CARs are values expected from Quicken.
;;;  The CDRs are the values that gnc:transaction-put-status requires...
	    '(("X" cleared-to-what)
	      ("*" 'marked)
	      ("?" 'budgeted-new)
	      ("!" 'budgeted-old)
	      (""  'unmarked))))

;;;  And here's the "thunk" that is to be returned.  It translates QIF statuses
;;;  into the form GnuCash expects to pass to gnc:transaction-put-status
    (lambda (status)
      (let
	  ((a (assoc status ttable)))
	(if
	 a
	 (cdr a)                  ;;; If the value was found, use it..
	 (cdr (assoc "" ttable))))))) ;;; No value?  Take the null value from ttable

(if testing?
    (begin (display "Need tests for qifstat.scm") (newline)));;; $Id$
(define qifstate #f)

(define (newqifstate line)
  (let*
      ((QIFstates
	'(("!Type:Cat" . category)
	  ("!Type:Class" . class)   ;;; Additional classification feature
	  ("!Option:AutoSwitch" . accounts)
	  ("!Clear:AutoSwitch"  . accounts)
	  ("!Account" . accounts)
	  ("!Type:Memorized" . memorized)
	  ("!Type:Bank" . txn)
	  ("!Type:CCard" . txn)
	  ("!Type:Oth A" . txn)))
       (name (striptrailingwhitespace line))
       (statepair (assoc name QIFstates)))
    (if (pair? statepair)
	(begin
	  (display "New qifstate:") (display (cdr statepair))
	  (newline)
	  (set! qifstate (cdr statepair))
	  (cdr statepair))
	(begin
	  (display "No new QIF state") (newline)
	  #f))))

(testing "newqifstate"
	 "!Account"
	 'accounts
	 (newqifstate "!Account"))

(testing "newqifstate"
	 "!Type:Cat "
	 'category
	 (newqifstate "!Type:Cat"))

(testing "newqifstate"
	 "nothing"
	 #f
	 (newqifstate "nothing"))
