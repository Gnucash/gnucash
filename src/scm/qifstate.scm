;;; $Id$
(define qifstate #f)

(define (newqifstate line)
  (let*
      ((QIFstates
	'(("!Type:Cat" . category)
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
	  (display "No new QIF state") (newline)))))

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
