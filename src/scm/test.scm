
(gnc:support "test.scm")

(define (gnc:test-load group)
  (let ((cash
	 (list (gnc:malloc-account)
	       "Sample Cash"
	       "Sample Cash Description"
	       "No notes - this is just a sample"
	       1))
	(inc1
	 (list (gnc:malloc-account)
	       "Misc Income"
	       "Miscellaneous Income"
	       "Just a dumb income account"
	       8))
	(exp1
	 (list (gnc:malloc-account)
	       "Misc Exp"
	       "Miscellaneous Expenses"
	       "Just a dumb expense account"
	       9)))
    (display "Samples: ") (newline)
    (display (list cash inc1 exp1)) (newline)
    (apply gnc:create-account cash)
    (apply gnc:create-account inc1)
    (apply gnc:create-account exp1)
    (display "group:") (display group) (newline)
    (gnc:group-insert-account group (car cash))
    (gnc:group-insert-account group (car inc1))
    (gnc:group-insert-account group (car exp1))
    (gnc:refresh-main-window))
  (display "Tried creation")(newline))
