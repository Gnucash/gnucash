(define (gnc:create-account AccPtr name description notes type)
  (display "start creation")(newline)

  (gnc:account-begin-edit AccPtr 0)
  (display "edit")(newline)

  (display (string-append "Name:" name)) (newline)
  (gnc:account-set-name AccPtr name)

  (display (string-append "Descr:" description)) (newline)
  (gnc:account-set-description AccPtr description)

  (display (string-append "notes:" notes)) (newline)
  (gnc:account-set-notes AccPtr notes)

  (display (string-append "Type:" (number->string type))) (newline)

  (gnc:account-set-type AccPtr type)

  (gnc:account-commit-edit AccPtr)
  (display "committed")(newline)
  )

(define (gnc:test-creation)
  (let ((group (gnc:get-current-group))
	(cash
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
