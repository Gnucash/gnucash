(define (gnc:create-account AccPtr  name description notes type)
  (display "start creation")(newline)
  (gnc:xaccAccountBeginEdit AccPtr 0)
  (display "edit")(newline)
  (display (string-append "Name:" name)) (newline)
  (gnc:xaccAccountSetName AccPtr name)
  (display (string-append "Descr:" description)) (newline)
  (gnc:xaccAccountSetDescription AccPtr description)
  (display (string-append "notes:" notes)) (newline)
  (gnc:xaccAccountSetNotes AccPtr notes)
  (display (string-append "Type:" (number->string type))) (newline)
  (gnc:xaccAccountSetType AccPtr type)
  (gnc:xaccAccountCommitEdit AccPtr)
  (display "committed")(newline)
  )

(display "Create some accounts:")(newline)
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
  (apply gnc:create-account exp1))
(display "Tried creation")(newline)

