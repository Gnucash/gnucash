;;; $Id$
(define oklist #f)
(define errorlist #f)
(define errcount #f)

(define (initialize-testing)
  (set! oklist '())
  (set! errorlist '())
  (set! errcount 0))

(define (testing funname input expected actual)
  (define (lookup-set! lookuptable key value)
    (let 
	((oldval (assoc key lookuptable)))
      (if oldval
	  (set-cdr! oldval value)
	  (set! lookuptable (cons (cons key value) lookuptable))))
    lookuptable)
  
  (if testing?
      (begin
	(display (string-append "Test: ("  funname " "))
	(display input)
	(display ")") (newline)
	(display "Expect: ") (display expected) (newline)
	(display "Got: ") (display actual) (newline)
	(let ((result (list funname input expected actual)))
	  (if (equal? expected actual)
	      (begin
		(display "OK")
		(set! oklist (lookup-set! oklist (list funname input) result)))
	      (begin
		(display "ERROR!!!!!!!!!")
		(set! errorlist (lookup-set! errorlist 
					     (list funname input) 
					     (list expected result))))))
	(newline))))

(define (reportonerrors)
  (newline)
  (display "Error Analysis:") (newline)
  (display "---------------------------") (newline)
  (display "Number Passed:") 
  (display (number->string (length (map car oklist)))) (newline)
  (display "Number Failed:") 
  (display (number->string (length (map car errorlist)))) (newline)

  (map 
   (lambda (lst)
     (display "Error:") (newline)
     (let* ((key (car lst))
	    (funname (car key))
	    (input (cadr key))
	    (value (cdr lst))
	    (expected (car value))
	    (actual (cadr value)))
       (display "-------------------------------------------") (newline)
       (display "Function:") (display funname) (newline)
       (display "Input:") (display input) (newline)
       (display "Expected result:") (display expected) (newline)
       (display "Actual result:") (display actual) (newline)
       (display "-------------------------------------------") (newline)
       )) 
   errorlist)
  (newline))
