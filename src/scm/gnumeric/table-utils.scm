(define (make-table-collector)
  (let          ;;; variable slots
      ((total 0)  ;;; Numeric total
       (rows '()) ;;; Collection of items into total
       (count 0)) ;;; Number of elements
    (let 
	((adder (lambda (amount pos)
		  (set! total (+ total amount))
		  (set! rows (cons pos rows))
		  (set! count (+ count 1))))
	 (gettotal (lambda () total))
	 (getcount (lambda () count))
	 (getrows (lambda () rows))
	 (resetall (lambda ()
		     (set! total 0)
		     (set! rows '())
		     (set! count 0))))
      (lambda (action value . rowdata)
	(case action
	  ('add (adder value rowdata))
	  ('total (gettotal))
	  ('getcount (getcount))
	  ('getrows (getrows))
	  ('reset (resetall)))))))

;;; Here's how it looks:
; > (define a (make-table-collector))
; > (a 'add 2)
; > (a 'add 4 5 6)
; > (a 'add 6 7 8)
; > (a 'add 9 10)
; > (a 'getcount #f)
; 4
; > (a 'total #f)
; 21
; > (a 'getrows #f)
; ((10) (7 8) (5 6) ())
; > (a 'reset #f)
; > (list (a 'getcount #f) (a 'total #f) (a 'getrows #f))
; (0 0 ())
; >

