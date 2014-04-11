;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  print-check.scm
;;;  print a check from a transaction. 
;;;
;;;  Copyright 2000 Bill Gribble <grib@billgribble.com>
;;;  June 2004 - D. Reiser - added capability to print wallet checks
;;;                          with left-side stubs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (gnucash printing number-to-words))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <print-check-format> class
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <print-check-format>
  (make-simple-class 
   'print-check-format
   '(format 
     position
     date-format
     custom-info)))

(define print-check-format?
  (record-predicate <print-check-format>))

(define print-check-format:format
  (simple-obj-getter <print-check-format> 'format))

(define print-check-format:set-format!
  (simple-obj-setter <print-check-format> 'format))

(define print-check-format:position
  (simple-obj-getter <print-check-format> 'position))

(define print-check-format:set-position!
  (simple-obj-setter <print-check-format> 'position))

(define print-check-format:date-format
  (simple-obj-getter <print-check-format> 'date-format))

(define print-check-format:set-date-format!
  (simple-obj-setter <print-check-format> 'date-format))

(define print-check-format:custom-info
  (simple-obj-getter <print-check-format> 'custom-info))

(define print-check-format:set-custom-info!
  (simple-obj-setter <print-check-format> 'custom-info))
   
(define (make-print-check-format fmt pos dateformat cust)
  (let ((retval (make-simple-obj <print-check-format>)))
    (print-check-format:set-format! retval fmt)
    (print-check-format:set-position! retval pos)
    (print-check-format:set-date-format! retval dateformat)
    (print-check-format:set-custom-info! retval cust)
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  stock formats 
;;  units for stock formats and positions are points (72/inch)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gnc:*stock-check-formats*
  '((deluxe  . ((payee . (126.0 147.0))
		(amount-words . (90.0 125.0))
		(amount-number . (395.0 147.0))
		(date . (343.0 178.0))
		(memo . (100.0 73.0))
		(rotate . 90.0)
		(translate . (232.0 300.0))
		(offset . 0.0)))                ;;declaration of offset preempts top/middle/bottom dialog choice
	 (quicken  . ((payee . (90.0 150.0))
		 (amount-words . (90.0 120.0))
		 (amount-number . (500.0 150.0))
		 (date . (500.0 185.0))
		 (memo . (50.0 40.0))
		 (top . 540.0)
		 (middle . 288.0)
		 (bottom . 36.0)))
    (wallet . ((payee . (231.0 140.0))  ;;these coord. for placement above amount-word line
                                        ;;use 202.0 94.0 for placement in address area
		 (amount-words . (195.0 125.0))
		 (amount-number . (518.0 137.0))
		 (date . (504.0 151.0))
		 (memo . (216.0 37.0))
		 (date-stub . (36.0 151.0))
		 (payee-stub . (28.0 126.0))
		 (amount-stub . (50.0 90.0))
		 (memo-stub . (28.0 65.0))
		 (top . 588.0)
		 (middle . 384.0)
		 (bottom . 180.0)))
	(custom . ((top . 540.0)  ;;set default perforation location for custom print layout
	     (middle . 288.0)
	     (bottom . 36.0))) 
    ))

(define (gnc:print-check format-info payee amount date memo)
  (let* ((int-part (inexact->exact (truncate amount)))
	 (frac-part (inexact->exact 
                     (truncate 
                      (+ (/ .5 100) (* 100 (- amount int-part))))))
	 (ps (gnc-print-session-create #t))
	 (format #f)
	 (offset #f)
	 (date-string "")
	 (payee-stub-text "")
	 (memo-stub-text ""))

    (if (not (null? ps))
     (begin
      (if (not (eq? (print-check-format:format format-info) 'custom))
          (begin 
            (set! format (assq (print-check-format:format format-info) 
                               gnc:*stock-check-formats*))
            (if (pair? format)
		(begin
		  (set! format (cdr format))
		  (let ((off (assq 'offset format)))
		    (if off (set! offset (cdr off)))))))
          (set! format (print-check-format:custom-info format-info)))
      
      (if (not (eq? (print-check-format:format format-info) 'custom))
          (begin 
	    (if (not (or offset (eq? (print-check-format:position format-info) 'custom)))
		(begin 
		  (set! offset 
			(cdr (assq (print-check-format:position format-info)
				   (cdr (assq (print-check-format:format format-info)
					      gnc:*stock-check-formats*)))))
		  (if (pair? offset)
		      (set! offset (cdr offset))))
		(set! offset
		      (caddr (assq 'translate 
				   (print-check-format:custom-info format-info))))))
	  (set! offset 0.0))
      
      (let ((fmt (print-check-format:date-format format-info)))
	(begin 
	  (set! date-string (strftime fmt (localtime date)))))

      (display "offset is ") (display offset) (newline)
      (let ((translate-pos (assq 'translate format)))
	(if translate-pos
	    (begin
	      (display "translate by ") (display (cadr translate-pos))
	      (display " ") (display (caddr translate-pos)) (newline)
	      (gnc-print-session-translate ps (cadr translate-pos)
					   (caddr translate-pos)))))

      (let ((rotate-angle (assq 'rotate format)))
	(if rotate-angle (gnc-print-session-rotate ps (cdr rotate-angle))))

      (let ((date-pos (assq 'date format)))
        (gnc-print-session-moveto ps (cadr date-pos)
                                  (+ offset (caddr date-pos)))
        (gnc-print-session-text ps date-string))
      
      (let ((payee-pos (assq 'payee format)))
        (gnc-print-session-moveto ps (cadr payee-pos)
                                  (+ offset (caddr payee-pos)))
        (gnc-print-session-text ps payee))
      
      (let ((number-pos (assq 'amount-number format)))
        (gnc-print-session-moveto ps (cadr number-pos)
                                  (+ offset (caddr number-pos)))
        (gnc-print-session-text ps (printable-value amount 100)))
      
      (let ((words-pos (assq 'amount-words format)))
        (gnc-print-session-moveto ps (cadr words-pos)
                                  (+ offset (caddr words-pos)))
        (gnc-print-session-text ps (number-to-words amount 100)))

      (if (not (eq? (print-check-format:format format-info) 'wallet))
        (let ((memo-pos (assq 'memo format)))
          (gnc-print-session-moveto ps (cadr memo-pos)
                                    (+ offset (caddr memo-pos)))
          (gnc-print-session-text ps memo)))

      (if (eq? (print-check-format:format format-info) 'wallet)
        (begin
           (let ((memo-pos (assq 'memo format)))
             (gnc-print-session-moveto ps (cadr memo-pos)
                                  (+ offset (caddr memo-pos)))
             (if (< (string-length memo) 28)
	          (gnc-print-session-text ps memo)
	          (gnc-print-session-text ps (substring memo 0 27))))
           (let ((memostub-pos (assq 'memo-stub format)))
             (gnc-print-session-moveto ps (cadr memostub-pos)
                                  (+ offset (caddr memostub-pos)))
             (if (< (string-length memo) 22)
	            (set! memo-stub-text memo)
	            (set! memo-stub-text (substring memo 0 20)))
             (gnc-print-session-text ps memo-stub-text))
        
           (let ((datestub-pos (assq 'date-stub format)))
              (gnc-print-session-moveto ps (cadr datestub-pos)
                                  (+ offset (caddr datestub-pos)))
             (gnc-print-session-text ps date-string))
        
           (let ((payeestub-pos (assq 'payee-stub format)))
                (gnc-print-session-moveto ps (cadr payeestub-pos)
                                  (+ offset (caddr payeestub-pos)))
              (if (< (string-length payee) 22)
	            (set! payee-stub-text payee)
	            (set! payee-stub-text (substring payee 0 20)))
             (gnc-print-session-text ps payee-stub-text))
        
           (let ((amountstub-pos (assq 'amount-stub format)))
             (gnc-print-session-moveto ps (cadr amountstub-pos)
                                  (+ offset (caddr amountstub-pos)))
             (gnc-print-session-text ps (printable-value amount 100)))))

      (gnc-print-session-done ps)))))

