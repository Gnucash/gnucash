;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  number-to-words.scm
;;;  convert a number into a sentence for check printing
;;;
;;;  Copyright 2000 Bill Gribble <grib@billgribble.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash printing number-to-words))
(export integer-to-words)
(export printable-value)
(export number-to-words)

(define (integer-to-words val)
  (let ((current-string "")
        (small-numbers
         #("zero" "one" "two" "three" "four" "five" 
           "six" "seven" "eight" "nine" "ten"
           "eleven" "twelve" "thirteen" "fourteen" "fifteen"
           "sixteen" "seventeen" "eighteen" "nineteen" "twenty"))
        (medium-numbers
         #("zero" "ten" "twenty" "thirty" "forty" "fifty"
           "sixty" "seventy" "eighty" "ninety"))
        (big-numbers
          #("hundred" "thousand" "million" "billion" "trillion"
            "quadrillion" "quintillion")))
    (cond 
     ((< val 20)
      (vector-ref small-numbers val))

     ((< val 100)
      (let ((this-part (quotient val 10))
            (that-part (remainder val 10)))
        (set! current-string (vector-ref medium-numbers this-part))
        (if (> that-part 0) 
            (set! current-string 
                  (string-append current-string "-"
                                 (vector-ref small-numbers that-part))))
        current-string))
     ((< val 1000)
      (let ((this-part (quotient val 100))
            (that-part (remainder val 100)))      
        (set! current-string  
              (string-append current-string 
                             (vector-ref small-numbers this-part) " "
                             (vector-ref big-numbers 0)))
        (if (> that-part 0)
            (set! current-string 
                  (string-append current-string 
                                 " " (integer-to-words that-part))))
        current-string))
     (#t 
      (let* ((log-val (inexact->exact 
                       (truncate (+ .00001 (/ (log10 val) 3)))))
             (this-part (quotient val 
                                  (inexact->exact 
                                   (truncate 
                                    (+ .00001 (expt 10 (* 3 log-val)))))))
             (that-part (remainder val 
                                   (inexact->exact  
                                    (truncate 
                                     (+ .00001 (expt 10 (* 3 log-val))))))))
        (if (> this-part 0)
            (set! current-string 
                  (string-append (integer-to-words this-part) 
                                 " " (vector-ref big-numbers log-val))))
        (if (> that-part 0)            
            (set! current-string 
                  (string-append current-string 
                                 " " (integer-to-words that-part))))
        current-string)))))

;; return a string with the number properly truncated and zero padded
;; for check printing 
(define (printable-value val frac-denom)
  (let* ((int-part (inexact->exact (truncate val)))
         (frac-part (inexact->exact 
                     (truncate 
                      (+ (/ .5 frac-denom) (* frac-denom 
                                              (- val int-part)))))))
    (with-output-to-string
      (lambda ()
        (write int-part) (display ".") 
        (if (< frac-part 10) (display "0"))
        (write frac-part)))))
    

(define (number-to-words val frac-denom)  
  (let* ((negative? 
          (if (< val 0)
              (begin (set! val (- val))
                     #t)
              #f))
         (int-part (inexact->exact (truncate val)))
         (frac-part (inexact->exact 
                     (truncate 
                      (+ (/ .5 frac-denom) (* frac-denom (- val int-part))))))
         (result-string ""))
    (set! result-string 
          (string-append (integer-to-words int-part) " and "
                         (with-output-to-string 
                           (lambda ()
                             (write frac-part)
                             (display "/")
                             (write frac-denom)))))
    (string-set! result-string 0
                 (char-upcase (string-ref result-string 0)))
    result-string))
       

    
    
    

