;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  print-check.scm
;;;  print a check from a transaction. 
;;;
;;;  Copyright 2000 Bill Gribble <grib@billgribble.com>
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "print-check.scm")
(gnc:depend  "number-to-words.scm")

;; format notes (I found a GIF of the check form and am measuring from
;; that, so this is definitely not perfect) positions are lower-left
;; text origin, (0,0) at lower left of page, in inches, for
;; US-Letter format paper. 
(define quicken-check-3up-at-top-us-letter
  '((payee . (1.25 9.5625))
    (amount-words . (1.25 9.1875))
    (amount-number . (7.0 9.625))
    (date . (7.0 10.0625))
    (memo . (0.75 8.0625))))

(define (gnc:print-check payee amount date memo)
  (let* ((int-part (inexact->exact (truncate amount)))
         (frac-part (inexact->exact 
                     (truncate 
                      (+ (/ .5 100) (* 100 (- amount int-part))))))
         (ps (gnc:print-session-create))
         (format quicken-check-3up-at-top-us-letter)
         (inches-to-points 
          (lambda (inches)
            (* inches 72))))

    (let ((date-pos (assq 'date format)))
      (gnc:print-session-moveto ps 
                                (inches-to-points (cadr date-pos))
                                (inches-to-points (caddr date-pos)))
      (gnc:print-session-text ps date))

    (let ((payee-pos (assq 'payee format)))
      (gnc:print-session-moveto ps 
                                (inches-to-points (cadr payee-pos))
                                (inches-to-points (caddr payee-pos)))
      (gnc:print-session-text ps payee))

    (let ((number-pos (assq 'amount-number format)))
      (gnc:print-session-moveto ps 
                                (inches-to-points (cadr number-pos))
                                (inches-to-points (caddr number-pos)))
      (gnc:print-session-text ps (printable-value amount 100)))

    (let ((words-pos (assq 'amount-words format)))
      (gnc:print-session-moveto ps 
                                (inches-to-points (cadr words-pos))
                                (inches-to-points (caddr words-pos)))
      (gnc:print-session-text ps (number-to-words amount 100)))

    (gnc:print-session-done ps)       
    (gnc:print-dialog-create ps)))
