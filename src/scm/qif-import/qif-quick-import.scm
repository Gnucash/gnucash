;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-quick-import.scm
;;;  import 1 file, accepting all defaults and not modifying 
;;;  saved preferences
;;;
;;;  Copyright 2001 Bill Gribble <grib@billgribble.com> 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-import/qif-merge-groups.scm")
(gnc:depend  "report-utilities.scm")

(define (qif-import:quick-import qif-filename) 
  (false-if-exception 
   (call-with-current-continuation
    (lambda (exit)

      ;; read the file 
      (let ((read-success (qif-file:read-file qif-file filename)))
        (if (not read-success)
            (exit (cons #f "An error occurred while reading the QIF file."))))
      
      ;; parse fields.  Take the default (first) date format if the
      ;; file is ambiguous -- FIXME
      (let ((parse-success (qif-file:parse-fields qif-file)))
        (if (not parse-success)
            (exit (cons #f "An error occurred while parsing the QIF file.")))
        (if (and (list? parse-success)
                 (car parse-success))
            (qif-import:reparse-dates qif-file (caadr parse-return))))
      
      ;; load the account, category, and stock map information.
      
      ;; load into a GNC account group 

      ;; check for duplicate transactions 
      
      ;; catenate and merge the transactions
      ))))
      diff -u 'tmp/gnucash/src/scm/qif-import/qif-to-gnc.scm' 'gnucash-1.5/src/scm/qif-import/qif-to-gnc.scm'
