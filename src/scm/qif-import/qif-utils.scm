;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-utils.scm
;;;  string munging and other utility routines 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;  $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "qif-utils.scm")

(define (simple-filter pred list)
  (let ((retval '()))
    (map (lambda (elt)
           (if (pred elt)
               (set! retval (cons elt retval))))
         list)
    (reverse retval)))

(define remove-trailing-space-rexp 
  (make-regexp "^(.*[^ ]+) *$"))

(define remove-leading-space-rexp 
  (make-regexp "^ *([^ ].*)$"))

(define (string-remove-trailing-space str)
  (if (eq? (string-ref str (- (string-length str) 1)) #\cr)
      (string-set! str (- (string-length str) 1) #\space))

  (let ((match (regexp-exec remove-trailing-space-rexp str)))
    (if match
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-leading-space str)
  (let ((match (regexp-exec remove-leading-space-rexp str)))
    (if match 
        (string-copy (match:substring match 1))
        "")))

(define (string-remove-char str char)
  (let ((rexpstr 
         (case char  
           ((#\.) "\\.")
           ((#\^) "\\^")
           ((#\$) "\\$")
           ((#\*) "\\*")
           ((#\+) "\\+")
           ((#\\) "\\\\")
           ((#\?) "\\?")
           (else 
             (make-string 1 char)))))
    (regexp-substitute/global #f rexpstr str 'pre 'post)))

(define (string-char-count str char)
  (length (simple-filter (lambda (elt) (eq? elt char))
                         (string->list str))))

(define (string-replace-char! str old new)
  (let ((rexpstr 
         (if (not (eq? old #\.))
             (make-string 1 old)
             "\\."))
        (newstr (make-string 1 new)))
    (regexp-substitute/global #f rexpstr str 'pre newstr 'post)))

(define (string-split-on str char)
  (let ((parts '())
        (first-char #f))
    (let loop ((last-char (string-length str)))
      (set! first-char (string-rindex str char 0 last-char))
      (if first-char 
          (begin 
            (set! parts (cons (substring str (+ 1 first-char) last-char) 
                              parts))
            (loop first-char))
          (set! parts (cons (substring str 0 last-char) parts))))    
    parts))

