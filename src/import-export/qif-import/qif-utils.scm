;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  qif-utils.scm
;;;  string munging and other utility routines 
;;;
;;;  Bill Gribble <grib@billgribble.com> 20 Feb 2000 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-modules (srfi srfi-13))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  string-remove-chars
;;
;;  Removes all characters in string "chars" from string "str".
;;  Example: (string-remove-chars "abcd" "cb") returns "ad".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-remove-chars str chars)
  (string-delete str (lambda (c) (string-index chars c))))


(define (string-char-count str char)
  (length (simple-filter (lambda (elt) (eq? elt char))
                         (string->list str))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  string-replace-char
;;
;;  Replaces all occurrences of char "old" with char "new".
;;  Example: (string-replace-char "foo" #\o #\c) returns "fcc".
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (string-replace-char str old new)
  (string-map (lambda (c) (if (char=? c old) new c)) str))


(define (string-replace-char! str old new)
  (let ((rexpstr 
         (if (not (eq? old #\.))
             (make-string 1 old)
             "\\."))
        (newstr (make-string 1 new)))
    (regexp-substitute/global #f rexpstr str 'pre newstr 'post)))

(define (string-to-canonical-symbol str)
  (string->symbol 
   (string-downcase
    (string-remove-leading-space
     (string-remove-trailing-space str)))))

