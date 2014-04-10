;; This is a portion of slib2c7 which has been included
;; with GnuCash in case the user has slib2c4, which is
;; lacking some functionality needed by GnuCash. This
;; file is only loaded if there is no later version of
;; slib available.

;; The complete distribution of slib is available at
;;   http://swissnet.ai.mit.edu/~jaffer/SLIB.html

;; Once slib2c6 or better becomes commonly available,
;; this file should be removed.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;"alist.scm", alist functions for Scheme.
;;;Copyright (c) 1992, 1993 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define (predicate->asso pred)
  (cond ((eq? eq? pred) assq)
	((eq? = pred) assv)
	((eq? eqv? pred) assv)
	((eq? char=? pred) assv)
	((eq? equal? pred) assoc)
	((eq? string=? pred) assoc)
	(else (lambda (key alist)
		(let l ((al alist))
		  (cond ((null? al) #f)
			((pred key (caar al)) (car al))
			(else (l (cdr al)))))))))

(define (alist-inquirer pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key)
      (let ((pair (assofun key alist)))
	(and pair (cdr pair))))))

(define (alist-associator pred)
  (let ((assofun (predicate->asso pred)))
    (lambda (alist key val)
      (let* ((pair (assofun key alist)))
	(cond (pair (set-cdr! pair val)
		    alist)
	      (else (cons (cons key val) alist)))))))

(define (alist-remover pred)
  (lambda (alist key)
    (cond ((null? alist) alist)
	  ((pred key (caar alist)) (cdr alist))
	  ((null? (cdr alist)) alist)
	  ((pred key (caadr alist))
	   (set-cdr! alist (cddr alist)) alist)
	  (else
	   (let l ((al (cdr alist)))
	     (cond ((null? (cdr al)) alist)
		   ((pred key (caadr al))
		    (set-cdr! al (cddr al)) alist)
		   (else (l (cdr al)))))))))

(define (alist-map proc alist)
  (map (lambda (pair) (cons (car pair) (proc (car pair) (cdr pair))))
       alist))

(define (alist-for-each proc alist)
  (for-each (lambda (pair) (proc (car pair) (cdr pair))) alist))


; "hash.scm", hashing functions for Scheme.
; Copyright (c) 1992, 1993, 1995 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define (hash:hash-char-ci char n)
  (modulo (char->integer (char-downcase char)) n))

(define hash:hash-char hash:hash-char-ci)

(define (hash:hash-symbol sym n)
  (hash:hash-string (symbol->string sym) n))

;;; This can overflow on implemenatations where inexacts have a larger
;;; range than exact integers.
(define hash:hash-number
  (if (provided? 'inexact)
      (lambda (num n)
	(if (integer? num)
	    (modulo (if (exact? num) num (inexact->exact num)) n)
	    (hash:hash-string-ci
	     (number->string (if (exact? num) (exact->inexact num) num))
	     n)))
      (lambda (num n)
	(if (integer? num)
	    (modulo num n)
	    (hash:hash-string-ci (number->string num) n)))))

(define (hash:hash-string-ci str n)
  (let ((len (string-length str)))
    (if (> len 5)
	(let loop ((h (modulo 264 n)) (i 5))
	  (if (positive? i)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase
				 (string-ref str (modulo h len)))))
			    n)
		    (- i 1))
	      h))
	(let loop ((h 0) (i (- len 1)))
	  (if (>= i 0)
	      (loop (modulo (+ (* h 256)
			       (char->integer
				(char-downcase (string-ref str i))))
			    n)
		    (- i 1))
	      h)))))

(define hash:hash-string hash:hash-string-ci)

(define (hash:hash obj n)
  (let hs ((d 10) (obj obj))
    (cond
     ((number? obj)      (hash:hash-number obj n))
     ((char? obj)        (modulo (char->integer (char-downcase obj)) n))
     ((symbol? obj)      (hash:hash-symbol obj n))
     ((string? obj)      (hash:hash-string obj n))
     ((vector? obj)
      (let ((len (vector-length obj)))
	(if (> len 5)
	    (let lp ((h 1) (i (quotient d 2)))
	      (if (positive? i)
		  (lp (modulo (+ (* h 256)
				 (hs 2 (vector-ref obj (modulo h len))))
			      n)
		      (- i 1))
		  h))
	    (let loop ((h (- n 1)) (i (- len 1)))
	      (if (>= i 0)
		  (loop (modulo (+ (* h 256) (hs (quotient d len)
						 (vector-ref obj i)))
				n)
			(- i 1))
		  h)))))
     ((pair? obj)
      (if (positive? d) (modulo (+ (hs (quotient d 2) (car obj))
				   (hs (quotient d 2) (cdr obj)))
				n)
	  1))
     (else
      (modulo
       (cond
	((null? obj)        256)
	((boolean? obj)     (if obj 257 258))
	((eof-object? obj)  259)
	((input-port? obj)  260)
	((output-port? obj) 261)
	((procedure? obj)   262)
	((and (provided? 'RECORD) (record? obj))
	 (let* ((rtd (record-type-descriptor obj))
		(fns (record-type-field-names rtd))
		(len (length fns)))
	   (if (> len 5)
	       (let lp ((h (modulo 266 n)) (i (quotient d 2)))
		 (if (positive? i)
		     (lp (modulo
			  (+ (* h 256)
			     (hs 2 ((record-accessor
				     rtd (list-ref fns (modulo h len)))
				    obj)))
			  n)
			 (- i 1))
		     h))
	       (let loop ((h (- n 1)) (i (- len 1)))
		 (if (>= i 0)
		     (loop (modulo
			    (+ (* h 256)
			       (hs (quotient d len)
				   ((record-accessor
				     rtd (list-ref fns (modulo h len)))
				    obj)))
			    n)
			   (- i 1))
		     h)))))
	(else               263))
       n)))))

(define hash hash:hash)
(define hashv hash:hash)

;;; Object-hash is somewhat expensive on copying GC systems (like
;;; PC-Scheme and MITScheme).  We use it only on strings, pairs,
;;; vectors, and records.  This also allows us to use it for both
;;; hashq and hashv.

(if (provided? 'object-hash)
    (set! hashv
	  (if (provided? 'record)
	      (lambda (obj k)
		(if (or (string? obj) (pair? obj) (vector? obj) (record? obj))
		    (modulo (object-hash obj) k)
		    (hash:hash obj k)))
	      (lambda (obj k)
		(if (or (string? obj) (pair? obj) (vector? obj))
		    (modulo (object-hash obj) k)
		    (hash:hash obj k))))))

(define hashq hashv)


; "hashtab.scm", hash tables for Scheme.
; Copyright (c) 1992, 1993 Aubrey Jaffer
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define (predicate->hash pred)
  (cond ((eq? pred eq?) hashq)
	((eq? pred eqv?) hashv)
	((eq? pred equal?) hash)
	((eq? pred =) hashv)
	((eq? pred char=?) hashv)
	((eq? pred char-ci=?) hashv)
	((eq? pred string=?) hash)
	((eq? pred string-ci=?) hash)
	(else (slib:error "unknown predicate for hash" pred))))

(define (make-hash-table k) (make-vector k '()))

(define (predicate->hash-asso pred)
  (let ((hashfun (predicate->hash pred))
	(asso (predicate->asso pred)))
    (lambda (key hashtab)
      (asso key
	    (vector-ref hashtab (hashfun key (vector-length hashtab)))))))

(define (hash-inquirer pred)
  (let ((hashfun (predicate->hash pred))
	(ainq (alist-inquirer pred)))
    (lambda (hashtab key)
      (ainq (vector-ref hashtab (hashfun key (vector-length hashtab)))
	    key))))

(define (hash-associator pred)
  (let ((hashfun (predicate->hash pred))
	(asso (alist-associator pred)))
    (lambda (hashtab key val)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (asso (vector-ref hashtab num) key val)))
      hashtab)))

(define (hash-remover pred)
  (let ((hashfun (predicate->hash pred))
	(arem (alist-remover pred)))
    (lambda (hashtab key)
      (let* ((num (hashfun key (vector-length hashtab))))
	(vector-set! hashtab num
		     (arem (vector-ref hashtab num) key)))
      hashtab)))

(define (hash-map proc ht)
  (define nht (make-vector (vector-length ht)))
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i) nht)
    (vector-set!
     nht i
     (alist-map proc (vector-ref ht i)))))

(define (hash-for-each proc ht)
  (do ((i (+ -1 (vector-length ht)) (+ -1 i)))
      ((negative? i))
    (alist-for-each proc (vector-ref ht i))))


;;; "strcase.scm" String casing functions.
; Written 1992 by Dirk Lutzebaeck (lutzeb@cs.tu-berlin.de)
;
; This code is in the public domain.

; Modified by Aubrey Jaffer Nov 1992.
; Authors of the original version were Ken Dickey and Aubrey Jaffer.

;string-upcase, string-downcase, string-capitalize
; are obvious string conversion procedures and are non destructive.
;string-upcase!, string-downcase!, string-capitalize!
; are destructive versions.

(define (string-upcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)
    (string-set! str i (char-upcase (string-ref str i)))))

(define (string-upcase str)
  (string-upcase! (string-copy str)))

(define (string-downcase! str)
  (do ((i (- (string-length str) 1) (- i 1)))
      ((< i 0) str)
    (string-set! str i (char-downcase (string-ref str i)))))

(define (string-downcase str)
  (string-downcase! (string-copy str)))

(define (string-capitalize! str)	; "hello" -> "Hello"
  (let ((non-first-alpha #f)		; "hELLO" -> "Hello"
	(str-len (string-length str)))	; "*hello" -> "*Hello"
    (do ((i 0 (+ i 1)))			; "hello you" -> "Hello You"
	((= i str-len) str)
      (let ((c (string-ref str i)))
	(if (char-alphabetic? c)
	    (if non-first-alpha
		(string-set! str i (char-downcase c))
		(begin
		  (set! non-first-alpha #t)
		  (string-set! str i (char-upcase c))))
	    (set! non-first-alpha #f))))))

(define (string-capitalize str)
  (string-capitalize! (string-copy str)))

(define string-ci->symbol
  (if (equal? "a" (symbol->string 'a))
      (lambda (str) (string->symbol (string-downcase str)))
      (lambda (str) (string->symbol (string-upcase str)))))


;;"genwrite.scm" generic write used by pretty-print and truncated-print.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

(define (generic-write obj display? width output)

  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((QUOTE QUASIQUOTE UNQUOTE UNQUOTE-SPLICING) (length1? tail))
        (else                                        #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((QUOTE)            "'")
        ((QUASIQUOTE)       "`")
        ((UNQUOTE)          ",")
        ((UNQUOTE-SPLICING) ",@"))))

  (define (out str col)
    (and col (output str) (+ col (string-length str))))

  (define (wr obj col)

    (define (wr-expr expr col)
      (if (read-macro? expr)
        (wr (read-macro-body expr) (out (read-macro-prefix expr) col))
        (wr-lst expr col)))

    (define (wr-lst l col)
      (if (pair? l)
	  (let loop ((l (cdr l))
		     (col (and col (wr (car l) (out "(" col)))))
	    (cond ((not col) col)
		  ((pair? l)
		   (loop (cdr l) (wr (car l) (out " " col))))
		  ((null? l) (out ")" col))
		  (else      (out ")" (wr l (out " . " col))))))
	  (out "()" col)))

    (cond ((pair? obj)        (wr-expr obj col))
          ((null? obj)        (wr-lst obj col))
          ((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
          ((boolean? obj)     (out (if obj "#t" "#f") col))
          ((number? obj)      (out (number->string obj) col))
          ((symbol? obj)      (out (symbol->string obj) col))
          ((procedure? obj)   (out "#[procedure]" col))
          ((string? obj)      (if display?
                                (out obj col)
                                (let loop ((i 0) (j 0) (col (out "\"" col)))
                                  (if (and col (< j (string-length obj)))
                                    (let ((c (string-ref obj j)))
                                      (if (or (char=? c #\\)
                                              (char=? c #\"))
                                        (loop j
                                              (+ j 1)
                                              (out "\\"
                                                   (out (substring obj i j)
                                                        col)))
                                        (loop i (+ j 1) col)))
                                    (out "\""
                                         (out (substring obj i j) col))))))
          ((char? obj)        (if display?
                                (out (make-string 1 obj) col)
                                (out (case obj
                                       ((#\space)   "space")
                                       ((#\newline) "newline")
                                       (else        (make-string 1 obj)))
                                     (out "#\\" col))))
          ((input-port? obj)  (out "#[input-port]" col))
          ((output-port? obj) (out "#[output-port]" col))
          ((eof-object? obj)  (out "#[eof-object]" col))
          (else               (out "#[unknown]" col))))

  (define (pp obj col)

    (define (spaces n col)
      (if (> n 0)
        (if (> n 7)
          (spaces (- n 8) (out "        " col))
          (out (substring "        " 0 n) col))
        col))

    (define (indent to col)
      (and col
           (if (< to col)
             (and (out (make-string 1 #\newline) col) (spaces to 0))
             (spaces (- to col) col))))

    (define (pr obj col extra pp-pair)
      (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
        (let ((result '())
              (left (min (+ (- (- width col) extra) 1) max-expr-width)))
          (generic-write obj display? #f
            (lambda (str)
              (set! result (cons str result))
              (set! left (- left (string-length str)))
              (> left 0)))
          (if (> left 0) ; all can be printed on one line
            (out (reverse-string-append result) col)
            (if (pair? obj)
              (pp-pair obj col extra)
              (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
        (wr obj col)))

    (define (pp-expr expr col extra)
      (if (read-macro? expr)
        (pr (read-macro-body expr)
            (out (read-macro-prefix expr) col)
            extra
            pp-expr)
        (let ((head (car expr)))
          (if (symbol? head)
            (let ((proc (style head)))
              (if proc
                (proc expr col extra)
                (if (> (string-length (symbol->string head))
                       max-call-head-width)
                  (pp-general expr col extra #f #f #f pp-expr)
                  (pp-call expr col extra pp-expr))))
            (pp-list expr col extra pp-expr)))))

    ; (head item1
    ;       item2
    ;       item3)
    (define (pp-call expr col extra pp-item)
      (let ((col* (wr (car expr) (out "(" col))))
        (and col
             (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

    ; (item1
    ;  item2
    ;  item3)
    (define (pp-list l col extra pp-item)
      (let ((col (out "(" col)))
        (pp-down l col col extra pp-item)))

    (define (pp-down l col1 col2 extra pp-item)
      (let loop ((l l) (col col1))
        (and col
             (cond ((pair? l)
                    (let ((rest (cdr l)))
                      (let ((extra (if (null? rest) (+ extra 1) 0)))
                        (loop rest
                              (pr (car l) (indent col2 col) extra pp-item)))))
                   ((null? l)
                    (out ")" col))
                   (else
                    (out ")"
                         (pr l
                             (indent col2 (out "." (indent col2 col)))
                             (+ extra 1)
                             pp-item)))))))

    (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

      (define (tail1 rest col1 col2 col3)
        (if (and pp-1 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+ extra 1) 0)))
            (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
          (tail2 rest col1 col2 col3)))

      (define (tail2 rest col1 col2 col3)
        (if (and pp-2 (pair? rest))
          (let* ((val1 (car rest))
                 (rest (cdr rest))
                 (extra (if (null? rest) (+ extra 1) 0)))
            (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
          (tail3 rest col1 col2)))

      (define (tail3 rest col1 col2)
        (pp-down rest col2 col1 extra pp-3))

      (let* ((head (car expr))
             (rest (cdr expr))
             (col* (wr head (out "(" col))))
        (if (and named? (pair? rest))
          (let* ((name (car rest))
                 (rest (cdr rest))
                 (col** (wr name (out " " col*))))
            (tail1 rest (+ col indent-general) col** (+ col** 1)))
          (tail1 rest (+ col indent-general) col* (+ col* 1)))))

    (define (pp-expr-list l col extra)
      (pp-list l col extra pp-expr))

    (define (pp-LAMBDA expr col extra)
      (pp-general expr col extra #f pp-expr-list #f pp-expr))

    (define (pp-IF expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr))

    (define (pp-COND expr col extra)
      (pp-call expr col extra pp-expr-list))

    (define (pp-CASE expr col extra)
      (pp-general expr col extra #f pp-expr #f pp-expr-list))

    (define (pp-AND expr col extra)
      (pp-call expr col extra pp-expr))

    (define (pp-LET expr col extra)
      (let* ((rest (cdr expr))
             (named? (and (pair? rest) (symbol? (car rest)))))
        (pp-general expr col extra named? pp-expr-list #f pp-expr)))

    (define (pp-BEGIN expr col extra)
      (pp-general expr col extra #f #f #f pp-expr))

    (define (pp-DO expr col extra)
      (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

    ; define formatting style (change these to suit your style)

    (define indent-general 2)

    (define max-call-head-width 5)

    (define max-expr-width 50)

    (define (style head)
      (case head
        ((LAMBDA LET* LETREC DEFINE) pp-LAMBDA)
        ((IF SET!)                   pp-IF)
        ((COND)                      pp-COND)
        ((CASE)                      pp-CASE)
        ((AND OR)                    pp-AND)
        ((LET)                       pp-LET)
        ((BEGIN)                     pp-BEGIN)
        ((DO)                        pp-DO)
        (else                        #f)))

    (pr obj col 0 pp-expr))

  (if width
    (out (make-string 1 #\newline) (pp obj 0))
    (wr obj 0)))

; (reverse-string-append l) = (apply string-append (reverse l))

(define (reverse-string-append l)

  (define (rev-string-append l i)
    (if (pair? l)
      (let* ((str (car l))
             (len (string-length str))
             (result (rev-string-append (cdr l) (+ i len))))
        (let loop ((j 0) (k (- (- (string-length result) i) len)))
          (if (< j len)
            (begin
              (string-set! result k (string-ref str j))
              (loop (+ j 1) (+ k 1)))
            result)))
      (make-string i)))

  (rev-string-append l 0))


;;;; "printf.scm" Implementation of standard C functions for Scheme
;;; Copyright (C) 1991-1993, 1996 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;; Parse the output of NUMBER->STRING.
;; Returns a list: (sign-character digit-string exponent-integer)
;; SIGN-CHAR will be either #\+ or #\-, DIGIT-STRING will always begin
;; with a "0", after which a decimal point should be understood.
;; If STR denotes a non-real number, 3 additional elements for the
;; complex part are appended.
(define (stdio:parse-float str)
  (let ((n (string-length str))
	(iend 0))
    (letrec ((prefix
	      (lambda (i rest)
		(if (and (< i (- n 1))
			 (char=? #\# (string-ref str i)))
		    (case (string-ref str (+ i 1))
		      ((#\d #\i #\e) (prefix (+ i 2) rest))
		      ((#\.) (rest i))
		      (else (parse-error)))
		    (rest i))))
	     (sign
	      (lambda (i rest)
		(if (< i n)
		    (let ((c (string-ref str i)))
		      (case c
			((#\- #\+) (cons c (rest (+ i 1))))
			(else (cons #\+ (rest i))))))))
	     (digits
	      (lambda (i rest)
		(do ((j i (+ j 1)))
		    ((or (>= j n)
			 (not (or (char-numeric? (string-ref str j))
				  (char=? #\# (string-ref str j)))))
		     (cons
		      (if (= i j) "0" (substring str i j))
		      (rest j))))))
	     (point
	      (lambda (i rest)
		(if (and (< i n)
			 (char=? #\. (string-ref str i)))
		    (rest (+ i 1))
		    (rest i))))
	     (exp
	      (lambda (i)
		(if (< i n)
		    (case (string-ref str i)
		      ((#\e #\s #\f #\d #\l #\E #\S #\F #\D #\L)
		       (let ((s (sign (+ i 1) (lambda (i) (digits i end!)))))
			 (list
			  (if (char=? #\- (car s))
			      (- (string->number (cadr s)))
			      (string->number (cadr s))))))
		      (else (end! i)
			    '(0)))
		    (begin (end! i)
			   '(0)))))
	     (end!
	      (lambda (i)
		(set! iend i)
		'()))
	     (real
	      (lambda (i)
		(let ((parsed
		       (prefix
			i
			(lambda (i)
			  (sign
			   i
			   (lambda (i)
			     (digits
			      i
			      (lambda (i)
				(point
				 i
				 (lambda (i)
				   (digits i exp)))))))))))
		  (and (list? parsed)
		       (apply
			(lambda (sgn idigs fdigs exp)
			  (let* ((digs (string-append "0" idigs fdigs))
				 (n (string-length digs)))
			    (let loop ((i 1)
				       (exp (+ exp (string-length idigs))))
			      (if (and (< i n)
				       (char=? #\0 (string-ref digs i)))
				  (loop (+ i 1) (- exp 1))
				  (list sgn (substring digs (- i 1) n) exp)))))
			parsed)))))
	     (parse-error
	      (lambda () #f)))
      (let ((realpart (real 0)))
	(cond ((= iend n) realpart)
	      ((memv (string-ref str iend) '(#\+ #\-))
	       (let ((complexpart (real iend)))
		 (and (= iend (- n 1))
		      (char-ci=? #\i (string-ref str iend))
		      (append realpart complexpart))))
	      ((eqv? (string-ref str iend) #\@)
	       ;; Polar form:  No point in parsing the angle ourselves,
	       ;; since some transcendental approximation is unavoidable.
	       (let ((num (string->number str)))
		 (and num
		      (let ((realpart
			     (stdio:parse-float
			      (number->string (real-part num))))
			    (imagpart
			     (if (real? num)
				 '()
				 (stdio:parse-float
				  (number->string (imag-part num))))))
			(and realpart imagpart
			     (append realpart imagpart))))))
	      (else #f))))))

;; STR is a digit string representing a floating point mantissa, STR must
;; begin with "0", after which a decimal point is understood.
;; The output is a digit string rounded to NDIGS digits after the decimal
;; point implied between chars 0 and 1.
;; If STRIP-0S is not #F then trailing zeros will be stripped from the result.
;; In this case, STRIP-0S should be the minimum number of digits required
;; after the implied decimal point.
(define (stdio:round-string str ndigs strip-0s)
  (let* ((n (- (string-length str) 1))
	 (res
	  (cond ((< ndigs 0) "")
		((= n ndigs) str)
		((< n ndigs)
		 (let ((zeropad (make-string
				 (max 0 (- (or strip-0s ndigs) n))
				 (if (char-numeric? (string-ref str n))
				     #\0 #\#))))
		   (if (zero? (string-length zeropad))
		       str
		       (string-append str zeropad))))
		(else
		 (let ((res (substring str 0 (+ ndigs 1)))
		       (dig (lambda (i)
			      (let ((c (string-ref str i)))
				(if (char-numeric? c)
				    (string->number (string c))
				    0)))))
		   (let ((ldig (dig (+ 1 ndigs))))
		     (if (or (> ldig 5)
			     (and (= ldig 5)
				  (let loop ((i (+ 2 ndigs)))
				    (if (> i n) (odd? (dig ndigs))
					(if (zero? (dig i))
					    (loop (+ i 1))
					    #t)))))
			 (let inc! ((i ndigs))
			   (let ((d (dig i)))
			     (if (< d 9)
				 (string-set! res i
					      (string-ref
					       (number->string (+ d 1)) 0))
				 (begin
				   (string-set! res i #\0)
				   (inc! (- i 1))))))))
		   res)))))
    (if strip-0s
	(let loop ((i (- (string-length res) 1)))
	  (if (or (<= i strip-0s)
		  (not (char=? #\0 (string-ref res i))))
	      (substring res 0 (+ i 1))
	      (loop (- i 1))))
	res)))

(define (stdio:iprintf out format-string . args)
  (cond
   ((not (equal? "" format-string))
    (let ((pos -1)
	  (fl (string-length format-string))
	  (fc (string-ref format-string 0)))

      (define (advance)
	(set! pos (+ 1 pos))
	(cond ((>= pos fl) (set! fc #f))
	      (else (set! fc (string-ref format-string pos)))))
      (define (must-advance)
	(set! pos (+ 1 pos))
	(cond ((>= pos fl) (incomplete))
	      (else (set! fc (string-ref format-string pos)))))
      (define (end-of-format?)
	(>= pos fl))
      (define (incomplete)
	(slib:error 'printf "conversion specification incomplete"
		    format-string))
      (define (wna)
	(slib:error 'printf "wrong number of arguments"
		    (length args)
		    format-string))

      (let loop ((args args))
	(advance)
	(cond
	 ((end-of-format?)
	  ;;(or (null? args) (wna))	;Extra arguments are *not* a bug.
	  )
	 ((eqv? #\\ fc);;Emulating C strings may not be a good idea.
	  (must-advance)
	  (and (case fc
		 ((#\n #\N) (out #\newline))
		 ((#\t #\T) (out slib:tab))
		 ;;((#\r #\R) (out #\return))
		 ((#\f #\F) (out slib:form-feed))
		 ((#\newline) #t)
		 (else (out fc)))
	       (loop args)))
	 ((eqv? #\% fc)
	  (must-advance)
	  (let ((left-adjust #f)	;-
		(signed #f)		;+
		(blank #f)
		(alternate-form #f)	;#
		(leading-0s #f)		;0
		(width 0)
		(precision -1)
		(type-modifier #f)
		(read-format-number
		 (lambda ()
		   (cond
		    ((eqv? #\* fc)	; GNU extension
		     (must-advance)
		     (let ((ans (car args)))
		       (set! args (cdr args))
		       ans))
		    (else
		     (do ((c fc fc)
			  (accum 0 (+ (* accum 10)
				      (string->number (string c)))))
			 ((not (char-numeric? fc)) accum)
		       (must-advance)))))))
	    (define (pad pre . strs)
	      (let loop ((len (string-length pre))
			 (ss strs))
		(cond ((>= len width) (apply string-append pre strs))
		      ((null? ss)
		       (cond (left-adjust
			      (apply string-append
				     pre
				     (append strs
					     (list (make-string
						    (- width len) #\space)))))
			     (leading-0s
			      (apply string-append
				     pre
				     (make-string (- width len) #\0)
				     strs))
			     (else
			      (apply string-append
				     (make-string (- width len) #\space)
				     pre strs))))
		      (else
		       (loop (+ len (string-length (car ss))) (cdr ss))))))
	    (define integer-convert
	      (lambda (s radix)
		(cond ((not (negative? precision))
		       (set! leading-0s #f)
		       (if (and (zero? precision)
				(eqv? 0 s))
			   (set! s ""))))
		(set! s (cond ((symbol? s) (symbol->string s))
			      ((number? s) (number->string s radix))
			      ((or (not s) (null? s)) "0")
			      ((string? s) s)
			      (else "1")))
		(let ((pre (cond ((equal? "" s) "")
				 ((eqv? #\- (string-ref s 0))
				  (set! s (substring s 1 (string-length s)))
				  "-")
				 (signed "+")
				 (blank " ")
				 (alternate-form
				  (case radix
				    ((8) "0")
				    ((16) "0x")
				    (else "")))
				 (else ""))))
		  (pad pre
		       (if (< (string-length s) precision)
			   (make-string
			    (- precision (string-length s)) #\0)
			   "")
		       s))))
	    (define (float-convert num fc)
	      (define (f digs exp strip-0s)
		(let ((digs (stdio:round-string
			     digs (+ exp precision) (and strip-0s exp))))
		  (cond ((>= exp 0)
			 (let* ((i0 (cond ((zero? exp) 0)
					  ((char=? #\0 (string-ref digs 0)) 1)
					  (else 0)))
				(i1 (max 1 (+ 1 exp)))
				(idigs (substring digs i0 i1))
				(fdigs (substring digs i1
						  (string-length digs))))
			   (cons idigs
				 (if (and (string=? fdigs "")
					  (not alternate-form))
				     '()
				     (list "." fdigs)))))
			((zero? precision)
			 (list (if alternate-form "0." "0")))
			((and strip-0s (string=? digs "") (list "0")))
			(else
			 (list "0."
			       (make-string (min precision (- -1 exp)) #\0)
			       digs)))))
	      (define (e digs exp strip-0s)
		(let* ((digs (stdio:round-string
			      digs (+ 1 precision) (and strip-0s 0)))
		       (istrt (if (char=? #\0 (string-ref digs 0)) 1 0))
		       (fdigs (substring
			       digs (+ 1 istrt) (string-length digs)))
		       (exp (if (zero? istrt) exp (- exp 1))))
		  (list
		   (substring digs istrt (+ 1 istrt))
		   (if (and (string=? fdigs "") (not alternate-form))
		       "" ".")
		   fdigs
		   (if (char-upper-case? fc) "E" "e")
		   (if (negative? exp) "-" "+")
		   (if (< -10 exp 10) "0" "")
		   (number->string (abs exp)))))
	      (define (g digs exp)
		(let ((strip-0s (not alternate-form)))
		  (set! alternate-form #f)
		  (cond ((<= (- 1 precision) exp precision)
			 (set! precision (- precision exp))
			 (f digs exp strip-0s))
			(else
			 (set! precision (- precision 1))
			 (e digs exp strip-0s)))))
	      (define (k digs exp sep)
		(let* ((units '#("y" "z" "a" "f" "p" "n" "u" "m" ""
				 "k" "M" "G" "T" "P" "E" "Z" "Y"))
		       (base 8)		;index of ""
		       (uind (let ((i (if (negative? exp)
					  (quotient (- exp 3) 3)
					  (quotient (- exp 1) 3))))
			       (and
				(< -1 (+ i base) (vector-length units))
				i))))
		  (cond (uind
			 (set! exp (- exp (* 3 uind)))
			 (set! precision (max 0 (- precision exp)))
			 (append
			  (f digs exp #f)
			  (list sep
				(vector-ref units (+ uind base)))))
			(else
			 (g digs exp)))))

	      (cond ((negative? precision)
		     (set! precision 6))
		    ((and (zero? precision)
			  (char-ci=? fc #\g))
		     (set! precision 1)))
	      (let* ((str
		      (cond ((number? num)
			     (number->string (exact->inexact num)))
			    ((string? num) num)
			    ((symbol? num) (symbol->string num))
			    (else "???")))
		     (parsed (stdio:parse-float str)))
		(letrec ((format-real
			  (lambda (signed? sgn digs exp . rest)
			    (if (null? rest)
				(cons
				 (if (char=? #\- sgn) "-"
				     (if signed? "+" (if blank " " "")))
				 (case fc
				   ((#\e #\E) (e digs exp #f))
				   ((#\f #\F) (f digs exp #f))
				   ((#\g #\G) (g digs exp))
				   ((#\k) (k digs exp ""))
				   ((#\K) (k digs exp " "))))
				(append (format-real signed? sgn digs exp)
					(apply format-real #t rest)
					'("i"))))))
		  (if parsed
		      (apply pad (apply format-real signed parsed))
		      (pad "???")))))
	    (do ()
		((case fc
		   ((#\-) (set! left-adjust #t) #f)
		   ((#\+) (set! signed #t) #f)
		   ((#\ ) (set! blank #t) #f)
		   ((#\#) (set! alternate-form #t) #f)
		   ((#\0) (set! leading-0s #t) #f)
		   (else #t)))
	      (must-advance))
	    (cond (left-adjust (set! leading-0s #f)))
	    (cond (signed (set! blank #f)))

	    (set! width (read-format-number))
	    (cond ((negative? width)
		   (set! left-adjust #t)
		   (set! width (- width))))
	    (cond ((eqv? #\. fc)
		   (must-advance)
		   (set! precision (read-format-number))))
	    (case fc			;Ignore these specifiers
	      ((#\l #\L #\h)
	       (set! type-modifier fc)
	       (must-advance)))

	    ;;At this point fc completely determines the format to use.
	    (if (null? args)
		(if (memv (char-downcase fc)
			  '(#\c #\s #\a #\d #\i #\u #\o #\x #\b
			    #\f #\e #\g #\k))
		    (wna)))

	    (case fc
	      ;; only - is allowed between % and c
	      ((#\c #\C)		; C is enhancement
	       (and (out (string (car args))) (loop (cdr args))))

	      ;; only - flag, no type-modifiers
	      ((#\s #\S)		; S is enhancement
	       (let ((s (cond
			 ((symbol? (car args)) (symbol->string (car args)))
			 ((not (car args)) "(NULL)")
			 (else (car args)))))
		 (cond ((not (or (negative? precision)
				 (>= precision (string-length s))))
			(set! s (substring s 0 precision))))
		 (and (out (cond
			    ((<= width (string-length s)) s)
			    (left-adjust
			     (string-append
			      s (make-string (- width (string-length s)) #\ )))
			    (else
			     (string-append
			      (make-string (- width (string-length s))
					   (if leading-0s #\0 #\ )) s))))
		      (loop (cdr args)))))

	      ;; SLIB extension
	      ((#\a #\A)		;#\a #\A are pretty-print
	       (let ((os "") (pr precision))
		 (generic-write
		  (car args) (not alternate-form) #f
		  (cond ((and left-adjust (negative? pr))
			 (set! pr 0)
			 (lambda (s)
			   (set! pr (+ pr (string-length s)))
			   (out s)))
			(left-adjust
			 (lambda (s)
			   (define sl (- pr (string-length s)))
			   (set! pr (cond ((negative? sl)
					   (out (substring s 0 pr)) 0)
					  (else (out s) sl)))
			   (positive? sl)))
			((negative? pr)
			 (set! pr width)
			 (lambda (s)
			   (set! pr (- pr (string-length s)))
			   (cond ((not os) (out s))
				 ((negative? pr)
				  (out os)
				  (set! os #f)
				  (out s))
				 (else (set! os (string-append os s))))
			   #t))
			(else
			 (lambda (s)
			   (define sl (- pr (string-length s)))
			   (cond ((negative? sl)
				  (set! os (string-append
					    os (substring s 0 pr))))
				 (else (set! os (string-append os s))))
			   (set! pr sl)
			   (positive? sl)))))
		 (cond ((and left-adjust (negative? precision))
			(cond
			 ((> width pr) (out (make-string (- width pr) #\ )))))
		       (left-adjust
			(cond
			 ((> width (- precision pr))
			  (out (make-string (- width (- precision pr)) #\ )))))
		       ((not os))
		       ((<= width (string-length os)) (out os))
		       (else (and (out (make-string
					(- width (string-length os)) #\ ))
				  (out os)))))
	       (loop (cdr args)))
	      ((#\d #\D #\i #\I #\u #\U)
	       (and (out (integer-convert (car args) 10)) (loop (cdr args))))
	      ((#\o #\O)
	       (and (out (integer-convert (car args) 8)) (loop (cdr args))))
	      ((#\x #\X)
	       (and (out ((if (char-upper-case? fc)
			      string-upcase string-downcase)
			  (integer-convert (car args) 16)))
		    (loop (cdr args))))
	      ((#\b #\B)
	       (and (out (integer-convert (car args) 2)) (loop (cdr args))))
	      ((#\%) (and (out #\%) (loop args)))
	      ((#\f #\F #\e #\E #\g #\G #\k #\K)
	       (and (out (float-convert (car args) fc)) (loop (cdr args))))
	      (else
	       (cond ((end-of-format?) (incomplete))
		     (else (and (out #\%) (out fc) (out #\?) (loop args))))))))
	 (else (and (out fc) (loop args)))))))))

(define (stdio:fprintf port format . args)
  (let ((cnt 0))
    (apply stdio:iprintf
	   (lambda (x)
	     (cond ((string? x)
		    (set! cnt (+ (string-length x) cnt)) (display x port) #t)
		   (else (set! cnt (+ 1 cnt)) (display x port) #t)))
	   format args)
    cnt))

(define (stdio:printf format . args)
  (apply stdio:fprintf (current-output-port) format args))

(define (stdio:sprintf str format . args)
  (let* ((cnt 0)
	 (s (cond ((string? str) str)
		  ((number? str) (make-string str))
		  ((not str) (make-string 100))
		  (else (slib:error 'sprintf "first argument not understood"
				    str))))
	 (end (string-length s)))
    (apply stdio:iprintf
	   (lambda (x)
	     (cond ((string? x)
		    (if (or str (>= (- end cnt) (string-length x)))
			(do ((lend (min (string-length x) (- end cnt)))
			     (i 0 (+ i 1)))
			    ((>= i lend))
			  (string-set! s cnt (string-ref x i))
			  (set! cnt (+ cnt 1)))
			(let ()
			  (set! s (string-append (substring s 0 cnt) x))
			  (set! cnt (string-length s))
			  (set! end cnt))))
		   ((and str (>= cnt end)))
		   (else (cond ((and (not str) (>= cnt end))
				(set! s (string-append s (make-string 100)))
				(set! end (string-length s))))
			 (string-set! s cnt (if (char? x) x #\?))
			 (set! cnt (+ cnt 1))))
	     (not (and str (>= cnt end))))
	   format
	   args)
    (cond ((string? str) cnt)
	  ((eqv? end cnt) s)
	  (else (substring s 0 cnt)))))

(define printf stdio:printf)
(define fprintf stdio:fprintf)
(define sprintf stdio:sprintf)

;;(do ((i 0 (+ 1 i))) ((> i 50)) (printf "%s\n" (sprintf i "%#-13a:%#13a:%-13.8a:" "123456789" "123456789" "123456789")))
