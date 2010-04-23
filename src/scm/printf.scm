;; gnucash
;; Copyright (C) 2009 Andy Wingo <wingo at pobox dot com>

;; This program is free software; you can redistribute it and/or    
;; modify it under the terms of the GNU General Public License as   
;; published by the Free Software Foundation; either version 2 of   
;; the License, or (at your option) any later version.              
;;                                                                  
;; This program is distributed in the hope that it will be useful,  
;; but WITHOUT ANY WARRANTY; without even the implied warranty of   
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    
;; GNU General Public License for more details.                     
;;                                                                  
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org

;;; Commentary:
;;
;;Code pulled in from Aubrey Jaffer's SLIB.
;;
;;; Code:

(define-module (gnucash printf)
  #:export (printf fprintf sprintf))

;; Stub slib support, so we don't depend on slib proper.
(define slib:error error)
(define slib:tab #\tab)
(define slib:form-feed #\page)
(define (require feature) #f) ; noop
(define (require-if condition feature) #f) ; noop

;; The parts of slib that we need: glob.scm, genwrite.scm, and printf.scm.

;;; "glob.scm" String matching for filenames (a la BASH).
;;; Copyright (C) 1998 Radey Shouman.
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

;;@code{(require 'filename)} or @code{(require 'glob)}
;;@ftindex filename
;;@ftindex glob

(define (glob:pattern->tokens pat)
  (cond
   ((string? pat)
    (let loop ((i 0)
	       (toks '()))
      (if (>= i (string-length pat))
	  (reverse toks)
	  (let ((pch (string-ref pat i)))
	    (case pch
	      ((#\? #\*)
	       (loop (+ i 1)
		     (cons (substring pat i (+ i 1)) toks)))
	      ((#\[)
	       (let ((j
		      (let search ((j (+ i 2)))
			(cond
			 ((>= j (string-length pat))
			  (slib:error 'glob:make-matcher
				      "unmatched [" pat))
			 ((char=? #\] (string-ref pat j))
			  (if (and (< (+ j 1) (string-length pat))
				   (char=? #\] (string-ref pat (+ j 1))))
			      (+ j 1)
			      j))
			 (else (search (+ j 1)))))))
		 (loop (+ j 1) (cons (substring pat i (+ j 1)) toks))))
	      (else
	       (let search ((j (+ i 1)))
		 (cond ((= j (string-length pat))
			(loop j (cons (substring pat i j) toks)))
		       ((memv (string-ref pat j) '(#\? #\* #\[))
			(loop j (cons (substring pat i j) toks)))
		       (else (search (+ j 1)))))))))))
   ((pair? pat)
    (for-each (lambda (elt) (or (string? elt)
				(slib:error 'glob:pattern->tokens
					    "bad pattern" pat)))
	      pat)
    pat)
   (else (slib:error 'glob:pattern->tokens "bad pattern" pat))))

(define (glob:make-matcher pat ch=? ch<=?)
  (define (match-end str k kmatch)
    (and (= k (string-length str)) (reverse (cons k kmatch))))
  (define (match-str pstr nxt)
    (let ((plen (string-length pstr)))
      (lambda (str k kmatch)
	(and (<= (+ k plen) (string-length str))
	     (let loop ((i 0))
	       (cond ((= i plen)
		      (nxt str (+ k plen) (cons k kmatch)))
		     ((ch=? (string-ref pstr i)
			    (string-ref str (+ k i)))
		      (loop (+ i 1)))
		     (else #f)))))))
  (define (match-? nxt)
    (lambda (str k kmatch)
      (and (< k (string-length str))
	   (nxt str (+ k 1) (cons k kmatch)))))
  (define (match-set1 chrs)
    (let recur ((i 0))
      (cond ((= i (string-length chrs))
	     (lambda (ch) #f))
	    ((and (< (+ i 2) (string-length chrs))
		  (char=? #\- (string-ref chrs (+ i 1))))
	     (let ((nxt (recur (+ i 3))))
	       (lambda (ch)
		 (or (and (ch<=? ch (string-ref chrs (+ i 2)))
			  (ch<=? (string-ref chrs i) ch))
		     (nxt ch)))))
	    (else
	     (let ((nxt (recur (+ i 1)))
		   (chrsi (string-ref chrs i)))
	       (lambda (ch)
		 (or (ch=? chrsi ch) (nxt ch))))))))
  (define (match-set tok nxt)
    (let ((chrs (substring tok 1 (- (string-length tok) 1))))
      (if (and (positive? (string-length chrs))
	       (memv (string-ref chrs 0) '(#\^ #\!)))
	  (let ((pred (match-set1 (substring chrs 1 (string-length chrs)))))
	    (lambda (str k kmatch)
	      (and (< k (string-length str))
		   (not (pred (string-ref str k)))
		   (nxt str (+ k 1) (cons k kmatch)))))
	  (let ((pred (match-set1 chrs)))
	    (lambda (str k kmatch)
	      (and (< k (string-length str))
		   (pred (string-ref str k))
		   (nxt str (+ k 1) (cons k kmatch))))))))
  (define (match-* nxt)
    (lambda (str k kmatch)
      (let ((kmatch (cons k kmatch)))
	(let loop ((kk (string-length str)))
	  (and (>= kk k)
	       (or (nxt str kk kmatch)
		   (loop (- kk 1))))))))

  (let ((matcher
	 (let recur ((toks (glob:pattern->tokens pat)))
	   (if (null? toks)
	       match-end
	       (let ((pch (or (string=? (car toks) "")
			      (string-ref (car toks) 0))))
		 (case pch
		   ((#\?) (match-? (recur (cdr toks))))
		   ((#\*) (match-* (recur (cdr toks))))
		   ((#\[) (match-set (car toks) (recur (cdr toks))))
		   (else (match-str (car toks) (recur (cdr toks))))))))))
    (lambda (str) (matcher str 0 '()))))

(define (glob:caller-with-matches pat proc ch=? ch<=?)
  (define (glob:wildcard? pat)
    (cond ((string=? pat "") #f)
	  ((memv (string-ref pat 0) '(#\* #\? #\[)) #t)
	  (else #f)))
  (let* ((toks (glob:pattern->tokens pat))
	 (wild? (map glob:wildcard? toks))
	 (matcher (glob:make-matcher toks ch=? ch<=?)))
    (lambda (str)
      (let loop ((inds (matcher str))
		 (wild? wild?)
		 (res '()))
	(cond ((not inds) #f)
	      ((null? wild?)
	       (apply proc (reverse res)))
	      ((car wild?)
	       (loop (cdr inds)
		     (cdr wild?)
		     (cons (substring str (car inds) (cadr inds)) res)))
	      (else
	       (loop (cdr inds) (cdr wild?) res)))))))

(define (glob:make-substituter pattern template ch=? ch<=?)
  (define (wildcard? pat)
    (cond ((string=? pat "") #f)
	  ((memv (string-ref pat 0) '(#\* #\? #\[)) #t)
	  (else #f)))
  (define (countq val lst)
    (do ((lst lst (cdr lst))
	 (c 0 (if (eq? val (car lst)) (+ c 1) c)))
	((null? lst) c)))
  (let ((tmpl-literals (map (lambda (tok)
			      (if (wildcard? tok) #f tok))
			    (glob:pattern->tokens template)))
	(pat-wild? (map wildcard? (glob:pattern->tokens pattern)))
	(matcher (glob:make-matcher pattern ch=? ch<=?)))
    (or (= (countq #t pat-wild?) (countq #f tmpl-literals))
	(slib:error 'glob:make-substituter
		    "number of wildcards doesn't match" pattern template))
    (lambda (str)
      (let ((indices (matcher str)))
	(and indices
	     (let loop ((inds indices)
			(wild? pat-wild?)
			(lits tmpl-literals)
			(res '()))
	       (cond
		((null? lits)
		 (apply string-append (reverse res)))
		((car lits)
		 (loop inds wild? (cdr lits) (cons (car lits) res)))
		((null? wild?)		;this should never happen.
		 (loop '() '() lits res))
		((car wild?)
		 (loop (cdr inds) (cdr wild?) (cdr lits)
		       (cons (substring str (car inds) (cadr inds))
			     res)))
		(else
		 (loop (cdr inds) (cdr wild?) lits res)))))))))

;;@body
;;Returns a predicate which returns a non-false value if its string argument
;;matches (the string) @var{pattern}, false otherwise.  Filename matching
;;is like
;;@cindex glob
;;@dfn{glob} expansion described the bash manpage, except that names
;;beginning with @samp{.} are matched and @samp{/} characters are not
;;treated specially.
;;
;;These functions interpret the following characters specially in
;;@var{pattern} strings:
;;@table @samp
;;@item *
;;Matches any string, including the null string.
;;@item ?
;;Matches any single character.
;;@item [@dots{}]
;;Matches any one of the enclosed characters.  A pair of characters
;;separated by a minus sign (-) denotes a range; any character lexically
;;between those two characters, inclusive, is matched.  If the first
;;character following the @samp{[} is a @samp{!} or a @samp{^} then any
;;character not enclosed is matched.  A @samp{-} or @samp{]} may be
;;matched by including it as the first or last character in the set.
;;@end table
(define (filename:match?? pattern)
  (glob:make-matcher pattern char=? char<=?))
(define (filename:match-ci?? pattern)
  (glob:make-matcher pattern char-ci=? char-ci<=?))


;;@args pattern template
;;Returns a function transforming a single string argument according to
;;glob patterns @var{pattern} and @var{template}.  @var{pattern} and
;;@var{template} must have the same number of wildcard specifications,
;;which need not be identical.  @var{pattern} and @var{template} may have
;;a different number of literal sections. If an argument to the function
;;matches @var{pattern} in the sense of @code{filename:match??} then it
;;returns a copy of @var{template} in which each wildcard specification is
;;replaced by the part of the argument matched by the corresponding
;;wildcard specification in @var{pattern}.  A @code{*} wildcard matches
;;the longest leftmost string possible.  If the argument does not match
;;@var{pattern} then false is returned.
;;
;;@var{template} may be a function accepting the same number of string
;;arguments as there are wildcard specifications in @var{pattern}.  In
;;the case of a match the result of applying @var{template} to a list
;;of the substrings matched by wildcard specifications will be returned,
;;otherwise @var{template} will not be called and @code{#f} will be returned.
(define (filename:substitute?? pattern template)
  (cond ((procedure? template)
	 (glob:caller-with-matches pattern template char=? char<=?))
	((string? template)
	 (glob:make-substituter pattern template char=? char<=?))
	(else
	 (slib:error 'filename:substitute?? "bad second argument" template))))
(define (filename:substitute-ci?? pattern template)
  (cond ((procedure? template)
	 (glob:caller-with-matches pattern template char-ci=? char-ci<=?))
	((string? template)
	 (glob:make-substituter pattern template char-ci=? char-ci<=?))
	(else
	 (slib:error 'filename:substitute-ci?? "bad second argument" template))))

;;@example
;;((filename:substitute?? "scm_[0-9]*.html" "scm5c4_??.htm")
;; "scm_10.html")
;;@result{} "scm5c4_10.htm"
;;((filename:substitute?? "??" "beg?mid?end") "AZ")
;;@result{} "begAmidZend"
;;((filename:substitute?? "*na*" "?NA?") "banana")
;;@result{} "banaNA"
;;((filename:substitute?? "?*?" (lambda (s1 s2 s3) (string-append s3 s1)))
;; "ABZ")
;;@result{} "ZA"
;;@end example

;;@body
;;@var{str} can be a string or a list of strings.  Returns a new string
;;(or strings) similar to @code{str} but with the suffix string @var{old}
;;removed and the suffix string @var{new} appended.  If the end of
;;@var{str} does not match @var{old}, an error is signaled.
(define (replace-suffix str old new)
  (let* ((f (glob:make-substituter (list "*" old) (list "*" new)
				   char=? char<=?))
	 (g (lambda (st)
	      (or (f st)
		  (slib:error 'replace-suffix "suffix doesn't match:"
			      old st)))))
    (if (pair? str)
	(map g str)
	(g str))))

;;@example
;;(replace-suffix "/usr/local/lib/slib/batch.scm" ".scm" ".c")
;;@result{} "/usr/local/lib/slib/batch.c"
;;@end example

;;@args proc k
;;@args proc
;;Calls @1 with @2 arguments, strings returned by successive calls to
;;@code{tmpnam}.
;;If @1 returns, then any files named by the arguments to @1 are
;;deleted automatically and the value(s) yielded by the @1 is(are)
;;returned.  @2 may be ommited, in which case it defaults to @code{1}.
;;
;;@args proc suffix1 ...
;;Calls @1 with strings returned by successive calls to @code{tmpnam},
;;each with the corresponding @var{suffix} string appended.
;;If @1 returns, then any files named by the arguments to @1 are
;;deleted automatically and the value(s) yielded by the @1 is(are)
;;returned.
(define (call-with-tmpnam proc . suffi)
  (define (do-call paths)
    (let ((ans (apply proc paths)))
      (for-each (lambda (path) (if (file-exists? path) (delete-file path)))
		paths)
      ans))
  (cond ((null? suffi) (do-call (list (tmpnam))))
	((and (= 1 (length suffi)) (number? (car suffi)))
	 (do ((cnt (if (null? suffi) 0 (+ -1 (car suffi))) (+ -1 cnt))
	      (paths '() (cons (tmpnam) paths)))
	     ((negative? cnt)
	      (do-call paths))))
	(else (do-call (map (lambda (suffix) (string-append (tmpnam) suffix))
			    suffi)))))


;;"genwrite.scm" generic write used by pretty-print and truncated-print.
;; Copyright (c) 1991, Marc Feeley
;; Author: Marc Feeley (feeley@iro.umontreal.ca)
;; Distribution restrictions: none

(define genwrite:newline-str (make-string 1 #\newline))
;@
(define (generic-write obj display? width output)

  (define (read-macro? l)
    (define (length1? l) (and (pair? l) (null? (cdr l))))
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote quasiquote unquote unquote-splicing) (length1? tail))
        (else                                        #f))))

  (define (read-macro-body l)
    (cadr l))

  (define (read-macro-prefix l)
    (let ((head (car l)) (tail (cdr l)))
      (case head
        ((quote)            "'")
        ((quasiquote)       "`")
        ((unquote)          ",")
        ((unquote-splicing) ",@"))))

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
             (and (out genwrite:newline-str col) (spaces to 0))
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
        ((lambda let* letrec define) pp-LAMBDA)
        ((if set!)                   pp-IF)
        ((cond)                      pp-COND)
        ((case)                      pp-CASE)
        ((and or)                    pp-AND)
        ((let)                       pp-LET)
        ((begin)                     pp-BEGIN)
        ((do)                        pp-DO)
        (else                        #f)))

    (pr obj col 0 pp-expr))

  (if width
    (out genwrite:newline-str (pp obj 0))
    (wr obj 0)))

; (reverse-string-append l) = (apply string-append (reverse l))
;@
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
;;; Copyright (C) 1991-1993, 1996, 1999-2001 Aubrey Jaffer and Radey Shouman.
;
;Permission to copy this software, to modify it, to redistribute it,
;to distribute modified versions, and to use it for any purpose is
;granted, subject to the following restrictions and understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warranty or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(require 'string-case)
(require-if 'compiling 'generic-write)

;; Determine the case of digits > 9.  We assume this to be constant.
(define stdio:hex-upper-case? (string=? "-F" (number->string -15 16)))

;; Parse the output of NUMBER->STRING and pass the results to PROC.
;; PROC takes (SIGN-CHARACTER DIGIT-STRING EXPONENT-INTEGER . IMAGPART)
;; SIGN-CHAR will be either #\+ or #\-, DIGIT-STRING will always begin
;; with a "0", after which a decimal point should be understood.
;; If STR denotes a number with imaginary part not exactly zero,
;; 3 additional elements for the imaginary part are passed.
;; If STR cannot be parsed, return #F without calling PROC.
(define (stdio:parse-float str proc)
  (let ((n (string-length str)))
    (define (parse-error) #f)
    (define (prefix i cont)
      (if (and (< i (- n 1))
	       (char=? #\# (string-ref str i)))
	  (case (string-ref str (+ i 1))
	    ((#\d #\i #\e) (prefix (+ i 2) cont))
	    ((#\.) (cont i))
	    (else (parse-error)))
	  (cont i)))
    (define (sign i cont)
      (if (< i n)
	  (let ((c (string-ref str i)))
	    (case c
	      ((#\- #\+) (cont (+ i 1) c))
	      (else (cont i #\+))))))
    (define (digits i cont)
      (do ((j i (+ j 1)))
	  ((or (>= j n)
	       (not (or (char-numeric? (string-ref str j))
			(char=? #\# (string-ref str j)))))
	   (cont j (if (= i j) "0" (substring str i j))))))
    (define (point i cont)
      (if (and (< i n)
	       (char=? #\. (string-ref str i)))
	  (cont (+ i 1))
	  (cont i)))
    (define (exp i cont)
      (cond ((>= i n) (cont i 0))
	    ((memv (string-ref str i)
		   '(#\e #\s #\f #\d #\l #\E #\S #\F #\D #\L))
	     (sign (+ i 1)
		   (lambda (i sgn)
		     (digits i
			     (lambda (i digs)
			       (cont i
				     (if (char=? #\- sgn)
					 (- (string->number digs))
					 (string->number digs))))))))
	    (else (cont i 0))))
    (define (real i cont)
      (prefix
       i
       (lambda (i)
	 (sign
	  i
	  (lambda (i sgn)
	    (digits
	     i
	     (lambda (i idigs)
	       (point
		i
		(lambda (i)
		  (digits
		   i
		   (lambda (i fdigs)
		     (exp i
			  (lambda (i ex)
			    (let* ((digs (string-append "0" idigs fdigs))
				   (ndigs (string-length digs)))
			      (let loop ((j 1)
					 (ex (+ ex (string-length idigs))))
				(cond ((>= j ndigs) ;; Zero
				       (cont i sgn "0" 1))
				      ((char=? #\0 (string-ref digs j))
				       (loop (+ j 1) (- ex 1)))
				      (else
				       (cont i sgn
					     (substring digs (- j 1) ndigs)
					     ex))))))))))))))))))
    (real 0
	  (lambda (i sgn digs ex)
	    (cond
	     ((= i n) (proc sgn digs ex))
	     ((memv (string-ref str i) '(#\+ #\-))
	      (real i
		    (lambda (j im-sgn im-digs im-ex)
		      (if (and (= j (- n 1))
			       (char-ci=? #\i (string-ref str j)))
			  (proc sgn digs ex im-sgn im-digs im-ex)
			  (parse-error)))))
	     ((eqv? (string-ref str i) #\@)
	      ;; Polar form: No point in parsing the angle ourselves,
	      ;; since some transcendental approximation is unavoidable.
	      (let ((num (string->number str)))
		(if num
		    (stdio:parse-float
		     (number->string (real-part num))
		     (lambda (sgn digs ex)
		       (stdio:parse-float
			(number->string (imag-part num))
			(lambda (im-sgn im-digs im-ex)
			  (proc sgn digs ex im-sgn im-digs im-ex)))))
		    (parse-error))))
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
		 (let ((padlen (max 0 (- (or strip-0s ndigs) n))))
		   (if (zero? padlen)
		       str
		       (string-append str
				      (make-string padlen
						   (if (char-numeric?
							(string-ref str n))
						       #\0 #\#))))))
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
				    (if (> i n)
					(odd? (dig ndigs))
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
      (define (out* strs)
	(if (string? strs) (out strs)
	    (let out-loop ((strs strs))
	      (or (null? strs)
		  (and (out (car strs))
		       (out-loop (cdr strs)))))))

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
		(cond ((>= len width) (cons pre strs))
		      ((null? ss)
		       (cond (left-adjust
			      (cons pre
				    (append strs
					    (list (make-string
						   (- width len) #\space)))))
			     (leading-0s
			      (cons pre
				    (cons (make-string (- width len) #\0)
					  strs)))
			     (else
			      (cons (make-string (- width len) #\space)
				    (cons pre strs)))))
		      (else
		       (loop (+ len (string-length (car ss))) (cdr ss))))))
	    (define integer-convert
	      (lambda (s radix fixcase)
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
		(if fixcase (set! s (fixcase s)))
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
			    (else "???"))))
		(define (format-real signed? sgn digs exp . rest)
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
			      '("i"))))
		(or (stdio:parse-float str
				    (lambda (sgn digs expon . imag)
				      (apply pad
					     (apply format-real
						    signed
						    sgn digs expon imag))))
		    (pad "???"))))
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
		 (and
		  (out* (cond
			 ((<= width (string-length s)) s)
			 (left-adjust
			  (list
			   s (make-string (- width (string-length s)) #\ )))
			 (else
			  (list
			   (make-string (- width (string-length s))
					(if leading-0s #\0 #\ ))
			   s))))
		  (loop (cdr args)))))

		;; SLIB extension
	      ((#\a #\A)		;#\a #\A are pretty-print
	       (require 'generic-write)
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
	       (and (out* (integer-convert (car args) 10 #f))
		    (loop (cdr args))))
	      ((#\o #\O)
	       (and (out* (integer-convert (car args) 8 #f))
		    (loop (cdr args))))
	      ((#\x)
	       (and (out* (integer-convert
			   (car args) 16
			   (if stdio:hex-upper-case? string-downcase #f)))
		    (loop (cdr args))))
	       ((#\X)
	       (and (out* (integer-convert
			   (car args) 16
			   (if stdio:hex-upper-case? #f string-upcase)))
		    (loop (cdr args))))
	      ((#\b #\B)
	       (and (out* (integer-convert (car args) 2 #f))
		    (loop (cdr args))))
	      ((#\%) (and (out #\%) (loop args)))
	      ((#\f #\F #\e #\E #\g #\G #\k #\K)
	       (and (out* (float-convert (car args) fc)) (loop (cdr args))))
	      (else
	       (cond
		((end-of-format?) (incomplete))
		(else (and (out #\%) (out fc) (out #\?) (loop args))))))))
	 (else (and (out fc) (loop args)))))))))
;@
(define (fprintf port format . args)
  (let ((cnt 0))
    (apply stdio:iprintf
	   (lambda (x)
	     (cond ((string? x)
		    (set! cnt (+ (string-length x) cnt)) (display x port) #t)
		   (else (set! cnt (+ 1 cnt)) (display x port) #t)))
	   format args)
    cnt))
;@
(define (printf format . args)
  (apply stdio:fprintf (current-output-port) format args))
;@
(define (sprintf str format . args)
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

(define stdio:fprintf fprintf)

;;(do ((i 0 (+ 1 i))) ((> i 50)) (printf "%s\\n" (sprintf i "%#-13a:%#13a:%-13.8a:" "123456789" "123456789" "123456789")))
