;;; $Id$
(define (directory? path)
  ;; This follows symlinks normally.
  (let* ((status (false-if-exception (stat path)))
         (type (if status (stat:type status) #f)))
    (eq? type 'directory)))

(define (filteroutnulls lst)
  (cond
   ((null? lst) '())
   ((eq? (car lst) #f) (filteroutnulls (cdr lst)))
   (else
    (cons (car lst) (filteroutnulls (cdr lst))))))

(if testing?
    (let ((i1 '(a b #f f g h #f #f (c d e #f #f) #f))
	  (i2 '(#f #f #f #f)))
      (testing  "filteroutnulls"
		i1
		'(a b f g h (c d e #f #f))
		(filteroutnulls i1))
	
      (testing  "filteroutnulls"
		i2
		'()
		(filteroutnulls i2))))

(define (atom? x)
  (and
   (not (pair? x))
   (not (null? x))))

(define (flatten lst)
  (cond
   ((null? lst) '())
   ((atom? lst) (list lst))
   ((list? lst) 
    (append (flatten (car lst)) 
	    (flatten (cdr lst))))
   (else lst)))

(if testing?
    (let ((input '(a b (c d (e (f) (g) (h i (j k))) l m no p))))
      (testing "flatten" 
	       input
	       '(a b c d e f g h i j k l m no p)
	       (flatten input))))

(define (striptrailingwhitespace line)
  (let
      ((stringsize (string-length line)))
    (if
     (< stringsize 1)
     ""
     (let*
	 ((lastchar (string-ref line (- stringsize 1))))
       (if
	(char-whitespace? lastchar)
	(striptrailingwhitespace (substring line 0  (- stringsize 1)))
	line)))))

(if testing?
    (begin
      (newline)
      (display "Test striptrailingwhitespace") (newline)
      (let ((tstring "Here's a string        



"))
      (display tstring) (newline)
      (display "Result:") (display (striptrailingwhitespace tstring)) (newline))))

(define (strip-qif-header line)
  (substring line 1 (string-length line)))

;;; Check amount to see if it's:
;;; a) "European" where one separates thousands using a period, and
;;; the decimal is represented via a comma, or if this be
;;; b) "American" where commas indicate groupings of digits, and
;;; decimal is a "."

(define (thousands-separator numstring)
  (define findcomma (substring-search-maker ","))
  (define findperiod (substring-search-maker "."))
  (let
      ((firstcomma (findcomma numstring))
       (firstperiod (findperiod numstring)))
    (cond
     ((not firstcomma)   ;; No commas found
      #\,)
     ((not firstperiod)  ;; No periods found
      #\.)
     ((> firstperiod firstcomma)  ;; First comma before first period
      #\,)
     ((< firstperiod firstcomma)  ;; First comma after first period
      #\.)
     (else #f))))

(if testing?
    (begin
      (let ((num "1,234,56.78"))
	(testing "thousands-separator"
		 num
		 #\,
		 (thousands-separator num)))
      (let ((num "1 234 56,78"))
	(testing "thousands-separator"
		 num
		 #\.
		 (thousands-separator num)))
      (let ((num "1 234 56.78"))
	(testing "thousands-separator"
		 num
		 #\,
		 (thousands-separator num)))
      (let ((num ".78"))
	(testing "thousands-separator"
		 num
		 #\,
		 (thousands-separator num)))
      (let ((num ""))
	(testing "thousands-separator"
		 num
		 #\,
		 (thousands-separator num)))
      (let ((num "1.234.56,78"))
	(testing "thousands-separator"
		 num
		 #\.
		 (thousands-separator num)))))

(define (string-join lst joinstr)
  (let ((len (length lst)))
    (cond 
     ((< 1 len)
      (string-append (car lst) joinstr (string-join (cdr lst) joinstr)))
     ((= 1 len)
      (car lst))
     (else
      ""))))

(define (numerizeamount amount-as-string)
  (let*
      (
       ;;; First, chop out spaces
       (spacesplit (split-on-somechar amount-as-string #\space))
       (despaced (apply string-append spacesplit))
       ;;; Second, separate based on #\, or #\.
       (curr-separator (thousands-separator despaced))
       (decimal-separator  (if (char=? curr-separator #\,)
			       #\.
			       #\,))
       (trio-split (split-on-somechar despaced curr-separator))
       ;;; Reform into a string
       (without-trios (apply string-append trio-split))
       ;;; Now, split on decimal separator...
       (decimal-split (split-on-somechar without-trios
					 decimal-separator)) 
       (rejoin-decimal (string-join decimal-split "."))
       ;;; Lastly, convert to a number
       (numeric   (string->number rejoin-decimal)))
    (if
     numeric				; did the conversion succeed?
     numeric				; Yup.  Return the value
     amount-as-string)))		; Nope.  Return the original value.

(if testing?
    (begin
      (let ((num " 1,234,56.78"))
	(testing "numerizeamount"
		 num
		 123456.78
		 (numerizeamount num)))
      (let ((num "1 .2 34.5 6,78"))
	(testing "numerizeamount"
		 num
		 123456.78
		 (numerizeamount num)))))

(define (find-min-cdr mlist)
  (if
   (null? mlist)
   #f
   (let
       ((first (car mlist))
	(rest (find-min-cdr (cdr mlist))))
     (if
      rest   ;;; Found a value for rest
      (if (> (cdr first) (cdr rest))
	  rest
	  first)
      first))))

(define (shorten-to-best! keep-top-n picklist)
  (let ((shortened '()))
    (let loop ((count keep-top-n))
      (if (= count 0)  ;;; No room left...
	  shortened    ;;; Return the present short list
	  (let ((bestitem (find-min-cdr picklist)))
	    (if bestitem
		(begin 
		  (if (> 9999 (cdr bestitem))
		      (set! shortened (cons (car bestitem) shortened)))
		  (set-cdr! bestitem 999999)
		  (loop (- count 1)))))))))

;;;; Test shorten-to-best:

(if testing? 
    (let  
	((alist '((a . 10)  (b . 15) (c . 20) (d . 12) (e . 7))))
      (testing "shorten-to-best! 3"
	       alist
	       '(d a e)
	       (shorten-to-best! 3 alist))))

;;;; Simple lookup scheme; can be turned into a hash table If Need Be.
;;; Initialize lookup table
(define (initialize-hashtable . size)
  (make-vector
   (if (null? size)
       313
       (car size))
   '()))