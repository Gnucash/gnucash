; IMPLEMENTS Substring search
; AUTHOR Ken Dickey
; DATE 1991 August 6
; LAST UPDATED
; NOTES
;Based on "A Very Fast Substring Search Algorithm", Daniel M. Sunday,
;CACM v33, #8, August 1990.
;;
;; SUBSTRING-SEARCH-MAKER takes a string (the "pattern") and returns a function
;; which takes a string (the "target") and either returns #f or the index in
;; the target in which the pattern first occurs as a substring.
;;
;; E.g.: ((substring-search-maker "test") "This is a test string")  -> 10
;;       ((substring-search-maker "test") "This is a text string")  -> #f

(define (substring-search-maker pattern-string)
  (define num-chars-in-charset 256)  ;; update this, e.g. for iso latin 1
  (define (build-shift-vector pattern-string)
    (let* ((pat-len (string-length pattern-string))
	   (shift-vec (make-vector num-chars-in-charset 
				   (+ pat-len 1)))
	   (max-pat-index (- pat-len 1)))
      (let loop ((index 0))
	(vector-set! shift-vec 
		     (char->integer 
		      (string-ref pattern-string index))
		     (- pat-len index))
	(if (< index max-pat-index)
	    (loop (+ index 1))
	    shift-vec))))
  (let ((shift-vec (build-shift-vector pattern-string))
	(pat-len   (string-length pattern-string)))
    (lambda (target-string)
      (let* ((tar-len (string-length target-string))
	     (max-tar-index (- tar-len 1))
	     (max-pat-index (- pat-len 1)))
	(let outer ( (start-index 0))
	  (if (> (+ pat-len start-index) tar-len)
	      #f
	      (let inner ( (p-ind 0) (t-ind start-index) )
		(cond
		 ((> p-ind max-pat-index)  ; nothing left to check
		  #f)       ; fail
		 ((char=? (string-ref pattern-string p-ind)
			  (string-ref target-string  t-ind))
		  (if (= p-ind max-pat-index)
		      start-index  ;; success -- return start index of match
		      (inner (+ p-ind 1) (+ t-ind 1)) ; keep checking
		      ))
		 ((> (+ pat-len start-index) max-tar-index) #f) ; fail
		 (else
		  (outer (+ start-index
			    (vector-ref 
			     shift-vec
			     (char->integer 
			      (string-ref target-string
					  (+ start-index pat-len)))))))))))))))

;;; Functions to split up strings
;;; Provides the generic facility to split based on *any* character
;;; We make use of splitting on spaces and on colons...

;;; Find the next occurance of [somechar] in the string [string] 
;;; starting at [startpos]


(define (split-on-somechar sourcestring somechar)
  (define (next-somechar string startpos endpos somechar)
    (let loop 
	; initialize
	((pos startpos))
      (cond
       ((>= pos endpos) endpos)   ; Reached end of string
       ((char=? (string-ref string pos) somechar) pos)  ; Reached "somechar"
       (else 
	(loop (+ pos 1))))))
  (let loop
      ((pos 0)
       (endpos (string-length sourcestring))
       (result '()))
    (cond
     ((>= pos endpos) result)
     (else
      (let ((nextwhatever 
	     (next-somechar sourcestring pos endpos somechar)))
	(loop
	 (+ nextwhatever 1)
	 endpos
	 (append result 
		 (list 
		  (substring sourcestring pos nextwhatever)))))))))
