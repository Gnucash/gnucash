;;;; http.scm: HTTP client library for Guile.
;;;;

(define-module (www http)
  :use-module (www url)
  :use-module (ice-9 regex))

;;;; 	Copyright (C) 1997 Free Software Foundation, Inc.
;;;; 
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2, or (at your option)
;;;; any later version.
;;;; 
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.
;;;; 
;;;; You should have received a copy of the GNU General Public License
;;;; along with this software; see the file COPYING.  If not, write to
;;;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;;;; Boston, MA 02111-1307 USA
;;;; 

;;; Compatibility

(or (defined? 'read-line)
    (use-modules (ice-9 rdelim)))


;;; Variables that affect HTTP usage.

(define-public http:version "HTTP/1.0")  ; bump up to 1.1 when ready
(define-public http:user-agent "GuileHTTP 0.1")

;;; An HTTP message is represented by a vector:
;;;	#(VERSION STATUS-CODE STATUS-TEXT HEADERS BODY)
;;;
;;; Each of VERSION, STATUS-CODE, STATUS-TEXT are strings.  HEADERS
;;; is an alist of headers and their contents.  BODY is a single string.

(define (http:make-message version statcode stattext headers body)
  (vector version statcode stattext headers body))

;;;; HTTP status predicates.
;;;
;;; (http:message-version MSG)
;;;	Returns the HTTP version in use in HTTP message MSG.
;;;
;;; (http:message-status-code MSG)
;;;	Returns the status code returned in HTTP message MSG.
;;;
;;; (http:message-status-text MSG)
;;;	Returns the text of the status line from HTTP message MSG.
;;;
;;; (http:message-status-ok? STATUS)
;;;	Returns #t if status code STATUS indicates a successful request,
;;;	#f otherwise.

(define-public (http:message-version msg)     (vector-ref msg 0))
(define-public (http:message-status-code msg) (vector-ref msg 1))
(define-public (http:message-status-text msg) (vector-ref msg 2))
(define-public (http:message-status-ok? msg)
  (http:status-ok? (http:status-code msg)))
(define-public (http:status-ok? status)
  (char=? #\2 (string-ref status 0)))

(define-public (http:message-body msg) (vector-ref msg 4))

;;; HTTP response headers functions
;;;
;;; An HTTP message header is represented here by a pair.  The CAR is a
;;; symbol representing the header name, and the CDR is a string
;;; containing the header text.  E.g.:
;;;
;;;	'((date . "Thu, 29 May 1997 23:48:27 GMT")
;;;	  (server . "NCSA/1.5.1")
;;;	  (last-modified . "Tue, 06 May 1997 18:32:03 GMT")
;;;	  (content-type . "text/html")
;;;	  (content-length . "8097"))
;;;
;;; Note: these symbols are all lowercase, although the original headers
;;; were mixed-case.  Clients using this library should keep this in
;;; mind, since Guile symbols are case-sensitive.
;;;
;;; FIXME: should headers with known semantics be parsed automatically?
;;;   I.e. should the Content-Length header automatically get string->number?
;;;   Should Date and Last-Modified headers be run through strptime?
;;;   It is advantageous to keep headers in a uniform format, but it may
;;;   be convenient to parse headers that have unambiguous meanings.
;;;
;;; (http:message-headers MSG)
;;;	Returns a list of the headers from HTTP message MSG.
;;; (http:message-header HEADER MSG)
;;;	Return the header field named HEADER from HTTP message MSG, or
;;;	#f if no such header is present in the message.

(define-public (http:message-headers msg) (vector-ref msg 3))
(define-public (http:message-header header msg)
  (http:fetch-header header (http:message-headers msg)))

(define (http:fetch-header header header-alist)
  (assq-ref header-alist header))

(define header-regex (make-regexp ": *"))

(define (http:header-parse hd)
  (let ((match (regexp-exec header-regex hd)))
    (cons (string->symbol
	   (apply string
		  (map char-downcase
		       (string->list (match:prefix match)))))
	  (match:suffix match))))

(define (parse-status-line statline)
  (let* ((first (string-index statline #\space))
	 (second (string-index statline #\space (1+ first))))
    (list (make-shared-substring statline 0 first)
	  (make-shared-substring statline (1+ first) second)
	  (make-shared-substring statline (1+ second)))))


;;; HTTP connection management functions.
;;;
;;; Open connections are cached on hostname in the connection-table.
;;; If an HTTP connection is already open to a particular host and TCP port,
;;; looking up the hostname and port number in connection-table will yield
;;; a Scheme port that may be used to communicate with that server.

(define connection-table '())

;; FIXME: you can only re-use a connection if the server sends the
;; Keep-Alive header, I think.  With these definitions, we were trying to
;; send more requests on connections the server assumed were dead.
;; (define (add-open-connection! host tcp-port port)
;;   (set! connection-table
;; 	(assoc-set! connection-table (cons host tcp-port) port)))
;; (define (get-open-connection host tcp-port)
;;   (assoc-ref connection-table (cons host tcp-port)))

(define (add-open-connection! host tcp-port port)
  #f)
(define (get-open-connection host tcp-port)
  #f)


;;; HTTP methods.
;;;
;;; Common methods: GET, POST etc.

(define-public (http:get url)
  ;; FIXME: if http:open returns an old connection that has been
  ;; closed remotely, this will fail.
  (http:request "GET" url))

;;; Connection-oriented functions:
;;;
;;; (http:open HOST [PORT])
;;;     Return an HTTP connection to HOST on TCP port PORT (default 80).
;;;     If an open connection already exists, use it; otherwise, create
;;;     a new socket.

(define-public (http:open host . args)
  (let ((port (cond ((null? args) 80)
		    ((not (car args)) 80)
		    (else (car args)))))
    (or (get-open-connection host port)
	(let* ((tcp (vector-ref (getproto "tcp") 2))
	       (addr (car (vector-ref (gethost host) 4)))
	       (sock (socket AF_INET SOCK_STREAM tcp)))
	  (connect sock AF_INET addr port)
	  (add-open-connection! host port sock)
	  sock))))

;;; (http:request METHOD URL [HEADERS [BODY]])
;;;	Submit an HTTP request.
;;;     URL is a structure returned by url:parse.
;;;     METHOD is the name of some HTTP method, e.g. "GET" or "POST".
;;;     The optional HEADERS and BODY arguments are lists of strings
;;;     which describe HTTP messages.  The `Content-Length' header
;;;     is calculated automatically and should not be supplied.
;;;
;;;	Example usage:
;;;	  (http:request "get" parsed-url
;;;			(list "User-Agent: GuileHTTP 0.1"
;;;			      "Content-Type: text/plain"))
;;;       (http:request "post" parsed-url
;;;			(list "User-Agent: GuileHTTP 0.1"
;;;			      "Content-Type: unknown/x-www-form-urlencoded")
;;;			(list "search=Gosper"
;;;			      "case=no"
;;;			      "max_hits=50"))

(define-public (http:request method url . args)
  (let ((host     (url:host url))
	(tcp-port (or (url:port url) 80))
	(path     (string-append "/" (or (url:path url) ""))))
    (let ((sock (http:open host tcp-port))
	  (request (string-append method " " path " " http:version))
	  (headers (if (pair? args) (car args) '()))
	  (body    (if (and (pair? args) (pair? (cdr args)))
		       (cadr args)
		       '())))
      (let* ((content-length
	      (apply +
		     (map (lambda (line)
			    (+ 2 (string-length line)))	; + 2 for CRLF
			  body)))
	     (headers (if (positive? content-length)
			  (cons (string-append "Content-Length: "
					       (number->string content-length))
				headers)
			  headers)))

	(with-output-to-port sock
	  (lambda ()
	    (display-with-crlf request)
	    (for-each display-with-crlf headers)
	    (display "\r\n")
	    (for-each display-with-crlf body)))

	;; parse and add status line
	;; also cons up a list of response headers
	(let* ((response-status-line (sans-trailing-whitespace
				      (read-line sock 'trim)))
	       (response-headers
		(let make-header-list ((ln (sans-trailing-whitespace
					    (read-line sock 'trim)))
				       (hlist '()))
		  (if (= 0 (string-length ln))
		      hlist
		      (make-header-list (sans-trailing-whitespace
					 (read-line sock 'trim))
					(cons (http:header-parse ln)
					      hlist)))))
	       (response-status-fields
		(parse-status-line response-status-line))
	       (response-version (car response-status-fields))
	       (response-code    (cadr response-status-fields))
	       (response-text    (caddr response-status-fields)))

	  ;; signal error if HTTP status is invalid
	  ;; (or (http:status-ok? response-code)
	  ;; (error 'http-status "HTTP server returned bad status"
	  ;;        response-status-line))
	  ;; Get message body: if Content-Length header was supplied, read
	  ;; that many chars.  Otherwise, read until EOF
	  
	  (let ((content-length (http:fetch-header
				 "content-length"
				 response-headers)))
	    (let ((response-body
		   (if (and content-length
			    (not (string-ci=? method "HEAD")))
		       (read-n-chars (string->number content-length) sock)
		       (with-output-to-string
			 (lambda ()
			   (while (not (eof-object? (peek-char sock)))
				  (display (read-char sock))))))))

	      ;; FIXME: what about keepalives?
	      (close-port sock)

	      (http:make-message response-version
				 response-code
				 response-text
				 response-headers
				 response-body))))))))



;;;; System interface cruft & string funcs

(define (read-n-chars num . port-arg)
  (let ((p (if (null? port-arg)
	       (current-input-port)
	       (car port-arg)))
	(s (make-string num)))
    (do ((i   0              (+ i 1))
	 (ch  (read-char p)  (read-char p)))
	((or (>= i num) (eof-object? ch)) s)
      (string-set! s i ch))))

(define (display-with-crlf line . p)
  (apply display line p)
  (apply display "\r\n" p))

;;; (sans-trailing-whitespace STR)
;;;	These are defined in module #/ice-9/string-fun, so this code
;;;	will prob.  be discarded when the module system and boot-9
;;;	settle down.

(define (sans-trailing-whitespace s)
  (let ((st 0)
	(end (string-length s)))
    (while (and (< 0 end)
		(char-whitespace? (string-ref s (1- end))))
	   (set! end (1- end)))
    (if (< end st)
	""
	(make-shared-substring s st end))))
