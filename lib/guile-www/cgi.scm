;;;; cgi.scm: Common Gateway Interface support for WWW scripts.

(define-module (www cgi)
  :use-module (www url))

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

(define form-variables '())

;;; CGI environment variables.
;;; Should these all be public?

(define-public cgi-server-software-type #f)
(define-public cgi-server-software-version #f)
(define-public cgi-server-hostname #f)
(define-public cgi-gateway-interface #f)
(define-public cgi-server-protocol-name #f)
(define-public cgi-server-protocol-version #f)
(define-public cgi-server-port #f)
(define-public cgi-request-method #f)
(define-public cgi-path-info #f)
(define-public cgi-path-translated #f)
(define-public cgi-script-name #f)
(define-public cgi-query-string #f)
(define-public cgi-remote-host #f)
(define-public cgi-remote-addr #f)
(define-public cgi-authentication-type #f)
(define-public cgi-remote-user #f)
(define-public cgi-remote-ident #f)
(define-public cgi-content-type #f)
(define-public cgi-content-length #f)
(define-public cgi-http-accept-types #f)
(define-public cgi-http-user-agent #f)


;;; CGI high-level interface
;;;
;;; A typical CGI program will first call (cgi:init) to initialize
;;; the environment and read in any data returned from a form.  Form
;;; data can be extracted conveniently with these functions:
;;;
;;; (cgi:values NAME)
;;;	Fetch any values associated with NAME found in the form data.
;;;	Returned value is a list, even if it contains only one element.
;;; (cgi:value NAME)
;;;	Fetch only the CAR from (cgi:values NAME).  Convenient for when
;;;	you are certain that NAME is associated with only one value.

(define-public (cgi:init)
  (init-environment)
  (and cgi-content-length
       (parse-form (read-raw-form-data)))
  (and cgi-query-string
       (parse-form cgi-query-string)))

(define-public (cgi:values name)
  (assoc-ref form-variables name))

(define-public (cgi:value name)
  ;; syntactic sugar for obtaining just one value from a particular key
  (let ((values (cgi:values name)))
    (and values (car values))))

(define-public (cgi:names) (map car form-variables))

(define-public (cgi:form-data?) (not (null? form-variables)))


;;;; Internal functions.
;;;;
;;;; (parse-form DATA): parse DATA as raw form response data, adding
;;;;   values as necessary to `form-variables'.
;;;; (read-raw-form-data): read in `content-length' bytes from stdin
;;;; (init-environment): initialize CGI environment from Unix env vars.

(define (parse-form raw-data)
  ;; get-name and get-value are used to parse individual `name=value' pairs.
  ;; Values are URL-encoded, so url:decode must be called on each one.
  (define (get-name pair)
    (let ((p (string-index pair #\=)))
      (and p (make-shared-substring pair 0 p))))
  (define (get-value pair)
    (let ((p (string-index pair #\=)))
      (and p (url:decode (make-shared-substring pair (+ p 1))))))
  (for-each (lambda (pair)
	      (let* ((name (get-name pair))
		     (value (get-value pair))
		     (old-value (cgi:values name)))
		(set! form-variables
		      (assoc-set! form-variables
				  name
				  (cons value (or old-value '()))))))
	    (separate-fields-discarding-char #\& raw-data)))

(define (read-raw-form-data)
  (and cgi-content-length (read-n-chars cgi-content-length)))

(define (init-environment)

  ;; SERVER_SOFTWARE format: name/version
  (let ((server-software (getenv "SERVER_SOFTWARE")))
    (if server-software
	(let ((slash (string-index server-software #\/)))
	  (set! cgi-server-software-type    (substring server-software 0 slash))
	  (set! cgi-server-software-version (substring server-software (1+ slash))))))

  (set! cgi-server-hostname	   (getenv "SERVER_NAME"))
  (set! cgi-gateway-interface	   (getenv "GATEWAY_INTERFACE"));"CGI/revision"

  (let* ((server-protocol (getenv "SERVER_PROTOCOL")))
    (if server-protocol
	(let ((slash (string-index server-protocol #\/)))
	  (set! cgi-server-protocol-name     (substring server-protocol 0 slash))
	  (set! cgi-server-protocol-version  (substring server-protocol (1+ slash))))))

  (let ((port (getenv "SERVER_PORT")))
    (set! cgi-server-port (and port (string->number port))))

  (set! cgi-request-method	   (getenv "REQUEST_METHOD"))
  (set! cgi-path-info		   (getenv "PATH_INFO"))
  (set! cgi-path-translated	   (getenv "PATH_TRANSLATED"))
  (set! cgi-script-name		   (getenv "SCRIPT_NAME"))
  (set! cgi-remote-host		   (getenv "REMOTE_HOST"))
  (set! cgi-remote-addr		   (getenv "REMOTE_ADDR"))
  (set! cgi-authentication-type	   (getenv "AUTH_TYPE"))
  (set! cgi-remote-user		   (getenv "REMOTE_USER"))
  (set! cgi-remote-ident	   (getenv "REMOTE_IDENT"))
  (set! cgi-content-type	   (getenv "CONTENT_TYPE"))
  (set! cgi-query-string	   (getenv "QUERY_STRING"))

  (and cgi-query-string
       (string-null? cgi-query-string)
       (set! cgi-query-string #f))

  (let ((contlen (getenv "CONTENT_LENGTH")))
    (set! cgi-content-length (and contlen (string->number contlen))))

  ;; HTTP_ACCEPT is a list of MIME types separated by commas.
  (let ((types (getenv "HTTP_ACCEPT")))
    (set! cgi-http-accept-types
	  (and types (separate-fields-discarding-char #\, types))))

  ;; HTTP_USER_AGENT format: software/version library/version.
  (set! cgi-http-user-agent		   (getenv "HTTP_USER_AGENT")))


;;; System I/O and low-level stuff.

(define (read-n-chars num . port-arg)
  (let ((p (if (null? port-arg)
	       (current-input-port)
	       (car port-arg)))
	(s (make-string num)))
    (do ((i   0              (+ i 1))
	 (ch  (read-char p)  (read-char p)))
	((or (>= i num) (eof-object? ch)) s)
      (string-set! s i ch))))

;; This is defined in #/ice-9/string-fun, but the interface is
;; weird, the semantics perverse, and it doesn't work.  We use
;; a working copy here.
(define (separate-fields-discarding-char ch str)
  (let loop ((fields '())
             (str str))
    (let ((pos (string-rindex str ch)))
      (if pos
	  (loop (cons (make-shared-substring str (+ 1 pos)) fields)
		(make-shared-substring str 0 pos))
	  (cons str fields)))))
