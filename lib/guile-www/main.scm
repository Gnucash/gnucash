;;;; www/main.scm: general WWW navigation aids.

(define-module (www main)
  :use-module (www http)
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

(define dispatch-table
  (acons 'http http:get '()))

;;; (www:get URL)
;;;   parse a URL into portions, open a connection, and retrieve
;;;   selected document

(define-public (www:set-protocol-handler! proto handler)
  (set! dispatch-table
	(assq-set! dispatch-table proto handler)))

(define-public (www:get url-str)
  (let ((url (url:parse url-str)))
    ;; get handler for this protocol
    (case (url:scheme url)
      ((http) (let ((msg (http:get url)))
		  (http:message-body msg)))
      (else
       (let ((handle (assq-ref dispatch-table (url:scheme url))))
	 (if handle
	     (handle (url:host url)
		     (url:port url)
		     (url:path url))
	     (error "unknown URL scheme" (url:scheme url))))))))

