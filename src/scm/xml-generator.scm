;;;;;;;;;;;;;
;;;; $Id$
;;;;;;;;;;;;;  Generating XML out of Scheme Lists

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

(gnc:support "xml-generator.scm")

;;;;;;;;;;;;;
;;;; by Christopher Browne
;;;; <cbbrowne@hex.net>, <cbbrowne@ntlug.org>
;;;;
;;;; This was created for GnuCash to assist in creating
;;;; XML output to generate spreadsheets readable by
;;;; Gnumeric.
;;;;
;;;; The model is that an element consists of a list with
;;;; three entries.  Elements are created thus:
;;;; (define (make-xml-element tag attributes children)
;;;;    (list tag attributes children))
;;;; - The first entry is the tag name.
;;;; - The second entry optionally consists of an association list
;;;;   containing the attributes of the element, or is #f.
;;;; - The third entry is either a list of children, or is #f.
;;;;
;;;; Notable idiosyncracies aka "features" aka "misfeatures":
;;;; - All elements may come in the form of symbols, strings, or
;;;;   numbers.  output-xml-element (and helpers) transform these all
;;;;   into strings.
;;;; - It is possible that efficiency could be improved by memoizing
;;;;   the strings that get generated.  That way, we don't need to
;;;;   generate a new string each time a symbol gets hit.
;;;; - The "children" can have three values:
;;;;   a) #f, indicating that there are no children, as with:
;;;;   (NoEndTag ((Att1 . 1) (Att2 . 2)) #f) which turns into
;;;;    <NoEndTag Att1="1" Att2="2"/>
;;;;   b) It may be a simple attribute, like "Contents" or 1.5, as
;;;;   with (SimpleEndTag #f "Contents") which transforms to:
;;;;    <SimpleEndTag>Contents</SimpleEndTag>
;;;;   c) Otherwise, it must consist of a list of elements, thusly:
;;;;   (Parent #f ((Child #f Value1) (Child #f Value2)) which turns
;;;;   to:  <Parent> <Child>Value1</Child> <Child>Value2</Child> </Parent>
;;;;
;;;;  Usage
;;;; -------
;;;; The driver of it is (output-xml-element element port).
;;;; One might output an XML document with a root node, ROOT, thus:
;;;;(let ((port (open-output-file "/tmp/sampleoutput")))
;;;;  (display "<?xml version=\"1.0\"?>" port)
;;;;  (newline port)
;;;;  (output-xml-element ROOT port)
;;;;  (close-output-port port))
;;;;
;;;; If you have a Very Large Document, you might not want to
;;;; construct the whole document as One Big List; 
;;;; output-xml-element will be useful for generating subtree output.
;;;; Your control structure will need to duplicate the structure of
;;;; output-xml-element.  Alternatively, if "children" could is a thunk
;;;; (function with no arguments),  invoking output-xml-element
;;;; internally as needed, the "children" can be an XML generator.

(define xml-indentation 0)

(define (xml-display x port)
  (if port
      (display x port)
      (display x)))

(define (xml-newline port)
  (if port
      (newline port)
      (newline)))

(define (make-tabs port)
  (let loop
      ((i 0))
    (if (>= i xml-indentation)
        #f
        (begin
          (xml-display " " port)
          (loop (+ i 1)))))
  (set! xml-indentation (+ xml-indentation 1)))

(define (output-xml-element-name elname port)
  (xml-newline port)
  (make-tabs port)
  (xml-display 
   (string-append
    "<"
    (element-to-string elname))
   port))


(define (output-xml-element-name-end elname port)
  (set! xml-indentation (- xml-indentation 1))
  (xml-display 
   (string-append
    "</"
    (element-to-string elname)
    ">")
   port))

(define (output-xml-attribute att port)
;  (display "output-xml-attribute: ") (display attribute) (newline)
  (xml-display (string-append
            " "
            (element-to-string (car att))
            "=\""
            (element-to-string (cdr att))
            "\"")
	   port))

(define (element-to-string obj)
;  (display "[element-to-string: ") (display obj) (display "]") (newline)
  (cond
   ((string? obj) obj)
   ((symbol? obj) (symbol->string obj))
   ((number? obj) (number->string obj))
   (else 
    (string-append "[ERROR in element-to-string: "
                   (list->string (list obj))
                   " not a symbol, string or number.]"))))

(define (output-xml-attributes attributes port)
;(display "output-xml-attributes: ") (display attributes) (newline)
  (if attributes
      (for-each 
       (lambda (attribute) 
	 (output-xml-attribute attribute port)) 
       attributes)))

(define (output-xml-children children port)
;  (display "[output-xml-children: ") (display children) (display "]")(newline)
  (cond
   ((list? children)
    (for-each (lambda (child) 
		(output-xml-element child port))
	      children))
   (else
     (xml-display (element-to-string children) port))))

(define (output-xml-element element port)
  (let ((elname (car element))
        (attributes (cadr element))
        (children (caddr element)))
    (output-xml-element-name elname port)
    (output-xml-attributes attributes port)
    (cond
     ((not children)           ;;; If children is blank
      (xml-display "/>" port)) ;;; Short result 
     ((procedure? children)    ;;; If children is a function
      (xml-display ">" port)   
      (children port)          ;;; Invoke the function
      (output-xml-element-name-end elname port))
     (else
      (xml-display ">" port)
      (output-xml-children children port)
      (output-xml-element-name-end elname port)))))

(define (xml-element tag attributes children)
  (list tag attributes children))

(define (xml-attribute name value)
  (cons name value))

(define (xml-attributes . alist)
  alist)
;;;  (if (> 0 (length alist))   ;;; If there's anything in the list
;;;      alist                  ;;; Return the list
;;;      #f))                   ;;; Otherwise, blank to #f

(define no-attributes
  (xml-attributes))

(define no-children
  #f)
