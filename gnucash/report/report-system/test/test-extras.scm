;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-module (gnucash report report-system test test-extras))

(use-modules (gnucash gnc-module))
(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (sxml simple))
(use-modules (sxml xpath))

(export pattern-streamer)

(export tbl-column-count)
(export tbl-row-count)
(export tbl-ref)
(export tbl-ref->number)

;;
;; Table parsing
;;
(use-modules (ice-9 regex))
(use-modules (ice-9 streams))

(define (values-for-keywords pos regex-list text)
  (make-stream (lambda (pos-keywords-pair)
		 (let ((current-pos (car pos-keywords-pair))
			(regex-list (cdr pos-keywords-pair)))
		   (if (null? regex-list)
		       '()
		       (let ((match (string-match (caar regex-list) text current-pos)))
			 (if (not match)
			     '()
			     (let ((new-state (cons (match:end match)
						    (cdr regex-list)))
				   (next-value (cons (match:end match)
						     (map (lambda (item)
							    (match:substring match item))
							  (cdar regex-list)))))
			       (cons next-value new-state)))))))
	       (cons pos regex-list)))

(define (pattern-streamer start-text regex-list text)
  (define (stream-next index)
    ;;(format #t "Next.  Index: ~a\n" index)
    (let ((head-index (string-contains text start-text index)))
      ;; (format #t "head index ~a ~a --> ~a\n" start-text index head-index)
      (if (not head-index) '()
	  (let ((values (stream->list (values-for-keywords head-index regex-list text))))
	    (if (null? values) '()
		(let ((new-state (car (car (last-pair values))))
		      (next-value (map cdr values)))
		  (cons next-value new-state)))))))
  ;;(format #t "Stream ~a\n" text)
  (make-stream stream-next 0))

;; silly table functions
(define (tbl-column-count tbl)
  (length (car tbl)))

(define (tbl-row-count tbl)
  (length tbl))

(define (tbl-ref tbl row-index column-index)
  (list-ref (list-ref tbl row-index) column-index))

(define (tbl-ref->number tbl row-index column-index)
  (string->number (car (tbl-ref tbl row-index column-index))))

(export gnc:options->render)
(define (gnc:options->render uuid options prefix test-title)
  ;; uuid - str to locate report uuid
  ;; options - gnc:options object
  ;; prefix - str describing tests e.g. "test-trep"
  ;; test-title: str describing each unit test e.g. "test disable filter"
  ;;
  ;; outputs: string
  ;;
  ;; This function abstracts the report renderer, producing a string. It
  ;; can be useful for reports which may not valid XML.
  ;;
  ;; It also dumps the render into /tmp/XX-YY.html where XX is the
  ;; test prefix and YY is the test title.

  (let* ((template (gnc:find-report-template uuid))
         (constructor (record-constructor <report>))
         (report (constructor uuid "bar" options #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template))
         (document (renderer report))
         (sanitize-char (lambda (c)
                          (if (or (char-alphabetic? c)
                                  (char-numeric? c)) c #\-))))
    (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
    (if test-title
        (gnc:html-document-set-title! document test-title))
    (let ((render (gnc:html-document-render document)))
      (with-output-to-file (format #f "/tmp/~a-~a.html"
                                   (string-map sanitize-char prefix)
                                   (string-map sanitize-char test-title))
        (lambda ()
          (display render)))
      render)))

(export gnc:options->sxml)
(define (gnc:options->sxml uuid options prefix test-title)
  ;; This functions calls the above gnc:options->render to render
  ;; report.  Then report is converted to SXML.  It catches XML
  ;; parsing errors, dumping the options changed.
  (let ((render (gnc:options->render uuid options prefix test-title)))
    (catch 'parser-error
      (lambda () (xml->sxml render
                            #:trim-whitespace? #t
                            #:entities '((nbsp . "\xa0"))))
      (lambda (k . args)
        (format #t "*** XML error: ~a ~a\n~a"
                prefix test-title
                (gnc:html-render-options-changed options #t))
        (throw k args)))))

(export sxml->table-row-col)
(define (sxml->table-row-col sxml tbl row col)
  ;; sxml - sxml input tree
  ;; tbl - table number (e.g. 2 = second table in tree)
  ;; row - row number (negative counts from bottom) or #f (all rows)
  ;;       or zero (retrieves <th> headers)
  ;; col - col number (negative counts from right) or all cols
  ;;
  ;; output: list-of-string
  (let* ((tbl-path `(table ,tbl))
         (row-path (if (and row (not (zero? row))) `(tr ,row) 'tr))
         (col-tag  (if (and row (zero? row)) 'th 'td))
         (col-path (if col `(,col-tag ,col) col-tag))
         (xpath `(// ,tbl-path // ,row-path // ,col-path // *text*)))
    ((sxpath xpath) sxml)))
