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

(export pattern-streamer)

(export tbl-column-count)
(export tbl-row-count)
(export tbl-ref)
(export tbl-ref->number)

(export gnc:options->sxml)

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

(define (gnc:options->sxml options test-title uuid prefix)
  ;; options object -> sxml tree
  ;; test-title: str describing tests e.g. "test-trep"
  ;; uuid - str to locate report uuid
  ;; prefix - str describing each unit test e.g. "test disable filter"
  ;;
  ;; This function abstracts the report renderer. It also catches XML
  ;; parsing errors, dumping the options changed.
  ;;
  ;; It also dumps the render into /tmp/XX-YY.html where XX is the
  ;; test prefix and YY is the test title.

  (let* ((template (gnc:find-report-template uuid))
         (constructor (record-constructor <report>))
         (report (constructor uuid "bar" options #t #t #f #f ""))
         (renderer (gnc:report-template-renderer template))
         (document (renderer report))
         (sanitize-char (lambda (c)
                          (if (char-alphabetic? c) c #\-)))
         (fileprefix (string-map sanitize-char prefix))
         (filename (string-map sanitize-char test-title)))
    (gnc:html-document-set-style-sheet! document (gnc:report-stylesheet report))
    (if test-title
        (gnc:html-document-set-title! document test-title))
    (let* ((filename (format #f "/tmp/~a-~a.html" fileprefix filename))
           (render (gnc:html-document-render document))
           (outfile (open-file filename "w")))
      (display render outfile)
      (close-output-port outfile)
      (catch 'parser-error
        (lambda () (xml->sxml render))
        (lambda (k . args)
          (test-assert k #f)            ; XML parse error doesn't cause a crash but logs as a failure
          (format #t "see render output at ~a\n~a" filename (gnc:html-render-options-changed options #t)))))))
