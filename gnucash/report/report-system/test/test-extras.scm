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

  (let* ((template (or (gnc:find-report-template uuid)
                       (error "report not found:" uuid)))
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
      (call-with-output-file (format #f "/tmp/~a-~a.html"
                                   (string-map sanitize-char prefix)
                                   (string-map sanitize-char test-title))
        (lambda (p)
          (display render p)))
      render)))

(define (strip-string s1 s2)
  (let loop ((str s1)
             (res '()))
    (let ((startpos (string-contains str (format #f "<~a" s2)))
          (endpos (string-contains str (format #f "</~a>" s2))))
      (if (and startpos endpos)
          (loop (substring str (+ endpos (string-length s2) 3))
                (cons (substring str 0 startpos) res))
          (string-concatenate-reverse (cons str res))))))

(export gnc:options->sxml)
(define* (gnc:options->sxml uuid options prefix test-title #:key strip-tag)
  ;; This functions calls the above gnc:options->render to render
  ;; report.  Then report is converted to SXML.  It catches XML
  ;; parsing errors, dumping the options changed. Also optionally strip
  ;; an HTML tag from the render, e.g. <script>...</script>
  (let ((render (gnc:options->render uuid options prefix test-title)))
    (catch 'parser-error
      (lambda () (xml->sxml (if strip-tag
                                (strip-string render strip-tag)
                                render)
                            #:trim-whitespace? #t
                            #:entities '((nbsp . "\xa0")
                                         (ndash . "­"))))
      (lambda (k . args)
        (format #t "*** XML error: ~a ~a: ~a / ~a\n~a"
                prefix test-title k args
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
