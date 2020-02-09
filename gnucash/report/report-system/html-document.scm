;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; html-document.scm : generate HTML programmatically, with support
;; for simple style elements.
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
;;
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:module-load "gnucash/html" 0)
(use-modules (ice-9 match))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-document> class
;;  this is the top-level object representing an entire HTML document.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-document>
  (make-record-type "<html-document>"
                    '(style-sheet style-stack style style-text title headline objects)))

(define gnc:html-document?
  (record-predicate <html-document>))

(define gnc:make-html-document-internal
  (record-constructor <html-document>))

(define (gnc:make-html-document)
  (gnc:make-html-document-internal
   #f                    ;; the stylesheet
   '()                   ;; style stack
   (gnc:make-html-style-table) ;; document style info
   #f                    ;; style text
   ""                    ;; document title
   #f                    ;; headline
   '()                   ;; subobjects
   ))

(define gnc:html-document-set-title!
  (record-modifier <html-document> 'title))

(define gnc:html-document-title
  (record-accessor <html-document> 'title))

(define gnc:html-document-set-headline!
  (record-modifier <html-document> 'headline))

(define gnc:html-document-headline
  (record-accessor <html-document> 'headline))

(define gnc:html-document-set-style-sheet!
  (record-modifier <html-document> 'style-sheet))

(define gnc:html-document-set-style-sheet!
  (record-modifier <html-document> 'style-sheet))

(define gnc:html-document-style-sheet
  (record-accessor <html-document> 'style-sheet))

(define gnc:html-document-set-style-stack!
  (record-modifier <html-document> 'style-stack))

(define gnc:html-document-style-stack
  (record-accessor <html-document> 'style-stack))

(define gnc:html-document-set-style-text!
  (record-modifier <html-document> 'style-text))

(define gnc:html-document-style-text
  (record-accessor <html-document> 'style-text))

(define gnc:html-document-set-style-internal!
  (record-modifier <html-document> 'style))

(define gnc:html-document-style
  (record-accessor <html-document> 'style))

(define gnc:html-document-set-objects!
  (record-modifier <html-document> 'objects))

(define gnc:html-document-objects
  (record-accessor <html-document> 'objects))

(define gnc:html-document?
  (record-predicate <html-document>))

(define (gnc:html-document-set-style! doc tag . rest)
  (gnc:html-style-table-set!
   (gnc:html-document-style doc) tag
   (if (and (= (length rest) 2)
            (procedure? (car rest)))
       (apply gnc:make-html-data-style-info rest)
       (apply gnc:make-html-markup-style-info rest))))

(define (gnc:html-document-tree-collapse . tree)
  (let lp ((e tree) (accum '()))
    (cond ((null? e) accum)
          ((pair? e) (fold lp accum e))
          ((string? e) (cons e accum))
          (else (cons (object->string e) accum)))))

;; first optional argument is "headers?"
;; returns the html document as a string, I think.
(define (gnc:html-document-render doc . rest)
  (let ((stylesheet (gnc:html-document-style-sheet doc))
        (headers? (or (null? rest) (car rest)))
        (style-text (gnc:html-document-style-text doc)))

    (if stylesheet
        ;; if there's a style sheet, let it do the rendering
        (gnc:html-style-sheet-render stylesheet doc headers?)

        ;; otherwise, do the trivial render.
        (let* ((retval '())
               (push (lambda (l) (set! retval (cons l retval))))
               (objs (gnc:html-document-objects doc))
               (work-to-do (length objs))
               (work-done 0)
               (title (gnc:html-document-title doc)))
          ;; compile the doc style
          (gnc:html-style-table-compile (gnc:html-document-style doc)
                                        (gnc:html-document-style-stack doc))
          ;; push it
          (gnc:html-document-push-style doc (gnc:html-document-style doc))
          (gnc:report-render-starting (gnc:html-document-title doc))
          (when headers?
            ;;This is the only place where <html> appears
            ;;with the exception of 2 reports:
            ;;./share/gnucash/scm/gnucash/report/taxinvoice.eguile.scm:<html>
            ;;./share/gnucash/scm/gnucash/report/balsheet-eg.eguile.scm:<html>

            (push "<!DOCTYPE html>\n")
            (push "<html dir='auto'>\n")
            (push "<head>\n")
            (push "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />\n")
            (if style-text
                (push (list "</style>" style-text "<style type=\"text/css\">\n")))
            (if (not (string-null? title))
                (push (list "</title>" title "<title>\n")))
            (push "</head>")

            ;; this lovely little number just makes sure that <body>
            ;; attributes like bgcolor get included
            (push ((gnc:html-markup/open-tag-only "body") doc)))

          ;; now render the children
          (for-each
           (lambda (child)
               (push (gnc:html-object-render child doc))
               (set! work-done (+ 1 work-done))
               (gnc:report-percent-done (* 100 (/ work-done work-to-do))))
           objs)

          (when headers?
            (push "</body>\n")
            (push "</html>\n"))

          (gnc:report-finished)
          (gnc:html-document-pop-style doc)
          (gnc:html-style-table-uncompile (gnc:html-document-style doc))

          (string-concatenate (gnc:html-document-tree-collapse retval))))))


(define (gnc:html-document-push-style doc style)
  (gnc:html-document-set-style-stack!
       doc (cons style (gnc:html-document-style-stack doc))))

(define (gnc:html-document-pop-style doc)
  (if (not (null? (gnc:html-document-style-stack doc)))
      (gnc:html-document-set-style-stack!
           doc (cdr (gnc:html-document-style-stack doc)))))

(define (gnc:html-document-add-object! doc obj)
  (gnc:html-document-set-objects!
   doc
   (append (gnc:html-document-objects doc)
           (list (gnc:make-html-object obj)))))

(define (gnc:html-document-append-objects! doc objects)
  (gnc:html-document-set-objects!
   doc
   (append (gnc:html-document-objects doc) objects)))

(define (gnc:html-document-fetch-markup-style doc markup)
  (let ((style-stack (gnc:html-document-style-stack doc)))
    (or (and (pair? style-stack)
             (gnc:html-style-table-fetch
              (car style-stack) (cdr style-stack) markup))
        (gnc:make-html-markup-style-info))))

(define (gnc:html-document-fetch-data-style doc markup)
  (let ((style-stack (gnc:html-document-style-stack doc)))
    (or (and (pair? style-stack)
             (gnc:html-style-table-fetch
              (car style-stack) (cdr style-stack) markup))
        (gnc:make-html-data-style-info
         (lambda (datum parms) (format #f "~a ~a" markup datum))
         #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  markup-rendering functions : markup-start and markup-end return
;;  pre-body and post-body HTML for the given markup tag.
;;  the optional rest arguments are lists of attribute-value pairs:
;;  (gnc:html-document-markup-start doc "markup"
;;                                 '("attr1" "value1") '("attr2" "value2"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-document-markup-start doc markup end-tag? . rest)
  (let* ((childinfo (gnc:html-document-fetch-markup-style doc markup))
         (extra-attrib (and (pair? rest) rest))
         (retval '())
         (tag   (or (gnc:html-markup-style-info-tag childinfo) markup))
         (attr  (gnc:html-markup-style-info-attributes childinfo))
         (face  (gnc:html-markup-style-info-font-face childinfo))
         (size  (gnc:html-markup-style-info-font-size childinfo))
         (color (gnc:html-markup-style-info-font-color childinfo)))

    (define (push l) (set! retval (cons l retval)))
    (define (add-internal-tag tag) (push "<") (push tag) (push ">"))
    (define (add-attribute key value)
      (push " ") (push key)
      (when value (push "=\"") (push value) (push "\"")))
    (define (addextraatt attr)
      (cond ((string? attr) (push " ") (push attr))
            (attr (gnc:warn "non-string attribute" attr))))
    (define (build-first-tag tag)
      (push "<") (push tag)
      (if attr (hash-for-each add-attribute attr))
      (if extra-attrib (for-each addextraatt extra-attrib))
      (unless end-tag? (push " /")) ;;add closing "/" for no-end elements...
      (push ">"))

    (match tag
      ("" #f)
      ((head . tail) (build-first-tag head) (for-each add-internal-tag tail))
      (_ (build-first-tag tag)))

    ;; XXX Font styling should be done through CSS, NOT html code
    ;; XXX Also, why is this even here?  'Font' is an html tag just like anything else,
    ;;       so why does it have it's own custom pseudo code here?  It should be built
    ;;       as a call to this function just like any other tag, passing face/size/color as attributes.
    (if (or face size color)
        (begin
          (issue-deprecation-warning
           "this section is unreachable in code")
          (push "<font ")
          (if face
              (begin
                (push "face=\"") (push face) (push "\" ")))
          (if size
              (begin
                (push "size=\"") (push size) (push "\" ")))
          (if color
              (begin
                (push "color=\"") (push color) (push "\" ")))
          (push ">")))
    retval))

(define (gnc:html-document-markup-end doc markup)
  (let* ((childinfo (gnc:html-document-fetch-markup-style doc markup))
         (tag (or (gnc:html-markup-style-info-tag childinfo) markup))
         (retval '()))
    (define (push l) (set! retval (cons l retval)))
    (define (addtag t)
      (push "</")
      (push t)
      (push ">\n"))
    (when (gnc:html-markup-style-info-closing-font-tag childinfo)
      (push "</font>\n"))
    ;; now generate the end tag
    ;; "" tags mean "show no tag"; #f tags means use default.)
    (match tag
      ("" #f)
      ((? string?) (addtag tag))
      ((? list?) (for-each addtag (reverse tag))))
    retval))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-document-render-data
;;  looks up the relevant data style and renders the data accordingly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gnc:html-document-render-data doc data)
  (let* ((data-type (cond
                     ((number? data) "<number>")
                     ((string? data) "<string>")
                     ((boolean? data) "<boolean>")
                     ((record? data) (record-type-name
                                      (record-type-descriptor data)))
                     (else "<generic>")))
         (style-info (gnc:html-document-fetch-data-style doc data-type)))

    ((gnc:html-data-style-info-renderer style-info)
     data (gnc:html-data-style-info-data style-info))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  <html-object> class
;;  this is the parent of all the html object types.  You should not
;;  be creating <html-object> directly... use the specific type you
;;  want.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define <html-object>
  (make-record-type "<html-object>"
                    '(renderer data)))
(define gnc:html-object?
  (record-predicate <html-object>))

(define gnc:make-html-object-internal
  (record-constructor <html-object>))

(define (gnc:make-html-object obj)
  (cond
   ((not (record? obj))
    ;; for literals (strings/numbers)
    ;; if the object is #f, make it a placeholder
    (gnc:make-html-object-internal
     (lambda (obj doc)
       (gnc:html-document-render-data doc obj))
     (or obj " ")))

   ((gnc:html-text? obj)
    (gnc:make-html-object-internal gnc:html-text-render obj))

   ((gnc:html-table? obj)
    (gnc:make-html-object-internal gnc:html-table-render obj))

   ((gnc:html-anytag? obj)
    (gnc:make-html-object-internal gnc:html-anytag-render obj))

   ((gnc:html-table-cell? obj)
    (gnc:make-html-object-internal gnc:html-table-cell-render obj))

   ((gnc:html-barchart? obj)
    (gnc:make-html-object-internal gnc:html-barchart-render obj))

   ((gnc:html-piechart? obj)
    (gnc:make-html-object-internal gnc:html-piechart-render obj))

   ((gnc:html-scatter? obj)
    (gnc:make-html-object-internal gnc:html-scatter-render obj))

   ((gnc:html-linechart? obj)
    (gnc:make-html-object-internal gnc:html-linechart-render obj))

   ((gnc:html-object? obj)
    obj)

   ;; other record types that aren't HTML
   (else
    (gnc:make-html-object-internal
     (lambda (obj doc)
       (gnc:html-document-render-data doc obj)) obj))))

(define gnc:html-object-renderer
  (record-accessor <html-object> 'renderer))

(define gnc:html-object-set-renderer!
  (record-modifier <html-object> 'renderer))

(define gnc:html-object-data
  (record-accessor <html-object> 'data))

(define gnc:html-object-set-data!
  (record-modifier <html-object> 'data))

(define (gnc:html-object-render obj doc)
  (if (gnc:html-object? obj)
      ((gnc:html-object-renderer obj) (gnc:html-object-data obj) doc)
      (let ((htmlo (gnc:make-html-object obj)))
        (gnc:html-object-render htmlo doc))))
