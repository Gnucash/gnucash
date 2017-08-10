;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-plain.css : the default style sheet, very simple
;; Copyright 2009 Phil Longstaff <plongstaff@rogers.com>
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


(define-module (gnucash report stylesheet-plain))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))
(use-modules (gnucash gettext))
(use-modules (srfi srfi-13))
(use-modules (srfi srfi-14))

(gnc:module-load "gnucash/html" 0)   ; added for 'gnc-html-engine-supports-css'
(gnc:module-load "gnucash/report/report-system" 0)

;; plain style sheet
;; this should generally be the default style sheet for most reports.
;; it's supposed to be lightweight and unobtrusive.
(define (plain-options)
  (let* ((options (gnc:new-options))
     (opt-register
      (lambda (opt)
        (gnc:register-option options opt))))
     (opt-register
          (gnc:make-color-option
           (N_ "General")
           (N_ "Background Color") "a" (N_ "Background color for reports.")
           (list #xff #xff #xff 0)
           255 #f))
         (opt-register
          (gnc:make-pixmap-option
           (N_ "General")
           (N_ "Background Pixmap") "b" (N_ "Background tile for reports.")
           ""))
         (opt-register
          (gnc:make-simple-boolean-option
           (N_ "General")
           (N_ "Enable Links") "c" (N_ "Enable hyperlinks in reports.")
           #t))
         (opt-register
          (gnc:make-color-option
           (N_ "Colors")
           (N_ "Alternate Table Cell Color") "a" (N_ "Background color for alternate lines.")
           (list #xff #xff #xff 0)
           255 #f))
         (opt-register
          (gnc:make-number-range-option
           (N_ "Tables")
           (N_ "Table cell spacing") "a" (N_ "Space between table cells.")
           0 0 20 0 1))
         (opt-register
          (gnc:make-number-range-option
           (N_ "Tables")
           (N_ "Table cell padding") "b" (N_ "Space between table cell edge and content.")
           4 0 20 0 1))
         (opt-register
          (gnc:make-number-range-option
           (N_ "Tables")
           (N_ "Table border width") "c" (N_ "Bevel depth on tables.")
           0 0 20 0 1))
         (register-font-options options)

         options))

(define (plain-renderer options doc)
  (let*
    ((ssdoc (gnc:make-html-document))
     (opt-val
      (lambda (section name)
        (gnc:option-value
         (gnc:lookup-option options section name))))
     (bgcolor
      (gnc:color-option->html
       (gnc:lookup-option options
                  "General"
                  "Background Color")))
     (bgpixmap (opt-val "General" "Background Pixmap"))
     (links? (opt-val "General" "Enable Links"))
     (alternate-row-color
      (gnc:color-option->html
       (gnc:lookup-option options
                  "Colors"
                  "Alternate Table Cell Color")))
     (spacing (opt-val "Tables" "Table cell spacing"))
     (padding (opt-val "Tables" "Table cell padding"))
     (border (opt-val "Tables" "Table border width"))
    )

    (gnc:html-document-set-style!
       ssdoc "body"
       'attribute (list "bgcolor" bgcolor))

    (if (and bgpixmap
         (not (string=? bgpixmap "")))
    (gnc:html-document-set-style!
     ssdoc "body"
     'attribute (list "background" (make-file-url bgpixmap))))

    (gnc:html-document-set-style!
       ssdoc "table"
       'attribute (list "border" border)
       'attribute (list "cellspacing" spacing)
       'attribute (list "cellpadding" padding))

    (if (gnc-html-engine-supports-css)
        (begin ;; this is for webkit
          (gnc:html-document-set-style!
             ssdoc "column-heading-left"
             'tag "th"
             'attribute (list "class" "column-heading-left"))

          (gnc:html-document-set-style!
             ssdoc "column-heading-center"
             'tag "th"
             'attribute (list "class" "column-heading-center"))

          (gnc:html-document-set-style!
             ssdoc "column-heading-right"
             'tag "th"
             'attribute (list "class" "column-heading-right"))

          (gnc:html-document-set-style!
             ssdoc "date-cell"
             'tag "td"
             'attribute (list "class" "date-cell"))

          (gnc:html-document-set-style!
             ssdoc "anchor-cell"
             'tag "td"
             'attribute (list "class" "anchor-cell"))

          (gnc:html-document-set-style!
             ssdoc "number-cell"
             'tag "td"
             'attribute (list "class" "number-cell"))

          (gnc:html-document-set-style!
             ssdoc "number-cell-neg"
             'tag "td"
             'attribute (list "class" "number-cell neg"))

          (gnc:html-document-set-style!
             ssdoc "number-header"
             'tag "th"
             'attribute (list "class" "number-header"))

          (gnc:html-document-set-style!
             ssdoc "text-cell"
             'tag "td"
             'attribute (list "class" "text-cell"))

          (gnc:html-document-set-style!
             ssdoc "total-number-cell"
             'tag "td"
             'attribute (list "class" "total-number-cell"))

          (gnc:html-document-set-style!
             ssdoc "total-number-cell-neg"
             'tag "td"
             'attribute (list "class" "total-number-cell neg"))

          (gnc:html-document-set-style!
             ssdoc "total-label-cell"
             'tag "td"
             'attribute (list "class" "total-label-cell"))

          (gnc:html-document-set-style!
             ssdoc "centered-label-cell"
             'tag "td"
             'attribute (list "class" "centered-label-cell"))
        )
        (begin ;; this is for gtkhtml
          (gnc:html-document-set-style!
             ssdoc "column-heading-left"
             'tag "th"
             'attribute (list "align" "left"))

          (gnc:html-document-set-style!
             ssdoc "column-heading-center"
             'tag "th"
             'attribute (list "align" "center"))

          (gnc:html-document-set-style!
             ssdoc "column-heading-right"
             'tag "th"
             'attribute (list "align" "right"))

          (gnc:html-document-set-style!
             ssdoc "date-cell"
             'tag "td"
             'attribute (list "nowrap" "nowrap"))

          (gnc:html-document-set-style!
             ssdoc "anchor-cell"
             'tag "td"
             'attribute (list "align" "left")
             'attribute (list "nowrap"))

          (gnc:html-document-set-style!
             ssdoc "number-cell"
             'tag "td"
             'attribute (list "align" "right")
             'attribute (list "nowrap"))

          (gnc:html-document-set-style!
             ssdoc "number-cell-neg"
             'tag "td"
             'attribute (list "align" "right")
             'attribute (list "nowrap"))

          (gnc:html-document-set-style!
             ssdoc "number-header"
             'tag "th"
             'attribute (list "align" "right"))

          (gnc:html-document-set-style!
             ssdoc "text-cell"
             'tag "td"
             'attribute (list "align" "left"))

          (gnc:html-document-set-style!
             ssdoc "total-number-cell"
             'tag "td"
             'attribute (list "align" "right"))

          (gnc:html-document-set-style!
             ssdoc "total-number-cell-neg"
             'tag "td"
             'attribute (list "align" "right"))

          (gnc:html-document-set-style!
             ssdoc "total-label-cell"
             'tag "td"
             'attribute (list "align" "left"))

          (gnc:html-document-set-style!
             ssdoc "centered-label-cell"
             'tag "td"
             'attribute (list "align" "center"))
        )
    )

    (gnc:html-document-set-style!
       ssdoc "normal-row"
       'tag "tr")

    (gnc:html-document-set-style!
       ssdoc "alternate-row"
       'tag "tr"
       'attribute (list "bgcolor" alternate-row-color))

    (gnc:html-document-set-style!
     ssdoc "primary-subheading"
     'attribute (list "bgcolor" bgcolor)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "secondary-subheading"
     'attribute (list "bgcolor" bgcolor)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "grand-total"
     'attribute (list "bgcolor" bgcolor)
     'tag "tr")

    ;; don't surround marked-up links with <a> </a>
    (if (not links?)
    (gnc:html-document-set-style!
     ssdoc "a"
     'tag ""))

    (add-css-information-to-doc options ssdoc)

    (let* ((title (gnc:html-document-title doc))
           (doc-headline (gnc:html-document-headline doc))
           (headline (if (eq? doc-headline #f)
                         title doc-headline)))
      (if headline
      (gnc:html-document-add-object!
       ssdoc
       (gnc:make-html-text
         (gnc:html-markup-h3 headline)))))

    (gnc:html-document-append-objects! ssdoc
                       (gnc:html-document-objects doc))

    ssdoc))

(gnc:define-html-style-sheet
 'version 1
 'name (N_ "Plain")
 'renderer plain-renderer
 'options-generator plain-options)

;; instantiate a default style sheet
(gnc:make-html-style-sheet "Plain" (N_ "Default"))
