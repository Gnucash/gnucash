;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-css.scm : a customisable CSS gnucash stylesheet
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

(define-module (gnucash report stylesheet-css))

(use-modules (gnucash gnc-module))
(use-modules (gnucash core-utils))
(use-modules (gnucash gettext))

(gnc:module-load "gnucash/html" 0)
(gnc:module-load "gnucash/report/report-system" 0)

(define default-css "
h3 { font-family: Noto Sans, Sans-Serif; font-size: 15pt; font-weight: bold; }
a { font-family: Noto Sans, Sans-Serif; font-size: 10pt; font-style: italic; }
body, p, table, tr, td { vertical-align: top; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
tr.alternate-row { background: #ffffff }
tr { page-break-inside: avoid !important;}
html, body { height: 100vh; margin: 0; }
td, th { border-color: grey }
th.column-heading-left { text-align: left; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
th.column-heading-center { text-align: center; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
th.column-heading-right { text-align: right; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
td.neg { color: red; }
td.number-cell, td.total-number-cell { text-align: right; white-space: nowrap; }
td.date-cell { white-space: nowrap; }
td.anchor-cell { white-space: nowrap; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
td.number-cell { font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
td.number-header { text-align: right; font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
td.text-cell { font-family: Noto Sans, Sans-Serif; font-size: 10pt; }
td.total-number-cell { font-family: Noto Sans, Sans-Serif; font-size: 12pt; font-weight: bold; }
td.total-label-cell { font-family: Noto Sans, Sans-Serif; font-size: 12pt; font-weight: bold; }
td.centered-label-cell { text-align: center; font-family: Noto Sans, Sans-Serif; font-size: 12pt; font-weight: bold; }
")

(define (css-options)
  (let ((options (gnc:new-options)))

    (gnc:register-option
     options
     (gnc:make-text-option
      (N_ "General") (N_ "CSS") "a"
      (N_ "CSS code. This field specifies the CSS code for styling reports.")
      default-css))

    options))

(define (css-renderer options doc)

  (let ((ssdoc (gnc:make-html-document))
        (css (gnc:option-value (gnc:lookup-option options "General" "CSS")))
        (headline (or (gnc:html-document-headline doc)
                      (gnc:html-document-title doc))))

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

    (gnc:html-document-set-style! ssdoc "normal-row" 'tag "tr")
    (gnc:html-document-set-style! ssdoc "alternate-row" 'tag "tr")
    (gnc:html-document-set-style! ssdoc "primary-subheading" 'tag "tr")
    (gnc:html-document-set-style! ssdoc "secondary-subheading" 'tag "tr")
    (gnc:html-document-set-style! ssdoc "grand-total" 'tag "tr")

    (gnc:html-document-set-style-text! ssdoc css)

    (unless (equal? headline "")
      (gnc:html-document-add-object!
       ssdoc (gnc:make-html-text (gnc:html-markup-h3 headline))))

    (gnc:html-document-append-objects! ssdoc (gnc:html-document-objects doc))

    ssdoc))

(gnc:define-html-style-sheet
 'version 1
 'name (N_ "CSS")
 'renderer css-renderer
 'options-generator css-options)

(gnc:make-html-style-sheet "CSS" (N_ "CSS-based stylesheet"))
