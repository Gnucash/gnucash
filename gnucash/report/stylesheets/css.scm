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

(define-module (gnucash report stylesheets css))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-13))
(use-modules (gnucash html))

;; DEFAULT style
(define default-css "/* DEFAULT style */
@media (prefers-color-scheme: dark) {
    body {
        color: #000; background-color: #fff;
    }
}

html, body {
    height: 100vh;
    margin-top: 0px;
    margin-bottom: 0px;
    margin-left: 8px;
    margin-right: 8px;
    font-family: \"Noto Sans\", Sans-Serif;
    font-size: 10pt;
}

body, p, table, tr, td, a, th {
    vertical-align: top;
}

h3 {
    font-size: 13pt;
    font-weight: bold;
}

a {
    font-style: italic;
}

/* table elements as follows */
td, th {
    padding:4px;
}

tr.alternate-row {
    background: #ececec;
}

tr {
    page-break-inside: avoid !important;
}

td, th {
    border-color: grey
}

td.total-number-cell, td.total-label-cell, td.centered-label-cell {
    font-size: 12pt;
    font-weight: bold;
}

th.column-heading-left {
    text-align: left;
}

td.centered-label-cell, th.column-heading-center {
    text-align: center;
}

td.number-header, th.column-heading-right, td.number-cell, td.total-number-cell {
    text-align: right;
}

td.neg {
    color: red;
}

td.number-cell, td.total-number-cell, td.anchor-cell, td.date-cell {
    white-space: nowrap;
}

td.highlight {
    background-color: #e1e1e1
}

 td.text-cell {
     /* placeholder */
}

@media print {
    html, body {
        height: unset;
    }
}

tr.primary-subheading > td, tr:has(> td.total-number-cell) > td {
     background-color: #ececec;
}
tr.grand-total td {
     background: #ececec !important;
}
")

;; EASY style converted to CSS
(define easy-css "/* EASY style */
 @media (prefers-color-scheme: dark) {
     body {
         color: #000;
         background-color: #FFF;
    }
}

 html, body {
     height: 100vh;
     margin-top: 0px;
     margin-bottom: 0px;
     max-width: max-content;
     margin:0 auto;
     font-family: \"Noto Sans\", Sans-Serif;
     font-size: 10pt;
}

 body, p, table, tr, td, a, th {
     vertical-align: top;
}

 table {
    border-spacing: 2px;
    border-collapse: separate;
    max-width: max-content;
    margin: 0 auto;
}

 h3 {
     font-size: 13pt;
     font-weight: bold;
}

 a {
     color: #b22222;
}

 td, th {
     padding:1px;
}

 tr.alternate-row {
     background: #ececec;
}

 tr {
     page-break-inside: avoid !important;
}

 td, th {
     border-color: grey
}

 td.total-number-cell, td.total-label-cell, td.centered-label-cell {
     font-size: 12pt;
     font-weight: bold;
}

 th.column-heading-left {
     text-align: left;
}

 td.centered-label-cell, th.column-heading-center {
     text-align: center;
}

 td.number-header, th.column-heading-right, td.number-cell, td.total-number-cell {
     text-align: right;
}

 td.neg {
     color: red;
}

 td.number-cell, td.total-number-cell, td.anchor-cell, td.date-cell {
     white-space: nowrap;
}

 td.highlight {
     background-color: #e1e1e1;
}

 tr.primary-subheading > td, tr:has(> td.total-number-cell) > td {
     background-color: #eee8aa;
}

 tr.grand-total td {
     background: #faf88c !important;
}

 td.text-cell {
     /* placeholder */
}

 @media print {
     html, body {
         height: unset;
    }
}
")

(define (css-options css-param)
  (let ((options (gnc-new-optiondb)))

    (gnc-register-text-option options
      (N_ "General") (N_ "CSS") "a"
      (N_ "CSS code. This field specifies the CSS code for styling reports.")
      css-param)
    options))

(define (css-options-default)
    (css-options default-css)
)

(define (css-options-easy)
    (css-options easy-css)
)

(define (css-renderer options doc)

  (let* ((ssdoc (gnc:make-html-document))
         (css (gnc-optiondb-lookup-value options "General" "CSS"))
         (report-css (or (gnc:html-document-style-text doc) ""))
         (all-css (string-append css report-css))
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

    (gnc:html-document-set-style!
     ssdoc "normal-row"
     'tag "tr"
     'attribute (list "class" "normal-row"))

    (gnc:html-document-set-style!
     ssdoc "alternate-row"
     'tag "tr"
     'attribute (list "class" "alternate-row"))

    (gnc:html-document-set-style!
     ssdoc "primary-subheading"
     'tag "tr"  'attribute (list "class" "primary-subheading"))

    (gnc:html-document-set-style!
     ssdoc "secondary-subheading"
     'tag "tr" 'attribute (list "class" "secondary-subheading"))

    (gnc:html-document-set-style!
     ssdoc "grand-total"
     'tag "tr" 'attribute (list "class" "grand-total"))

    (cond
     ((string-contains-ci all-css "</style")
      (gnc:html-document-set-style-text! ssdoc default-css)
      (gnc:html-document-add-object!
       ssdoc (gnc:make-html-text
              (G_ "&lt;/style is disallowed in CSS. Using default CSS."))))

     (else
      (gnc:html-document-set-style-text! ssdoc all-css)))

    (unless (equal? headline "")
      (gnc:html-document-add-object!
       ssdoc (gnc:make-html-text (gnc:html-markup-h3 headline))))

    (gnc:html-document-append-objects! ssdoc (gnc:html-document-objects doc))

    ssdoc))

(gnc:define-html-style-sheet
 'version 1
 'name (N_ "CSS")
 'renderer css-renderer
 'options-generator css-options-default)

(gnc:define-html-style-sheet
 'version 1
 'name (N_ "CSS_Easy")
 'renderer css-renderer
 'options-generator css-options-easy)

(gnc:make-html-style-sheet "CSS" (N_ "CSS-based stylesheet (experimental)"))
(gnc:make-html-style-sheet "CSS_Easy" (N_ "CSS-based - Easy (experimental)"))
