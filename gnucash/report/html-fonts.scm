;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  html-fonts.scm
;;  html stuff for fonts/css
;;
;;  Copyright (c) 2001 Linux Developers Group, Inc. 
;;  Copyright (c) Phil Longstaff <plongstaff@rogers.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define-module (gnucash report html-fonts))

(eval-when (compile load eval expand)
  (load-extension "libgnc-report" "scm_init_sw_report_module"))

(use-modules (sw_report))

(use-modules (gnucash core-utils))
(use-modules (gnucash report report-core))
(use-modules (gnucash report html-document))

(use-modules (ice-9 regex))

;; html-fonts.scm

(export register-font-options)
(export add-css-information-to-doc)
(export font-name-to-style-info)

(define (string-strip s1 s2)
  (let ((idx (string-contains-ci s1 s2)))
    (string-append
     (string-take s1 idx)
     (string-drop s1 (+ idx (string-length s2))))))

;; Converts a font name to css style information
(define (font-name-to-style-info font-name)
  (let* ((font-style "")
         (font-weight "")
         (font-stretch "")
         (idx (string-index-right font-name #\space))
         (font-size (substring font-name (1+ idx) (string-length font-name)))
         (font-name (string-take font-name idx))
         (pat (make-regexp " weight=([0-9]+)" regexp/icase regexp/extended))
         (match (regexp-exec pat font-name)))


    (cond
     ((string-contains-ci font-name " bold")
      (set! font-weight "font-weight: bold; ")
      (set! font-name (string-strip font-name " bold")))
     ((string-contains-ci font-name " regular")
      (set! font-weight "font-weight: normal; ")
      (set! font-name (string-strip font-name " regular")))
     ((string-contains-ci font-name " light")
      (set! font-weight "font-weight: lighter; ")
      (set! font-name (string-strip font-name " light")))
     ((regexp-match? match)
      (set! font-weight (regexp-substitute #f match "font-weight: " 1 "; "))
      (set! font-name (regexp-substitute #f match 'pre 'post))))

    (cond
     ((string-contains-ci font-name " italic")
      (set! font-style "font-style: italic; ")
      (set! font-name (string-strip font-name " italic")))

     ((string-contains-ci font-name " oblique")
      (set! font-style "font-style: oblique; ")
      (set! font-name (string-strip font-name " oblique"))))

    (cond
     ((string-contains-ci font-name " expanded")
      (set! font-stretch "font-stretch: expanded; ")
      (set! font-name (string-strip font-name " expanded")))

     ((string-contains-ci font-name " condensed")
      (set! font-stretch "font-stretch: condensed; ")
      (set! font-name (string-strip font-name " condensed"))))

    (string-append "font-family: \"" font-name "\", sans-serif; "
                   "font-size: " font-size "pt; "
                   font-style font-weight font-stretch)))

;; Registers font options
(define (register-font-options options)
  (let ((font-family (gnc-get-default-report-font-family))
        (odb (gnc:optiondb options)))
     (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Title") "a" (N_ "Font info for the report title.")
      (string-append font-family " Bold 15"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Account link") "b" (N_ "Font info for account name.")
      (string-append font-family " Italic 10"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Number cell") "c" (N_ "Font info for regular number cells.")
      (string-append font-family " 10"))
    (gnc-register-simple-boolean-option odb
      (N_ "Fonts")
      (N_ "Negative Values in Red") "d" (N_ "Display negative values in red.")
      #t)
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Number header") "e" (N_ "Font info for number headers.")
      (string-append font-family " 10"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Text cell") "f" (N_ "Font info for regular text cells.")
      (string-append font-family " 10"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Total number cell") "g"
      (N_ "Font info for number cells containing a total.")
      (string-append font-family " Bold 12"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Total label cell") "h"
      (N_ "Font info for cells containing total labels.")
      (string-append font-family " Bold 12"))
    (gnc-register-font-option odb
      (N_ "Fonts")
      (N_ "Centered label cell") "i" (N_ "Font info for centered label cells.")
      (string-append font-family " Bold 12"))))

;; Adds CSS style information to an html document
(define (add-css-information-to-doc options ssdoc doc)
  (define (opt-font-val name)
    (gnc-optiondb-lookup-value (gnc:optiondb options) "Fonts" name))
  (define (opt-style-info name) (font-name-to-style-info (opt-font-val name)))
  (let* ((negative-red? (opt-font-val "Negative Values in Red"))
         (alternate-row-color
          (gnc:color->html
           (gnc-optiondb-lookup-value (gnc:optiondb options)
                              "Colors" "Alternate Table Cell Color")))
         (title-info (opt-style-info "Title"))
         (account-link-info (opt-style-info "Account link"))
         (number-cell-info (opt-style-info "Number cell"))
         (number-header-info (opt-style-info "Number header"))
         (text-cell-info (opt-style-info "Text cell"))
         (total-number-cell-info (opt-style-info "Total number cell"))
         (total-label-cell-info (opt-style-info "Total label cell"))
         (centered-label-cell-info (opt-style-info "Centered label cell")))

    (gnc:html-document-set-style-text!
     ssdoc
     (string-append
      ;; Note: any changes in the default CSS *should* be duplicated in
      ;; stylesheet-css.scm
      "@media (prefers-color-scheme: dark) {body {color: #000; background-color: #fff;}}\n"
      "h3 { " title-info " }\n"
      "a { " account-link-info " }\n"
      "body, p, table, tr, td { vertical-align: top; " text-cell-info " }\n"
      "tr.alternate-row { background: " alternate-row-color " }\n"
      "tr { page-break-inside: avoid !important;}\n"
      "html, body { height: 100vh; margin-top: 0px; margin-bottom: 0px; margin-left: 8px; margin-right: 8px; }\n"
      "td, th { border-color: grey }\n"
      "th.column-heading-left { text-align: left; " number-header-info " }\n"
      "th.column-heading-center { text-align: center; " number-header-info " }\n"
      "th.column-heading-right { text-align: right; " number-header-info " }\n"
      "td.highlight {background-color:#e1e1e1}"
      "td.neg { " (if negative-red? "color: red; " "") " }\n"
      "td.number-cell, td.total-number-cell { text-align: right; white-space: nowrap; }\n"
      "td.date-cell { white-space: nowrap; }\n"
      "td.anchor-cell { white-space: nowrap; " text-cell-info " }\n"
      "td.number-cell { " number-cell-info " }\n"
      "td.number-header { text-align: right; " number-header-info " }\n"
      "td.text-cell { " text-cell-info " }\n"
      "td.total-number-cell { " total-number-cell-info " }\n"
      "td.total-label-cell { " total-label-cell-info " }\n"
      "td.centered-label-cell { text-align: center; " centered-label-cell-info " }\n"
      "sub { top: 0.4em; }\n"
      "sub, sup { vertical-align: baseline; position: relative; top: -0.4em; }\n"
      "@media print { html, body { height: unset; }}\n"
      (or (gnc:html-document-style-text doc) "")))))
