;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-head-or-tail.scm: stylesheet with nicer formatting for
;; printing and easier configurability
;;
;; Copyright 2004 James Strandboge <jstrand1@rochester.rr.com>
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
;;
;; Based on work from:
;; stylesheet-header.scm
;; Copyright 2000 Bill Gribble <grib@gnumatic.com>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
;; Modified by Graham Billiau to include a text
;; with small adjustments by Frank H. Ellenberger 2010
;; extended with positioning at top or at bottom by Carsten Rinke 2014
;


(define-module (gnucash report stylesheets head-or-tail))

(use-modules (gnucash engine))
(use-modules (gnucash utilities))
(use-modules (gnucash core-utils)) ; for gnc:version and (_ ...)
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(define (head-or-tail-options)
  (let* ((options (gnc:new-options))
         (opt-register
          (lambda (opt)
            (gnc:register-option options opt))))
    (opt-register
     (gnc:make-string-option
      (N_ "General")
      (N_ "Preparer") "a"
      (N_ "Name of person preparing the report.")
      ""))
    (opt-register
     (gnc:make-string-option
      (N_ "General")
      (N_ "Prepared for") "b"
      (N_ "Name of organization or company prepared for.")
      ""))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show preparer info") "c"
      (N_ "Name of organization or company.")
      #t))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show receiver info") "d"
      (N_ "Name of organization or company the report is prepared for.")
      #t))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show date") "e"
      (N_ "The creation date for this report.")
      #t))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show time in addition to date") "f"
      (N_ "The creation time for this report can only be shown if the date is shown.")
      #t))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show GnuCash Version") "g"
      (N_ "Show the currently used GnuCash version.")
      #t))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Enable Links") "h"
      (N_ "Enable hyperlinks in reports.")
      #t))
    ;; FIXME: put this in a more sensible tab like Text or Header/Footer
    (opt-register
     (gnc:make-text-option
      (N_ "General")
      (N_ "Additional Comments") "i"
      (N_ "String for additional report information.")
      ""))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show preparer info at bottom") "j"
      (N_ "Per default the preparer info will be shown before the report data.")
      #f))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show receiver info at bottom") "k"
      (N_ "Per default the receiver info will be shown before the report data.")
      #f))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show date/time at bottom") "l"
      (N_ "Per default the date/time info will be shown before the report data.")
      #f))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show comments at bottom") "m"
      (N_ "Per default the additional comments text will be shown before the report data.")
      #f))
    (opt-register
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show GnuCash version at bottom") "m"
      (N_ "Per default the GnuCash version will be shown before the report data.")
      #f))

    (opt-register
     (gnc:make-pixmap-option
      (N_ "Images")
      (N_ "Background Tile") "a" (N_ "Background tile for reports.")
      ""))
    (opt-register
     (gnc:make-pixmap-option
      (N_ "Images")
;;; Translators: Banner is an image like Logo.
      (N_ "Heading Banner") "b" (N_ "Banner for top of report.")
      ""))
    (opt-register
     (gnc:make-multichoice-option
      (N_ "Images")
      (N_ "Heading Alignment") "c" (N_ "Banner for top of report.")
      'left
      (list (vector 'left
                    (N_ "Left")
                    (N_ "Align the banner to the left."))
            (vector 'center
                    (N_ "Center")
                    (N_ "Align the banner in the center."))
            (vector 'right
                    (N_ "Right")
                    (N_ "Align the banner to the right."))
            )))
    (opt-register
     (gnc:make-pixmap-option
      (N_ "Images")
      (N_ "Logo") "d" (N_ "Company logo image.")
      ""))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Background Color") "a" (N_ "General background color for report.")
      (list #xff #xff #xff #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Text Color") "b" (N_ "Normal body text color.")
      (list #x00 #x00 #x00 #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Link Color") "c" (N_ "Link text color.")
      (list #xb2 #x22 #x22 #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Table Cell Color") "c" (N_ "Default background for table cells.")
      (list #xff #xff #xff #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Alternate Table Cell Color") "d"
      (N_ "Default alternate background for table cells.")
      (list #xff #xff #xff #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Subheading/Subtotal Cell Color") "e"
      (N_ "Default color for subtotal rows.")
      (list #xee #xe8 #xaa #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Sub-subheading/total Cell Color") "f"
      (N_ "Color for subsubtotals.")
      (list #xfa #xfa #xd2 #xff)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Grand Total Cell Color") "g"
      (N_ "Color for grand totals.")
      (list #xff #xff #x00 #xff)
      255 #f))

    (opt-register
     (gnc:make-number-range-option
      (N_ "Tables")
      (N_ "Table cell spacing") "a" (N_ "Space between table cells.")
      1 0 20 0 1))

    (opt-register
     (gnc:make-number-range-option
      (N_ "Tables")
      (N_ "Table cell padding") "b" (N_ "Space between table cell edge and content.")
      1 0 20 0 1))

    (opt-register
     (gnc:make-number-range-option
      (N_ "Tables")
      (N_ "Table border width") "c" (N_ "Bevel depth on tables.")
      1 0 20 0 1))
    (register-font-options options)

    options))

(define (head-or-tail-renderer options doc)
  (let* ((ssdoc (gnc:make-html-document))
         (opt-val
          (lambda (section name)
            (gnc:option-value
             (gnc:lookup-option options section name))))
         (color-val
          (lambda (section name)
            (gnc:color-option->html
             (gnc:lookup-option options section name))))
         (preparer (opt-val "General" "Preparer"))
         (prepared-for (opt-val "General" "Prepared for"))
         (show-preparer? (opt-val "General" "Show preparer info"))
         (show-receiver? (opt-val "General" "Show receiver info"))
         (show-date? (opt-val "General" "Show date"))
         (show-time? (opt-val "General" "Show time in addition to date"))
         (show-gnucash-version? (opt-val "General" "Show GnuCash Version"))
         (show-preparer-at-bottom? (opt-val "General" "Show preparer info at bottom"))
         (show-receiver-at-bottom? (opt-val "General" "Show receiver info at bottom"))
         (show-date-time-at-bottom? (opt-val "General" "Show date/time at bottom"))
         (show-comments-at-bottom? (opt-val "General" "Show comments at bottom"))
         (show-gnucash-version-at-bottom?
          (opt-val "General" "Show GnuCash version at bottom"))
         (links? (opt-val "General" "Enable Links"))
         (additional-comments (opt-val "General" "Additional Comments"))
         (bgcolor (color-val "Colors" "Background Color"))
         (textcolor (color-val "Colors" "Text Color"))
         (linkcolor (color-val "Colors" "Link Color"))
         (normal-row-color (color-val "Colors" "Table Cell Color"))
         (alternate-row-color (color-val "Colors" "Alternate Table Cell Color"))
         (primary-subheading-color
          (color-val "Colors" "Subheading/Subtotal Cell Color"))
         (secondary-subheading-color
          (color-val "Colors" "Sub-subheading/total Cell Color"))
         (grand-total-color (color-val "Colors" "Grand Total Cell Color"))
         (bgpixmap (opt-val "Images" "Background Tile"))
         (headpixmap (opt-val "Images" "Heading Banner"))
         (logopixmap (opt-val "Images" "Logo"))
         (align (gnc:value->string (opt-val "Images" "Heading Alignment")))
         (spacing (opt-val "Tables" "Table cell spacing"))
         (padding (opt-val "Tables" "Table cell padding"))
         (border (opt-val "Tables" "Table border width"))
         (headcolumn 0))

    (gnc:html-document-set-style!
     ssdoc "body"
     'attribute (list "bgcolor" bgcolor)
     'attribute (list "text" textcolor)
     'attribute (list "link" linkcolor))
;;;;
;;;;
;;;;
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
     'tag '("td")
     'attribute (list "class" "total-number-cell"))

    (gnc:html-document-set-style!
     ssdoc "total-number-cell-neg"
     'tag '("td")
     'attribute (list "class" "total-number-cell neg"))

    (gnc:html-document-set-style!
     ssdoc "total-label-cell"
     'tag '("td")
     'attribute (list "class" "total-label-cell"))

    (gnc:html-document-set-style!
     ssdoc "centered-label-cell"
     'tag '("td")
     'attribute (list "class" "centered-label-cell"))

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

    (gnc:html-document-set-style!
     ssdoc "normal-row"
     'attribute (list "bgcolor" normal-row-color)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "alternate-row"
     'attribute (list "bgcolor" alternate-row-color)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "primary-subheading"
     'attribute (list "bgcolor" primary-subheading-color)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "secondary-subheading"
     'attribute (list "bgcolor" secondary-subheading-color)
     'tag "tr")
    (gnc:html-document-set-style!
     ssdoc "grand-total"
     'attribute (list "bgcolor" grand-total-color)
     'tag "tr")

    ;; don't surround marked-up links with <a> </a>
    (if (not links?)
        (gnc:html-document-set-style! ssdoc "a" 'tag ""))

    (add-css-information-to-doc options ssdoc doc)

    (let ((t (gnc:make-html-table)))
      ;; we don't want a bevel for this table, but we don't want
      ;; that to propagate
      (gnc:html-table-set-style!
       t "table"
       'attribute (list "border" 0)
       'attribute (list "style" "margin-left:auto; margin-right:auto")
       'inheritable? #f)

      ;; set the header column to be the 2nd when we have a logo
      ;; do this so that when logo is not present, the document
      ;; is perfectly centered
      (if (and logopixmap (> (string-length logopixmap) 0))
          (set! headcolumn 1))

      (let* ((headline (or (gnc:html-document-headline doc)
                           (gnc:html-document-title doc))))

        (gnc:html-table-set-cell!
         t 1 headcolumn
         ;; print title
         (gnc:make-html-text
          (gnc:html-markup-h3 headline))
         (if (and show-preparer? (not show-preparer-at-bottom?))
             ;; print preparer info as additional header info
             (gnc:make-html-text
              (gnc:html-markup-i
               (_ "Prepared by: ")
               (gnc:html-markup-b preparer))
              (gnc:html-markup-br))
             " ")
         (if (and show-receiver? (not show-receiver-at-bottom?))
             ;; print receiver info as additional header info
             (gnc:make-html-text
              (gnc:html-markup-i
               (_ "Prepared for: ")
               (gnc:html-markup-b prepared-for)
               (gnc:html-markup-br)))
             " ")
         (if (and show-date? (not show-date-time-at-bottom?))
             ;; print date/time info as additional header info
             (if show-time?
                 (gnc:make-html-text
                  (gnc:html-markup-i
                   (_ "Report Creation Date: ")
                   (qof-print-date (gnc:get-today))
                   " "
                   (gnc-print-time64 (current-time) "%X %Z"))
                  (gnc:html-markup-br))
                 (gnc:make-html-text
                  (gnc:html-markup-i
                   (_ "Report Creation Date: ")
                   (qof-print-date (gnc:get-today)))
                  (gnc:html-markup-br)))
             " ")
         (if (and show-gnucash-version? (not show-gnucash-version-at-bottom?))
             ;; print the GnuCash version string as additional header info
             (gnc:make-html-text
              (gnc:html-markup-i
               "GnuCash "
               gnc:version)
              (gnc:html-markup-br))
             " ")
         (if (not show-comments-at-bottom?)
             ;; print additional comments as additional header info
             (gnc:make-html-text
              (gnc:html-markup-br)
              (gnc:html-markup-i additional-comments)
              (gnc:html-markup-br))
             " ")
         ;; add separator line if any additional header info is printed
         (if (or
              (and show-preparer? (not show-preparer-at-bottom?))
              (and show-receiver? (not show-receiver-at-bottom?))
              (and show-date? (not show-date-time-at-bottom?))
              (and show-gnucash-version? (not show-gnucash-version-at-bottom?))
              (not show-comments-at-bottom?))
             (gnc:make-html-text
              (gnc:html-markup-br))
             " ")))

      ;; only setup an image if we specified one
      (if (and logopixmap (> (string-length logopixmap) 0))
          (begin
            (gnc:html-table-set-cell!
             t 0 0
             (gnc:make-html-text
              (gnc:html-markup-img (make-file-url logopixmap))))))

      (if (and headpixmap (> (string-length headpixmap) 0))
          (let* ((div (gnc:html-markup-img (make-file-url headpixmap)))
                 (cell (gnc:make-html-table-cell (gnc:make-html-text div))))
            (gnc:html-table-cell-set-style! cell "td" 'attribute `("align" ,align))
            (gnc:html-table-set-cell! t 0 headcolumn cell))
          (gnc:html-table-set-cell! t 0 headcolumn (gnc:make-html-text " ")))

      (apply
       gnc:html-table-set-cell!
       t 2 headcolumn
       (gnc:html-document-objects doc))
      (gnc:html-document-add-object! ssdoc t)

      ;; I think this is the correct place to put the footer
      (gnc:html-table-set-cell!
       t 3 headcolumn
       ;;(gnc:make-html-text additional-comments)
       ;; add separator line if any additional header info is printed
       (if (or
            (and show-preparer? show-preparer-at-bottom?)
            (and show-receiver? show-receiver-at-bottom?)
            (and show-date? show-date-time-at-bottom?)
            (and show-gnucash-version?
                 show-gnucash-version-at-bottom?)
            show-comments-at-bottom?)
           (gnc:make-html-text
            (gnc:html-markup-br))
           " ")
       (if (and show-preparer? show-preparer-at-bottom?)
           ;; print preparer info as additional header info
           (gnc:make-html-text
            (gnc:html-markup-i
             (_ "Prepared by: ")
             (gnc:html-markup-b preparer))
            (gnc:html-markup-br))
           " ")
       (if (and show-receiver? show-receiver-at-bottom?)
           ;; print receiver info as additional header info
           (gnc:make-html-text
            (gnc:html-markup-i
             (_ "Prepared for: ")
             (gnc:html-markup-b prepared-for))
            (gnc:html-markup-br))
           " ")
       (if (and show-date? show-date-time-at-bottom?)
           ;; print date/time info as additional header info
           (if show-time?
               (gnc:make-html-text
                (gnc:html-markup-i
                 (_ "Report Creation Date: ")
                 (qof-print-date (gnc:get-today))
                 " "
                 (gnc-print-time64 (current-time) "%X %Z"))
                (gnc:html-markup-br))
               (gnc:make-html-text
                (gnc:html-markup-i
                 (_ "Report Creation Date: ")
                 (qof-print-date (gnc:get-today))
                 (gnc:html-markup-br))))
           " ")
       (if (and show-gnucash-version? show-gnucash-version-at-bottom?)
           ;; print the GnuCash version string as additional header info
           (gnc:make-html-text
            (gnc:html-markup-i
             (_ "GnuCash ")
             gnc:version)
            (gnc:html-markup-br))
           " ")
       (if show-comments-at-bottom?
           ;; print additional comments as additional header info
           (gnc:make-html-text
            (gnc:html-markup-br)
            (gnc:html-markup-i additional-comments)
            (gnc:html-markup-br))
           " ")))
    ssdoc))

(gnc:define-html-style-sheet
 'version 1
 'name (N_ "Head or Tail")
 'renderer head-or-tail-renderer
 'options-generator head-or-tail-options)

(gnc:make-html-style-sheet "Head or Tail" (N_ "Head or Tail"))
