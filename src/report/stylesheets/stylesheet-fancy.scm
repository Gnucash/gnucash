;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-header.scm : stylesheet with nicer layout
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


(define-module (gnucash report stylesheet-fancy))

(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(gnc:module-load "gnucash/report/report-system" 0)

(define (fancy-options)
  (let* ((options (gnc:new-options))
	 (opt-register 
	  (lambda (opt) 
	    (gnc:register-option options opt))))
    (opt-register 
     (gnc:make-string-option
      (N_ "General")
      (N_ "Preparer") "a"
      (N_ "Name of person preparing the report") 
      ""))
    (opt-register 
     (gnc:make-string-option
      (N_ "General")
      (N_ "Prepared for") "b"
      (N_ "Name of organization or company prepared for") 
      ""))
    (opt-register 
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Show preparer info") "c"
      (N_ "Name of organization or company") 
      #f))
    (opt-register 
     (gnc:make-simple-boolean-option
      (N_ "General")
      (N_ "Enable Links") "c"
      (N_ "Enable hyperlinks in reports") 
      #t))
    
    (opt-register
     (gnc:make-pixmap-option
      (N_ "Images")
      (N_ "Background Tile") "a" (N_ "Background tile for reports.")
      ""))
    (opt-register
     (gnc:make-pixmap-option
      (N_ "Images")
      (N_ "Heading Banner") "b" (N_ "Banner for top of report.")
      ""))
    (opt-register
     (gnc:make-multichoice-option
      (N_ "Images")
      (N_ "Heading Alignment") "c" (N_ "Banner for top of report.")
      'left
      (list (vector 'left
                     (N_ "Left")
                     (N_ "Align the banner to the left"))
            (vector 'center
                     (N_ "Center")
                     (N_ "Align the banner in the center"))
            (vector 'right
                     (N_ "Right")
                     (N_ "Align the banner to the right"))
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
      (list #xff #xff #xff 0)
      255 #f))      

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Text Color") "b" (N_ "Normal body text color.")
      (list #x00 #x00 #x00 0)
      255 #f))      

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Link Color") "c" (N_ "Link text color.")
      (list #xb2 #x22 #x22 0)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Table Cell Color") "c" (N_ "Default background for table cells.")
      (list #xff #xff #xff 0)
      255 #f))      

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Alternate Table Cell Color") "d"
      (N_ "Default alternate background for table cells.")
      (list #xff #xff #xff 0)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Subheading/Subtotal Cell Color") "e"
      (N_ "Default color for subtotal rows.")
      (list #xee #xe8 #xaa 0)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Sub-subheading/total Cell Color") "f"
      (N_ "Color for subsubtotals")
      (list #xfa #xfa #xd2 0)
      255 #f))

    (opt-register
     (gnc:make-color-option
      (N_ "Colors")
      (N_ "Grand Total Cell Color") "g"
      (N_ "Color for grand totals")
      (list #xff #xff #x00 0)
      255 #f))

    (opt-register 
     (gnc:make-number-range-option 
      (N_ "Tables")
      (N_ "Table cell spacing") "a" (N_ "Space between table cells")
      1 0 20 0 1))

    (opt-register 
     (gnc:make-number-range-option 
      (N_ "Tables")
      (N_ "Table cell padding") "b" (N_ "Space between table cells")
      1 0 20 0 1))

    (opt-register 
     (gnc:make-number-range-option 
      (N_ "Tables")
      (N_ "Table border width") "c" (N_ "Bevel depth on tables")
      1 0 20 0 1))
    options))

(define (fancy-renderer options doc)
  (let* ((ssdoc (gnc:make-html-document))
	 (opt-val 
	  (lambda (section name)
	    (gnc:option-value
	     (gnc:lookup-option options section name))))
	 (color-val
	  (lambda (section name)
	    (gnc:color-option->html
	     (gnc:lookup-option options section name))))
	 (preparer (opt-val (N_ "General") (N_ "Preparer")))
	 (prepared-for (opt-val (N_ "General") (N_ "Prepared for")))
	 (show-preparer? (opt-val (N_ "General") (N_ "Show preparer info")))
	 (links? (opt-val (N_ "General") (N_ "Enable Links")))           
	 (bgcolor (color-val (N_ "Colors") (N_ "Background Color")))
	 (textcolor (color-val (N_ "Colors") (N_ "Text Color")))
	 (linkcolor (color-val (N_ "Colors") (N_ "Link Color")))
	 (normal-row-color (color-val (N_ "Colors") (N_ "Table Cell Color")))
	 (alternate-row-color (color-val (N_ "Colors")
					 (N_ "Alternate Table Cell Color")))
	 (primary-subheading-color
	  (color-val (N_ "Colors")
		     (N_ "Subheading/Subtotal Cell Color")))
	 (secondary-subheading-color
	  (color-val (N_ "Colors") 
		     (N_ "Sub-subheading/total Cell Color")))
	 (grand-total-color (color-val (N_ "Colors")
				       (N_ "Grand Total Cell Color")))
	 (bgpixmap (opt-val (N_ "Images") (N_ "Background Tile")))
	 (headpixmap (opt-val (N_ "Images") (N_ "Heading Banner")))
	 (logopixmap (opt-val (N_ "Images") (N_ "Logo")))
         (align (gnc:value->string(opt-val (N_ "Images") (N_ "Heading Alignment"))))
	 (spacing (opt-val (N_ "Tables") (N_ "Table cell spacing")))
	 (padding (opt-val (N_ "Tables") (N_ "Table cell padding")))
	 (border (opt-val (N_ "Tables") (N_ "Table border width")))
         (headcolumn 0))

    ; center the document without elements inheriting anything
    (gnc:html-document-add-object! ssdoc
       (gnc:make-html-text "<center>"))

    (gnc:html-document-set-style! 
     ssdoc "body" 
     'attribute (list "bgcolor" bgcolor)
     'attribute (list "text" textcolor)
     'attribute (list "link" linkcolor))

;;;;
;;;;
;;;;
    (gnc:html-document-set-style!
     ssdoc "number-header"
     'tag "th"
     'attribute (list "align" "right"))
;;;;
;;;;
;;;;
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

    (if (and bgpixmap
	     (not (string=? bgpixmap "")))
	(gnc:html-document-set-style!
	 ssdoc "body" 
	 'attribute (list "background" bgpixmap)))
    
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

    (gnc:html-document-set-style!
     ssdoc "text-cell"
     'tag "td"
     'attribute (list "align" "left"))

    (gnc:html-document-set-style!
     ssdoc "total-number-cell"
     'tag '("td" "b")
     'attribute (list "align" "right"))

    (gnc:html-document-set-style!
     ssdoc "total-number-cell-neg"
     'tag '("td" "b")
     'attribute (list "align" "right"))

    (gnc:html-document-set-style!
     ssdoc "total-label-cell"
     'tag '("td" "b")
     'attribute (list "align" "left"))

    (gnc:html-document-set-style!
     ssdoc "centered-label-cell"
     'tag '("td" "b")
     'attribute (list "align" "center"))

    ;; don't surround marked-up links with <a> </a>
    (if (not links?)
	(gnc:html-document-set-style!
	 ssdoc "a" 'tag ""))
    
    (let ((t (gnc:make-html-table)))
      ;; we don't want a bevel for this table, but we don't want 
      ;; that to propagate 
      (gnc:html-table-set-style!
       t "table" 
       'attribute (list "border" 0)
       'inheritable? #f)

      (let* ((title (gnc:html-document-title doc))
             (doc-headline (gnc:html-document-headline doc))
             (headline (if (eq? doc-headline #f) title doc-headline)))

	; set the header column to be the 2nd when we have a logo
	; do this so that when logo is not present, the document
	; is perfectly centered
	(if (and logopixmap (> (string-length logopixmap) 0))
	    (set! headcolumn 1))

        (gnc:html-table-set-cell! 
         t 1 headcolumn
         (if show-preparer? 
             ;; title plus preparer info 
             (gnc:make-html-text
              (gnc:html-markup-b 
               (gnc:html-document-title doc))  
              (gnc:html-markup-br)
              (_ "Prepared by: ")
              (gnc:html-markup-b preparer)
              (gnc:html-markup-br)
              (_ "Prepared for: ")
              (gnc:html-markup-b prepared-for)
              (gnc:html-markup-br)
              (_ "Date: ")
              (gnc-print-date
               (cons (current-time) 0)))

             ;; title only 
             (gnc:make-html-text
              (gnc:html-markup-b 
               (gnc:html-document-title doc)))))
        )
      
      (if (and logopixmap
	       (not (string=? logopixmap "")))
	  ;; check for logo image file name non blank
	  (gnc:html-table-set-cell!
	   t 0 0
           (gnc:make-html-text
	    (gnc:html-markup-img (make-file-url logopixmap)))) )

      (if (and headpixmap
	       (not (string=? headpixmap "")))
	  ;; check for header image file name nonblank
	  (begin
	    (gnc:html-table-set-cell!
             t 0 headcolumn
             (gnc:make-html-text
	      ;; XX: isn't there some way to apply the alignment to
	      ;; (gnc:html-markup-img headpixmap)?
	      (string-append
	       "<div align=\"" align "\">"
	       "<img src=\"" (make-file-url headpixmap) "\">"
	       "</div>")))
	    )
	  (gnc:html-table-set-cell!
           t 0 headcolumn
           (gnc:make-html-text "&nbsp;")))

      (apply 
       gnc:html-table-set-cell! 
       t 2 headcolumn
       (gnc:html-document-objects doc))
      
      (gnc:html-document-add-object! ssdoc t))
    ssdoc))

(gnc:define-html-style-sheet 
 'version 1.01
 'name (N_ "Fancy")
 'renderer fancy-renderer
 'options-generator fancy-options)

(gnc:make-html-style-sheet "Fancy" (N_ "Technicolor"))
