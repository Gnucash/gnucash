;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stylesheet-plain.scm : the default stylesheet, very simple.
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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/stylesheet-plain.scm")
(gnc:depend  "report-html.scm")

;; plain style sheet 
;; this should generally be the default style sheet for most reports.
;; it's supposed to be lightweight and unobtrusive.

(let ()
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
       (gnc:make-number-range-option 
        (N_ "Tables")
        (N_ "Table cell spacing") "c" (N_ "Space between table cells")
        1 0 20 0 1))
      (opt-register 
       (gnc:make-number-range-option 
        (N_ "Tables")
        (N_ "Table cell padding") "d" (N_ "Space between table cells")
        1 0 20 0 1))
      (opt-register 
       (gnc:make-number-range-option 
        (N_ "Tables")
        (N_ "Table border width") "e" (N_ "Bevel depth on tables")
        0 0 20 0 1))
      options))

  (define (plain-renderer options doc)
    (let* ((ssdoc (gnc:make-html-document))
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
           (spacing (opt-val "Tables" "Table cell spacing"))
           (padding (opt-val "Tables" "Table cell padding"))
           (border (opt-val "Tables" "Table border width")))

      (gnc:html-document-set-style! 
       ssdoc "body" 
       'attribute (list "bgcolor" bgcolor))
      
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
       ssdoc "number-cell"
       'tag "td"
       'attribute (list "align" "right"))

      (gnc:html-document-set-style!
       ssdoc "number-header"
       'tag "th"
       'attribute (list "align" "right"))

      ;; don't surround marked-up links with <a> </a>
      (if (not links?)
          (gnc:html-document-set-style!
           ssdoc "a"
           'tag ""))
      
      (let ((title (gnc:html-document-title doc)))
        (if title
            (gnc:html-document-add-object!
             ssdoc
             (gnc:make-html-text
              (gnc:html-markup-p
               (gnc:html-markup-h3 title))))))
      
      (gnc:html-document-append-objects! ssdoc (gnc:html-document-objects doc))
      ssdoc))
  
  (gnc:define-html-style-sheet 
   'version 1.0
   'name "Plain"
   'renderer plain-renderer
   'options-generator plain-options))

;; instantiate a default style sheet 
(gnc:make-html-style-sheet "Plain" "Default")
