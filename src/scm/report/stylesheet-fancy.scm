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
;; 59 Temple Place - Suite 330        Fax:    +1-617-542-2652
;; Boston, MA  02111-1307,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(gnc:support "report/stylesheet-plain.scm")
(gnc:depend  "report-html.scm")
(gnc:depend  "date-utilities.scm")

;; plain style sheet 
;; this should generally be the default style sheet for most reports.
;; it's supposed to be lightweight and unobtrusive.

(let ()
  (define (fancy-options)
    (let* ((options (gnc:new-options))
           (opt-register 
            (lambda (opt) 
              (gnc:register-option options opt))))
      (opt-register 
       (gnc:make-string-option
        (_ "General")
        (_ "Preparer") "a"
        (_ "Name of person preparing the report") 
        ""))
      (opt-register 
       (gnc:make-string-option
        (_ "General")
        (_ "Prepared for") "b"
        (_ "Name of organization or company prepared for") 
        ""))
      (opt-register 
       (gnc:make-simple-boolean-option
        (_ "General")
        (_ "Show preparer info") "c"
        (_ "Name of organization or company") 
        #f))
      
      (opt-register
       (gnc:make-pixmap-option
        (_ "Images")
        (_ "Background Tile") "a" (_ "Background tile for reports.")
        ""))
      (opt-register
       (gnc:make-pixmap-option
        (_ "Images")
        (_ "Heading Banner") "b" (_ "Banner for top of report.")
        ""))
      (opt-register
       (gnc:make-pixmap-option
        (_ "Images")
        (_ "Logo") "c" (_ "Company logo image.")
        ""))

      (opt-register
       (gnc:make-color-option
        (_ "Colors")
        (_ "Background Color") "a" (_ "General background color for report.")
        (list #xff #x88 #xff 0)
        255 #f))      
      (opt-register
       (gnc:make-color-option
        (_ "Colors")
        (_ "Text Color") "b" (_ "Normal body text color.")
        (list #x00 #x00 #x00 0)
        255 #f))      
      (opt-register
       (gnc:make-color-option
        (_ "Colors")
        (_ "Link Color") "c" (_ "Link text color.")
        (list #x00 #xff #xff 0)
        255 #f))      
      (opt-register
       (gnc:make-color-option
        (_ "Colors")
        (_ "Table Cell Color") "c" (_ "Default background for table cells.")
        (list #xff #x00 #xff 0)
        255 #f))      

      (opt-register 
       (gnc:make-number-range-option 
        (_ "Tables")
        (_ "Table cell spacing") "a" (_ "Space between table cells")
        1 0 20 0 1))
      (opt-register 
       (gnc:make-number-range-option 
        (_ "Tables")
        (_ "Table cell padding") "b" (_ "Space between table cells")
        1 0 20 0 1))
      (opt-register 
       (gnc:make-number-range-option 
        (_ "Tables")
        (_ "Table border width") "c" (_ "Bevel depth on tables")
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
           (preparer (opt-val (_ "General") (_ "Preparer")))
           (prepared-for (opt-val (_ "General") (_ "Prepared for")))
           (show-preparer? (opt-val (_ "General") (_ "Show preparer info")))
           (bgcolor (color-val (_ "Colors") (_ "Background Color")))
           (textcolor (color-val (_ "Colors") (_ "Text Color")))
           (linkcolor (color-val (_ "Colors") (_ "Link Color")))
           (cellcolor (color-val (_ "Colors") (_ "Table Cell Color")))
           (bgpixmap (opt-val (_ "Images") (_ "Background Tile")))
           (headpixmap (opt-val (_ "Images") (_ "Heading Banner")))
           (logopixmap (opt-val (_ "Images") (_ "Logo")))
           (spacing (opt-val (_ "Tables") (_ "Table cell spacing")))
           (padding (opt-val (_ "Tables") (_ "Table cell padding")))
           (border (opt-val (_ "Tables") (_ "Table border width"))))
            
      (gnc:html-document-set-style! 
       ssdoc "body" 
       'attribute (list "bgcolor" bgcolor)
       'attribute (list "text" textcolor)
       'attribute (list "link" linkcolor))
      
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

      (let ((t (gnc:make-html-table)))
        ;; we don't want a bevel for this table, but we don't want 
        ;; that to propagate 
        (gnc:html-table-set-style!
         t "table" 
         'attribute (list "border" 0)
         'inheritable? #f)
        
        (gnc:html-table-set-cell! 
         t 1 1
         (if show-preparer? 
             ;; title plus preparer info 
             (gnc:make-html-text
              (gnc:html-markup-b 
               (gnc:html-document-title doc))  
              (gnc:html-markup-br)
              "Prepared by: "
              (gnc:html-markup-b preparer)
              (gnc:html-markup-br)
              "Prepared for: "
              (gnc:html-markup-b prepared-for)
              (gnc:html-markup-br)
              "Date: " 
              (gnc:timepair-to-datestring 
               (cons (current-time) 0)))

             ;; title only 
             (gnc:make-html-text
              (gnc:html-markup-b 
               (gnc:html-document-title doc)))))
                      
        (gnc:html-table-set-cell!
         t 0 0
         (gnc:make-html-text
          (gnc:html-markup-img logopixmap)))
        
        (gnc:html-table-set-cell!
         t 0 1
         (gnc:make-html-text
          (gnc:html-markup-img headpixmap)))
        
        (apply 
         gnc:html-table-set-cell! 
         t 2 1 
         (gnc:html-document-objects doc))
        
        (gnc:html-document-add-object! ssdoc t))
      ssdoc))
  
  (gnc:define-html-style-sheet 
   'version 1.0
   'name "Fancy"
   'renderer fancy-renderer
   'options-generator fancy-options)
  
  #t)
