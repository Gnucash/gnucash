;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; view-column.scm : simple multi-column table view. 
;; Copyright 2001 Bill Gribble <grib@gnumatic.com>
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

(gnc:support "report/view-column.scm")

;; multi-column view.  this is the no brainer.  The only parameters
;; are the list of reports to display (with rowspan and colspan for
;; each), in order, and the number of columns in the table.  It gets
;; edited in a special window.  Every view gets a stylesheet so we
;; don't have to worry about that here.

(let ()
  (define (make-options)
    (let* ((options (gnc:new-options))
           (opt-register
            (lambda (opt)
              (gnc:register-option options opt))))
      ;; the report-list is edited by a special add-on page for the
      ;; options editor.
      (opt-register 
       (gnc:make-internal-option
        "__general" "report-list" '()))
      
      (opt-register
       (gnc:make-number-range-option 
        (N_ "General") (N_ "Number of columns") "a"
        (N_ "Number of columns before wrapping to a new row")
        1 0 20 0 1))
      
      options))
  
  (define (edit-options option-obj report-obj)
    (gnc:column-view-edit-new option-obj report-obj))
  
  (define (render-view report)
    (let* ((view-doc (gnc:make-html-document))
           (options (gnc:report-options report))
           (reports
            (gnc:option-value
             (gnc:lookup-option options "__general" "report-list")))
           (table-width 
            (gnc:option-value
             (gnc:lookup-option 
              options (N_ "General") (N_ "Number of columns"))))
           (column-allocs (make-hash-table 11))
           (column-tab (gnc:make-html-table))
           (current-row '())
           (current-width 0)
           (current-row-num 0))

      ;; we really would rather do something smart here with the
      ;; report's cached text if possible.  For the moment, we'll have
      ;; to rerun every report, every time... FIXME
      (for-each
       (lambda (report-info)
         ;; run the report renderer, pick out the document style table
         ;; and objects from the returned document, then make a new
         ;; HTML table cell with those objects as content and append
         ;; it to the table.  The weird stuff with the column-allocs
         ;; hash is an attempt to compute how many columnc are
         ;; actually used in a row; items with non-1 rowspans will take
         ;; up cells in the row without actually being in the row.
         (let* ((report (gnc:find-report (car report-info)))
                (colspan (cadr report-info))
                (rowspan (caddr report-info))
                (template (hash-ref *gnc:_report-templates_*
                                    (gnc:report-type report)))
                (renderer (gnc:report-template-renderer template))
                (report-doc (renderer report))
                (report-style (gnc:html-document-style report-doc))
                (report-objects (gnc:html-document-objects report-doc))
                (report-table (gnc:make-html-table))
                (contents-cell
                 (apply gnc:make-html-table-cell report-objects))
                (toplevel-cell 
                 (gnc:make-html-table-cell/size rowspan colspan)))
           
           ;; increment the alloc number for each occupied row
           (let loop ((row current-row-num))
             (let ((allocation (hash-ref column-allocs row)))
               (if (not allocation) 
                   (set! allocation 0))
               (hash-set! column-allocs row (+ colspan allocation))
               (if (< (- row current-row-num) rowspan)
                   (loop (+ 1 row)))))
           
           (gnc:html-table-cell-set-style-internal! 
            contents-cell report-style)
           
           (gnc:html-table-cell-set-style!
            toplevel-cell "td"
            'attribute (list "valign" "top")
            'inheritable? #f)
           
           ;; put the report in the contents-cell 
           (gnc:html-table-append-row! 
            report-table (list contents-cell))
           
           ;; and a parameter editor 
           (gnc:html-table-append-row!
            report-table 
            (list (gnc:make-html-text 
                   (gnc:html-markup-anchor
                    (sprintf #f "gnc-options:report-id=%a" (car report-info))
                    "Edit Options"))))

           ;; add the report-table to the toplevel-cell
           (gnc:html-table-cell-append-objects!
            toplevel-cell report-table)
           
           (set! current-row (append current-row (list toplevel-cell)))
           (set! current-width (+ current-width colspan))
           
           (if (>= current-width table-width)
               (begin 
                 (gnc:html-table-append-row! column-tab current-row)
                 ;; cells above with non-1 rowspan can force 'pre-allocation'
                 ;; of space on this row
                 (set! current-row-num (+ 1 current-row-num))
                 (set! current-width (hash-ref column-allocs current-row-num))
                 (if (not current-width) (set! current-width 0))
                 (set! current-row '())))))
       reports)
      
      (if (not (null? current-row))
          (gnc:html-table-append-row! column-tab current-row))
      
      ;; make sure the table is nice and big
      (gnc:html-table-set-style! 
       column-tab "table"
       'attribute (list "width" "100%"))
      
      (gnc:html-document-add-object! view-doc column-tab)
      ;; and we're done.
      view-doc))
  
  ;; define the view now.
  (gnc:define-report 
   'version 1.0
   'name "Multicolumn View"
   'renderer render-view
   'options-generator make-options
   'options-editor edit-options))
       
         
                
           
                
                

      #### End of Patch data ####
