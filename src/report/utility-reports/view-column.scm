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
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; multi-column view.  this is the no brainer.  The only parameters
;; are the list of reports to display (with rowspan and colspan for
;; each), in order, and the number of columns in the table.  It gets
;; edited in a special window.  Every view gets a stylesheet so we
;; don't have to worry about that here.

(define-module (gnucash report view-column))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (ice-9 slib))
(use-modules (gnucash gnc-module))
(use-modules (sw_report_system))

(require 'printf)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define (make-options)
  (let* ((options (gnc:new-options))
	 (opt-register
	  (lambda (opt)
	    (gnc:register-option options opt))))
    ;; the report-list is edited by a special add-on page for the
    ;; options editor.
    (opt-register 
     (gnc:make-internal-option "__general" "report-list" '()))
    
    (opt-register
     (gnc:make-number-range-option 
      (N_ "General") (N_ "Number of columns") "a"
      (N_ "Number of columns before wrapping to a new row")
      1 0 20 0 1))
    
    options))

(define (make-child-options-callback view child)
  (let* ((view-opts (gnc:report-options view))
	 (child-opts (gnc:report-options child))
	 (id 
	  (gnc:options-register-callback
	   #f #f 
	   (lambda ()
	     (gnc:report-set-dirty?! child #t)
	     (gnc:options-touch view-opts))
	   child-opts)))
    id))

(define (render-view report)
  (let* ((view-doc (gnc:make-html-document))
	 (options (gnc:report-options report))
	 (report-opt (gnc:lookup-option options "__general" "report-list"))
	 (reports (gnc:option-value report-opt))
	 (table-width 
	  (gnc:option-value
	   (gnc:lookup-option 
	    options (N_ "General") (N_ "Number of columns"))))
	 (column-allocs (make-hash-table 11))
	 (column-tab (gnc:make-html-table))
	 (current-row '())
	 (current-width 0)
	 (current-row-num 0))

    ;; make sure each subreport has an option change callback that 
    ;; pings the parent 
    (let ((new-reports '()))
      (for-each 
       (lambda (report-info)
	 (let ((child (car report-info))
	       (rowspan (cadr report-info))
	       (colspan (caddr report-info))
	       (callback (cadddr report-info)))
	   (if (not callback)
	       (begin 
		 (set! callback 
		       (make-child-options-callback
			report (gnc-report-find child)))
		 (set! report-info 
		       (list child rowspan colspan callback))))
	   (set! new-reports (cons report-info new-reports))))
       reports)
      (gnc:option-set-value report-opt (reverse new-reports)))
    
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
       (let* ((subreport (gnc-report-find (car report-info)))
	      (colspan (cadr report-info))
	      (rowspan (caddr report-info))
	      (opt-callback (cadddr report-info))
	      (toplevel-cell (gnc:make-html-table-cell/size rowspan colspan))
	      (report-table (gnc:make-html-table))
	      (contents-cell (gnc:make-html-table-cell)))

	 ;; set the report's style properly ... this way it will
	 ;; also get marked as dirty when the stylesheet is edited.
	 (gnc:report-set-stylesheet! 
	  subreport (gnc:report-stylesheet report))
	 
	 ;; render the report body ... hopefully this will DTRT
	 ;; if there is an error, then dump a backtrace and print
	 ;; an error message
	 ;; and cache when it's ok to cache.
	 (if (not (gnc:backtrace-if-exception
		   (lambda ()
		     (gnc:html-table-cell-append-objects! 
		      contents-cell (gnc:report-render-html subreport #f))
		     #t)))
	     (gnc:html-table-cell-append-objects!
	      contents-cell
	      (gnc:make-html-text
	       (string-append
		"<h3>" (_ "Report error") "</h3><p>"
		(_ "An error occurred while running the report.")))))

	 ;; increment the alloc number for each occupied row
	 (let loop ((row current-row-num))
	   (let ((allocation (hash-ref column-allocs row)))
	     (if (not allocation) 
		 (set! allocation 0))
	     (hash-set! column-allocs row (+ colspan allocation))
	     (if (< (+ 1 (- row current-row-num)) rowspan)
		 (loop (+ 1 row)))))
	 
	 (gnc:html-table-cell-set-style!
	  toplevel-cell "td"
	  'attribute (list "valign" "top")
	  'inheritable? #f)
	 
	 ;; put the report in the contents-cell 
	 (gnc:html-table-append-row! report-table (list contents-cell))
	 
	 ;; and a parameter editor link
	 (gnc:html-table-append-row!
	  report-table 
	  (list (gnc:make-html-text 
		 (gnc:html-markup-anchor
		  (gnc-build-url
		   URL-TYPE-OPTIONS
		   (string-append "report-id=" 
				  (sprintf #f "%a" (car report-info)))
		   "")
		  (_ "Edit Options"))
		 "&nbsp;"
		 (gnc:html-markup-anchor
		  (gnc-build-url
		   URL-TYPE-REPORT
		   (string-append "id=" 
				  (sprintf #f "%a" (car report-info)))
		   "")
		  (_ "Single Report")))))

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

(define (options-changed-cb report)
  (let* ((options (gnc:report-options report))
	 (reports
	  (gnc:option-value
	   (gnc:lookup-option options "__general" "report-list"))))
    (for-each 
     (lambda (child)
       (gnc:report-set-dirty?! (gnc-report-find (car child)) #t))
     reports)))

(define (cleanup-options report)
  (let* ((options (gnc:report-options report))
	 (report-opt (gnc:lookup-option options "__general" "report-list"))
	 (reports (gnc:option-value report-opt))
	 (new-reports '()))
    (for-each 
     (lambda (report-info)
       (let ((rep (car report-info))
	     (rowspan (cadr report-info))
	     (colspan (caddr report-info)))
	 (set! report-info 
	       (list rep rowspan colspan #f))
	 (set! new-reports (cons report-info new-reports))))
     reports)
    (gnc:option-set-value report-opt (reverse new-reports))))

;; define the view now.
(gnc:define-report 
 'version 1
 'name (N_ "Multicolumn View")
 'report-guid "d8ba4a2e89e8479ca9f6eccdeb164588"
 'menu-name (N_ "Custom Multicolumn Report")
 'menu-path (list gnc:menuname-utility)
 'renderer render-view
 'options-generator make-options
 'options-cleanup-cb cleanup-options 
 'options-changed-cb options-changed-cb)
