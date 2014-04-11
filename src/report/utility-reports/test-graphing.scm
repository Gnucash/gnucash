;; -*-scheme-*-

;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash report test-graphing))
(use-modules (gnucash main)) ;; FIXME: delete after we finish modularizing.
(use-modules (gnucash gnc-module))

(debug-enable 'debug)
(debug-enable 'backtrace)

(gnc:module-load "gnucash/report/report-system" 0)
(gnc:module-load "gnucash/gnome-utils" 0) ;for gnc-build-url

(define (simple-pie-chart)
  (let ((chart (gnc:make-html-piechart)))
    (gnc:html-piechart-set-title! chart "Pie Chart Title")
    (gnc:html-piechart-set-subtitle! chart "Pie Chart SubTitle")
    (gnc:html-piechart-set-width! chart 320)
    (gnc:html-piechart-set-height! chart 240)
    (gnc:html-piechart-set-data! chart '(25 45 30))
    (gnc:html-piechart-set-labels! chart '("foo" "bar" "baz"))
    (gnc:html-piechart-set-colors! chart (gnc:assign-colors 3))
    chart
    )
)

(define (simple-bar-chart stacked?)
  (let ((chart (gnc:make-html-barchart))
        (text (gnc:make-html-text (gnc:html-markup-p "[bar goes here]"))))
    (gnc:html-barchart-set-title! chart "Bar Chart Title")
    (gnc:html-barchart-set-subtitle! chart "Bar Chart SubTitle")
    (gnc:html-barchart-append-row! chart '(25 45 30))
    (gnc:html-barchart-append-row! chart '(75 55 70))
    (gnc:html-barchart-set-width! chart 320)
    (gnc:html-barchart-set-height! chart 240)
    (gnc:html-barchart-set-row-labels! chart '("date1" "date2"))
    (gnc:html-barchart-set-col-labels! chart '("dataset1" "dataset2" "dataset3"))
    (gnc:html-barchart-set-col-colors! chart (gnc:assign-colors 3))
    (gnc:html-barchart-set-stacked?! chart stacked?)
    (gnc:html-barchart-set-y-axis-label! chart "A Y Axis")
    chart))

(define (simple-scatter-chart)
  (let ((chart (gnc:make-html-scatter))
        (text (gnc:make-html-text (gnc:html-markup-p "[scatter goes here]"))))
    (gnc:html-scatter-set-title! chart "Scatter Title")
    (gnc:html-scatter-set-subtitle! chart "Scatter SubTitle")
    (gnc:html-scatter-add-datapoint! chart '(25 75))
    (gnc:html-scatter-add-datapoint! chart '(45 55))
    (gnc:html-scatter-add-datapoint! chart '(70 30))
    (gnc:html-scatter-set-width! chart 320)
    (gnc:html-scatter-set-height! chart 240)
    (gnc:html-scatter-set-markercolor! chart (car (gnc:assign-colors 1)))
    chart))

(define (options-generator)
  (let* ((options (gnc:new-options)))
    (gnc:options-set-default-section options "Test Graphing")
    options)
  )

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (test-graphing-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  (let ((document (gnc:make-html-document)))

    (gnc:html-document-set-title! document "Graphs")

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text
      (gnc:html-markup-p
       (gnc:html-markup/format
        "Sample graphs:"))))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p "Pie:")))
    (gnc:html-document-add-object! document (simple-pie-chart))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p "Bar, normal:")))
    (gnc:html-document-add-object! document (simple-bar-chart #f))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p "Bar, stacked:")))
    (gnc:html-document-add-object! document (simple-bar-chart #t))

    (gnc:html-document-add-object!
     document
     (gnc:make-html-text (gnc:html-markup-p "Scatter:")))
    (gnc:html-document-add-object! document (simple-scatter-chart))
    
    (gnc:html-document-add-object! 
     document 
     (gnc:make-html-text 
      (gnc:html-markup-p "Done.")))
      
    document
    )
  )

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name "Test Graphing"

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "2eff1729072e411ab124065b850dee6a"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name "Sample graphs."

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip "Sample graphs."

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-utility)

 ;; The options generator function defined above.
 'options-generator options-generator
 ;; 'options-generator gnc:new-options
 
 ;; The rendering function defined above.
 'renderer test-graphing-renderer)
