;; -*-scheme-*-
(gnc:support "report/table-test.scm")
(gnc:depend  "report-html.scm")

(let ()
  (define (options-generator)    
    (let* ((options (gnc:new-options)))
      options))

  (define (renderer report-obj)
    (let ((document (gnc:make-html-document))
          (tab (gnc:make-html-table)))
      
      (gnc:html-document-set-style! 
       document "totalcell"
       'tag "td"
       'attribute (list "bgcolor" "ff00ff"))

      (gnc:html-document-set-style! 
       document "subtotalrow"
       'tag "tr"
       'attribute (list "bgcolor" "ffff00"))

      (gnc:html-table-append-row! tab (list 0 1 2 3 4 5 6))
      (gnc:html-table-append-row/markup!
       tab "subtotalrow"
       (list 0 1 2 3 4 5 
             (gnc:make-html-table-cell/markup "totalcell" 6)))

      (gnc:html-document-add-object! document tab)
      document))
  
  (gnc:define-report
   'version 1
   'name (N_ "Table test")
   'options-generator options-generator
   'renderer renderer))
