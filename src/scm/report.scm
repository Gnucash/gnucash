(require 'hash-table)

(gnc:support "report.scm")

;; We use a hash to store the report info so that whenever a report is
;; requested, we'll look up the action to take dynamically.  That
;; makes it easier for us to allow changing the report definitions on
;; the fly later, and it should have no appreciable performance
;; effect.

(define *gnc:_report-info_* (make-hash-table 23))
;; This hash should contain all the reports available and will be used
;; to generate the reports menu whenever a new window opens and to
;; figure out what to do when a report needs to be generated.
;;
;; The key is the string naming the report and the value is the
;; rendering thunk.

(define (gnc:run-report report-name options)
  ;; Return a string consisting of the contents of the report.

  (define (display-report-list-item item port)
    (cond
     ((string? item) (display item port))
     ((null? item) #t)
     ((list? item) (map (lambda (item) (display-report-list-item item port))
                        item))
     (else (gnc:warn "gnc:run-report - " item " is the wrong type."))))

  (define (report-output->string lines)
    (call-with-output-string
     (lambda (port)
       (for-each
        (lambda (item) (display-report-list-item item port))
        lines))))

  (define (call-report rendering-thunk options)
    (let ((lines (rendering-thunk options)))
      (report-output->string lines)))

  (let ((report (hash-ref *gnc:_report-info_* report-name)))
    (if (not report)
        #f
        (let ((rendering-thunk (gnc:report-rendering-thunk report)))
          (call-report rendering-thunk options)))))

(define (gnc:report-menu-setup win)

  (define menu (gnc:make-menu "_Reports" (list "_Settings")))
  (define menu-namer (gnc:new-menu-namer))

  (gnc:add-extension menu)

  (hash-for-each
   (lambda (name report)
     (define item
       (gnc:make-menu-item
        ((menu-namer 'add-name) name)
        (string-append "Display the " name " report.")
        (list "_Reports" "")
        (lambda ()
          (let ((options (false-if-exception (gnc:report-new-options report))))
            (gnc:report-window (string-append "Report: " name)
                               (lambda () (gnc:run-report name options))
                               options)))))
     (gnc:add-extension item))
   *gnc:_report-info_*))

(define (gnc:define-report version name option-generator rendering-thunk)
  ;; For now the version is ignored, but in the future it'll let us
  ;; change behaviors without breaking older reports.
  ;;
  ;; FIXME: If we wanted to be uber-dynamic we might want to consider
  ;; re-generating the menus whenever this function is called.
  
  ;; The rendering-thunk should be a function that generates the report
  ;;
  ;; This code must return as its final value a collection of strings in
  ;; the form of a list of elements where each element (recursively) is
  ;; either a string, or a list containing nothing more than strings and
  ;; lists of strings.  Any null lists will be ignored.  The final html
  ;; output will be produced by an in-order traversal of the tree
  ;; represented by the list.  i.e. ("a" (("b" "c") "d") "e") produces
  ;; "abcde" in the output.
  ;;
  ;; For those who speak BNF-ish the output should look like
  ;;
  ;; report -> string-list
  ;; string-list -> ( items ) | ()
  ;; items -> item items | item
  ;; item -> string | string-list
  ;; 
  ;; Valid examples:
  ;;
  ;; ("<html>" "</html>")
  ;; ("<html>" " some text " "</html>")
  ;; ("<html>" ("some" ("other" " text")) "</html>")
  (let ((report (vector version name option-generator rendering-thunk)))
    (hash-set! *gnc:_report-info_* name report)))

(define (gnc:report-version report)
  (vector-ref report 0))
(define (gnc:report-name report)
  (vector-ref report 1))
(define (gnc:report-options-generator report)
  (vector-ref report 2))
(define (gnc:report-rendering-thunk report)
  (vector-ref report 3))

(define (gnc:report-new-options report)
  (let ((generator (gnc:report-options-generator report)))
    (if (procedure? generator)
        (generator)
        #f)))

(gnc:hook-add-dangler gnc:*main-window-opened-hook* gnc:report-menu-setup)
