(use-modules (gnucash gnc-module))
(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-options")
  (test-lookup-option)
  (test-end "test-options"))

(define (test-lookup-option)
  (let ((options (gnc:new-options)))
    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      "Section" "Start Date" "sort-tag" "docstring" 'default-val))

    (gnc:register-option
     options
     (gnc:make-simple-boolean-option
      "Filter" "Void Transactions" "sort-tag" "docstring" 'default-val))

    (test-assert "lookup-option changed name"
      (gnc:lookup-option options "Section" "From"))

    (test-assert "lookup-option changed section and name"
      (gnc:lookup-option options "Section" "Void Transactions?"))))
