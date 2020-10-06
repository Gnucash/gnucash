;;(use-modules (gnucash app-utils))
;; Load the C++ option implementation, avoiding the options.scm ones.
(eval-when
 (compile load eval expand)
 (load-extension "libgnc-app-utils" "scm_init_sw_app_utils_module"))
(use-modules (sw_app_utils))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-options")
  (test-lookup-option)
  (test-end "test-options"))

(define (test-lookup-option)
  (let* ((options (new-gnc-optiondb))
         (string-opt (gnc-register-string-option (GncOptionDBPtr-get options)
                                                 "Section" "Start Date"
                                                 "sort-tag" "docstring" "waldo")
                     ))

    (gnc-register-simple-boolean-option
     (GncOptionDBPtr-get options)
      "Filter" "Void Transactions" "sort-tag" "docstring" 'default-val)
    ;; Testing that the old option name aliases work.
    (let ((option (gnc-lookup-option options "Section" "From")))
      (test-assert "lookup-option changed name" option))
    (let ((option (gnc-lookup-option options "Section" "Void Transactions?")))
    (test-assert "lookup-option changed section and name" option))))
