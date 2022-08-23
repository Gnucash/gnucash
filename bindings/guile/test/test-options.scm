;;(use-modules (gnucash app-utils))
;; Load the C++ option implementation, avoiding the options.scm ones.
(eval-when
 (compile load eval expand)
 (load-extension "libgnucash-guile" "scm_init_sw_app_utils_module")
 (load-extension "libgnucash-guile" "scm_init_sw_engine_module"))
(use-modules (sw_app_utils))
(use-modules (sw_engine))
(use-modules (srfi srfi-64))
(use-modules (tests srfi64-extras))

(define (run-test)
  (test-runner-factory gnc:test-runner)
  (test-begin "test-options")
  (test-lookup-option)
  (test-end "test-options"))

(define (test-lookup-option)
  (let* ((options (gnc-new-optiondb))
         (string-opt (gnc-register-string-option options "Section" "Start Date"
                                                 "sort-tag" "docstring" "waldo")
                     ))

    (gnc-register-simple-boolean-option
     options
      "Filter" "Void Transactions" "sort-tag" "docstring" 'default-val)
    ;; Testing that the old option name aliases work.
    (let ((option (gnc-lookup-option options "Section" "From")))
      (test-assert "lookup-option changed name" option))
    (let ((option (gnc-lookup-option options "Section" "Void Transactions?")))
    (test-assert "lookup-option changed section and name" option))))
