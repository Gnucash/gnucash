(use-modules (gnucash gnc-module))

(gnc:module-begin-syntax (gnc:module-load "gnucash/app-utils" 0))

(use-modules (gnucash engine test test-extras))
(use-modules (gnucash report report-system))
(use-modules (srfi srfi-64))
(use-modules (gnucash engine test srfi64-extras))

(define (run-test)
    (set! test-report-system-flag #t)
    (test-runner-factory gnc:test-runner)
    (test-begin "Testing/Temporary/test-report-system") ;; if (test-runner-factory gnc:test-runner) is commented out, this
                                                            ;; will create Testing/Temporary/test-asset-performance.log
    (test-assert "Minimum Report Definition" (test-check1))
    (test-assert "Missing GUID detection" (test-check2))
    (test-assert "Detect double GUID" (test-check3))
    (test-assert "Report with Full Argument Set" (test-check4))
    (set! test-report-system-flag #f)
    (test-end "Testing/Temporary/test-report-system")
)

;; -----------------------------------------------------------------------

(define (test-check1)
  (gnc:define-report 'version "1" 'name "Test Report Template" 'report-guid "54c2fc051af64a08ba2334c2e9179e23")
)

;; -----------------------------------------------------------------------

(define (test-check2)
  (not (gnc:define-report 'version "1" 'name "Test Report Template"))
)

;; -----------------------------------------------------------------------

(define (test-check3)
  (if (not (gnc:define-report 'version "1" 'name "Test Report Template" 'report-guid "54c2fc051af64a08ba2334c2e9179e23" 'parent-type "Parent Type" 'options-generator "Options Generator" 'renderer "Renderer" 'options-cleanup-cb "Options Clean-Up" 'options-changed-cb "Options Changed" 'in-menu? #f 'menu-path "Menu Path" 'menu-name "Menu Name" 'menu-tip "Menu Tip" 'export-types "Export Types" 'export-thunk "Export Thunk"))
    #t
    #f
  )
)

;; -----------------------------------------------------------------------

(define (test-check4)
  (and
    (gnc:define-report 'version "1"
                       'name "Test Report Template"
                       'report-guid "54c2fc051af64a08ba2334c2e9179e24"
                       'parent-type "Parent Type"
                       'options-generator "Options Generator"
                       'renderer "Renderer"
                       'options-cleanup-cb "Options Clean-Up"
                       'options-changed-cb "Options Changed"
                       'in-menu? #f
                       'menu-path "Menu Path"
                       'menu-name "Menu Name"
                       'menu-tip "Menu Tip"
                       'export-types "Export Types"
                       'export-thunk "Export Thunk"
    )
    (string=? (gnc:report-template-version (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "1")
    (string=? (gnc:report-template-name (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Test Report Template")
    (string=? (gnc:report-template-report-guid
                (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "54c2fc051af64a08ba2334c2e9179e24")
    ;; parent type is not exported -> it is used in gnc:make-report
    (string=? (gnc:report-template-options-generator (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Options Generator")
    (string=? (gnc:report-template-renderer (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Renderer")
    (string=? (gnc:report-template-options-cleanup-cb (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Options Clean-Up")
    (string=? (gnc:report-template-options-changed-cb (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Options Changed")
    (not (gnc:report-template-in-menu? (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")))
    (string=? (gnc:report-template-menu-path (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Menu Path")
    (string=? (gnc:report-template-menu-name (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Menu Name")
    (string=? (gnc:report-template-menu-tip (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Menu Tip")
    (string=? (gnc:report-template-export-types (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Export Types")
    (string=? (gnc:report-template-export-thunk (gnc:find-report-template "54c2fc051af64a08ba2334c2e9179e24")) "Export Thunk")
  )
)
